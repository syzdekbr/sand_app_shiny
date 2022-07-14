
## SAND Data to construct probability tables
  sand_all <- readxl::read_xlsx("data/final_sand_data_manual_final_final.xlsx", sheet = 1) %>% 
    mutate(across(everything(), na_if, "NA"),
           sensory_difference_probable_w_na = as.numeric(factor(sensory_difference_probable_w_na)) - 1)


# Function for probability prediction of glm and generate predictions for ASD- Use to generate one prediction
  probability_pred_glm <- function(x, model_name){
    1/(1+exp(-(coef(model_name)[1] + coef(model_name)[2]*x)))
  }
  logit_func = function(x){
    exp(x)/(1+exp(x))
  }

  prediction_function <- function(df, score, model){
    df %>%
      dplyr::summarize(
        inputs = min(!!sym(score)):max(!!sym(score)),
        predicted_fit = sapply(inputs, function(x) predict(model, setNames(data.frame(x), quo_name(score)), type='link', se.fit=TRUE)$fit),
        probabilities = round(logit_func(predicted_fit),4),
        predicted_fit_se = sapply(inputs, function(x) predict(model, setNames(data.frame(x), quo_name(score)), type='link', se.fit=TRUE)$se.fit),
        se_low = round(logit_func(predicted_fit - (predicted_fit_se*1.96)),4),
        se_high = round(logit_func(predicted_fit + (predicted_fit_se*1.96)), 4)
      ) %>%
      dplyr::filter(se_low > .01 & se_low <= .995) %>%
      dplyr::select(inputs, probabilities, se_low, se_high)
  }


  glm_sensory_difference_probable_total <- glm(sensory_difference_probable_w_na ~ Total, 
                                               data = sand_all, family = binomial)

  accuracy_func <- function(df, scale, score){
    df %>%
      mutate(sensory_difference_probable_w_na = case_when(
        sensory_difference_probable_w_na == 1 ~ "Sensory Difference",
        sensory_difference_probable_w_na == 0 ~ "None",
        TRUE ~ NA_character_
      )) %>% 
      filter(!is.na(sensory_difference_probable_w_na)) %>%
      mutate(critical = ifelse(!!sym(scale) >= score, "Yes", "No")) %>%
      dplyr::summarize(tp = sum(critical == "Yes" & sensory_difference_probable_w_na == "Sensory Difference", na.rm = T),
                       tt = sum(sensory_difference_probable_w_na == "Sensory Difference", na.rm = T),
                       sensitivity = tp/tt,
                       tn = sum(critical == "No" & sensory_difference_probable_w_na == "None", na.rm = T),
                       tf = sum(sensory_difference_probable_w_na == "None", na.rm = T),
                       specificity = tn/tf,
                       accuracy = (tp + tn)/ (tt + tf))
  }

  apply_accuracy_func <- function(df, scale_name){
    df %>%
      dplyr::summarize(max_score = max(!!sym(scale_name))) %>%
      pull(max_score) -> max_score
    lapply(1:max_score, function(score) accuracy_func(df, quo_name(scale_name), score)) %>% 
      bind_rows() %>%
      cbind(Score = 1:max_score, .) %>%
      dplyr::select(Score, "Sensitivity" = sensitivity, "Specificity" = specificity, 
                    "Total Accuracy" = accuracy) 
  } 


accuracy_table <- apply_accuracy_func(sand_all, "Total") %>% 
  {inner_join(prediction_function(sand_all, "Total", glm_sensory_difference_probable_total),
              ., by = c("inputs" = "Score"))} %>%
  dplyr::filter(inputs >= 1) %>% 
  # dplyr::filter(Sensitivity < .999 & Specificity < .999) %>%
  mutate(across(-1, ~ round(., 4)*100)) %>% 
  dplyr::rename(
    "Score" = inputs,
    "Probabilities" = probabilities,
    "CI Low" = se_low,
    "CI High" = se_high
  )

  scale_names <- list(sand_all %>% dplyr::select(Hyperreactivity:Seeking) %>% colnames, sand_all %>% dplyr::select(TotalVisual:TotalAuditory) %>% colnames, sand_all %>% dplyr::select(V_Hyper:A_Seek) %>% colnames)
  max_scale_all_func <- function(i){
    sand_all %>% 
      filter(!is.na(sensory_difference_probable_w_na)) %>%
      mutate(max_score = dplyr::select(., !!!syms(scale_names[[i]])) %>% apply(., 1, max),
             max_score_name = dplyr::select(., !!!syms(scale_names[[i]])) %>% apply(., 1, function(x) x[which(x == max(x))] %>% names())) %>%
      cbind(., data.frame(stringi::stri_list2matrix(.$max_score_name, byrow = TRUE)) %>% setNames(paste0("max_score_name_", seq(1, length(scale_names[[i]]))))
      )  %>% 
      dplyr::select(ID, sensory_difference_probable_w_na, max_score, paste0("max_score_name_", seq(1, length(scale_names[[i]])))) %>%
      pivot_longer(-c(ID, sensory_difference_probable_w_na, max_score), values_to = "max_score_name") %>%
      filter(!is.na(max_score_name)) 
  }

subscale_probability_list <- lapply(1:length(scale_names), function(i) {
    glm_scale_max <- glm(sensory_difference_probable_w_na ~ max_score, data = max_scale_all_func(i), family = binomial)
    prediction_function(max_scale_all_func(i), "max_score", glm_scale_max) %>%
      mutate(across(-1, ~ round(., 4)*100)) %>% 
      dplyr::rename(
        "Score" = inputs,
        "Probabilities" = probabilities,
        "CI Low" = se_low,
        "CI High" = se_high
      )
  })

###*** Generate z score tables
###*
weight_sample_func <- function(scale_names){
  sand_all %>%
    dplyr::select("NDD" = NDD, !!!syms(scale_names)) %>% 
    mutate(max_col_value = apply(.[-1], 1, max)) %>%
    group_by(NDD) %>%
    dplyr::summarize(tot = mean(max_col_value),
                     stand = sd(max_col_value)) %>%
    ungroup() %>%
    dplyr::summarize(weighted_mean = wtd.mean(tot, c(53/54, 1/54)),
                     weighted_sd = wtd.mean(stand, c(53/54, 1/54)))
}

scale_names <- list(sand_all %>% dplyr::select(Hyperreactivity:Seeking) %>% colnames, sand_all %>% dplyr::select(TotalVisual:TotalAuditory) %>% colnames, sand_all %>% dplyr::select(V_Hyper:A_Seek) %>% colnames)
scale_range <- list(0:30, 0:30, 0:10)
scale_type <- c("Domain", "Modality", "Subscale")
scale_names_all <- c(list("Total") %>% setNames("Total"), scale_names)
scale_type_all <- c("Total", scale_type)

convert_z_score <- function(raw_score, scale_mean, scale_sd){
  (raw_score - scale_mean) / scale_sd 
}

z_score_table_func <- function(scale_names, scale_range){
  z_score_vec <- as.numeric(mapply(convert_z_score, scale_range, weight_sample_func(scale_names)[1], weight_sample_func(scale_names)[2]))
  z_percentiles <- sapply(z_score_vec, pnorm)
  # print(
  cbind.data.frame("Score" = scale_range, "Z-Score" = round(z_score_vec,2), "Percentiles" = round(z_percentiles*100,2)) %>%
    filter(Percentiles < 100) 
}

percentile_lists <- c(
  list(z_score_table_func("Total", 0:90), z_score_table_func("Total", 0:90)),
  lapply(1:length(scale_names), function(i) z_score_table_func(scale_names[[i]], scale_range[[i]]))
) 