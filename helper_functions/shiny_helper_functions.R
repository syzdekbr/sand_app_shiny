#### SHINY CODE FUNCTIONS ####

#**** BUILD UI FUNCTION FROM FILE ****#
## Data should be organized in a file like:
#data <- tibble(
#    type = c("textInput", "actionButton"),
#    id = c("text1", "button1"),
#    label = c("Some Text", "Some Action"),
#    value = c(NA, NA),
#    min = c(NA, NA),
#    max = c(NA, NA),
#    choices = c(NA, NA) ## choices should be a comma separated string
#)

ui_func <- function(ui_type, ...){
  eval(parse(text = ui_type))(...)
} 

remove_na_func <- function(df){
  df %>% 
    dplyr::select(which(!is.na(.))) %>%
    pmap(., ui_func) 
}


## In server, call
# ui_builder(name_of_data)

## Then in ui, insert

#        mainPanel( # or wherever it goes
#            lapply(1:nrow(name_of_data), function(i) {
#                uiOutput(name_of_data$id[i])
#            })        
#        )


### Rmarkdown styling functions
{
  ## Adds color to any text, either in html or other output format
  colorize <- function(text_to_color, color) {
    if (knitr::is_latex_output()) {
      sprintf("\\textcolor{%s}{%s}", color, text_to_color)
    } else if (knitr::is_html_output()) {
      sprintf("<span style='color: %s;'>%s</span>", color, 
              text_to_color)
    } else text_to_color
  }
  
  ## Use to print tables in any format
  
  # Preferred Kable styling table options
  kable_options <- function(., ...) {kable_styling(.,bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                                              ...)} #latex_options = c("striped", "scale_down", "repeat_header"))}
  
  ## Gets the type of output format, which is then used to set options
  getOutputFormat <- function() {
    output <- rmarkdown:::parse_yaml_front_matter(
      readLines(knitr::current_input())
    )$output
    if (is.list(output)){
      return(names(output)[1])
    } else {
      return(output[1])
    }
  }
  
  ## Will output table in format that best fits report style
  table_output_func <- function(df, colnames, caption = NULL, output_format = "html", size = 10, ...){
    # with html- typically use table_output_func(df, colnames = c("col1", "col2"), caption = table_counter_func("caption")), df req
    if (output_format == "html"){
      size = NULL
      kable(df, row.names = F, col.names = colnames, caption = caption) %>%
        kable_options()
    } else {
      # with pdf or word use table_output_func(df, colnames = c("col1", "col2"), values = table_counter_func("caption")), df req
      names1 = names(df)
      names2 = if(missing(colnames)){
        names1
      } else {
        colnames
      }
      if (missing(caption)){
        caption <- NULL
      }
      FitFlextableToPage <- function(ft, pgwidth = 6){
        
        ft_out <- ft %>% autofit()
        
        ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
        return(ft_out)
      }
      df %>% 
      flextable() %>% 
      set_header_df(x = ., mapping = data.frame(keys = names1, values = names2, stringsAsFactors = FALSE),
                          key = "keys" ) %>%
        add_header_lines(top = TRUE, values = caption) %>% 
      theme_zebra() %>% FitFlextableToPage() %>% fontsize(part = "header", size = size) 
    }
  }
  
  ## Use this table_print call in code because it will run in console- good for development- can also pair with
    ## table_counter_func in general scripts as caption to count figures programatically
  table_print <- function(df, colnames = "", caption = "", output_format, size, ...){
    colnames <- case_when(
      colnames == "" ~ pretty_columns_func(colnames(df)),
      TRUE ~ colnames
    )
    if (isTRUE(getOption('knitr.in.progress'))) {
      table_output_func(df, colnames, caption, output_format, size, ...)
    } else {
      df %>%
        kable(caption = caption, col.names = colnames) %>%
        kable_options(., ...)
     }
  }
}

# Prettify column names
  pretty_columns_func <- function(colnames){
    tools::toTitleCase(gsub("[_|.]", " ", colnames))
  }

## Use this to wait until inputs load before executing. Wrap expression in function name- not used here
  execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
    observeEvent(once = TRUE, reactiveValuesToList(session$input), {
      print(reactiveValuesToList(session$input))
      force(expr)
    }, ignoreInit = TRUE)
  }
  
###*** Print headers at desired level within loop
  ###*** THis will print section headers. Reuse and replace level and title. Will not abstract to function
  # cat("  \n###", params$output_name[-1][[i]], "  \n")
