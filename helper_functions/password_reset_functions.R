### ***** Password management functions *****
  # These functions work to manage passwords for shiny apps. User data should be stored in google sheet, accessed via
  # googlesheets4 methods, used in app, recommend following fields
  # user_name (should be unique for each user, like email), user_password, retrieve_password_question, retrieve_password_answer
  # Uses modals- state modal function here, with modalDialog; in app, call showModal upon event, like button click
## Does following- 1. Forgot password retrieval, 2. Reset password, 3. Reset retrieval info

# General functions used to retrieve values in database based on user entered info
{
  # General function used in each function to get a value in a specified column corresponding to value in specified column
  get_column_associated_with_user_name_func <- function(googlesheet_url, user_name_input, user_name_column, selected_column, ...){
    google_df <- read_sheet(googlesheet_url)
    google_df %>% 
      dplyr::filter(!!sym(user_name_column) == user_name_input) %>%
      dplyr::select(!!sym(selected_column)) %>%
      pull()
  }
  ## This takes above function and allows for a different specified column from which value comes- "variable_name".
    ## This is useful because passing these functions within functions, we can't change the "selected_column" in call
  password_general_func <- function(googlesheet_url, user_name_input, user_name_column, selected_column, variable_name){
    get_column_associated_with_user_name_func(googlesheet_url, user_name_input, user_name_column, selected_column = variable_name)
  }
}

# Forgot password? Enter user_name. If matches, output retrieve question, accept answer
{
  ## Ask for user name input
  forgot_password_modal_func <- function(failed = FALSE) {
    modalDialog(
      textInput("forgot_password_user_name_input", 
                "Enter user name:",
                placeholder = 'Enter user name here'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("forgot_password_user_name_submit", "Submit Response")
      )
    )
  }
  
  ## Check if user name valid, output error if not, output modal with password retrieve question and field for answer
  forgot_password_retrieve_question_func <- function(failed = FALSE, googlesheet_url, user_name_input, user_name_column, selected_column) {
    google_df <- read_sheet(googlesheet_url)
    # Feedback username doesn't exist
    if(google_df %>% select(!!sym(user_name_column)) %>% filter(. == user_name_input) %>% nrow() != 1){
      modalDialog(
        h4("User name doesn't exist or invalid. Retry password retrieval")
      )
    } else {
      # User name exists prompts retrieve question and response
      modalDialog(
        h4("Retrieve Password Question"),
        textInput("forgot_password_question_input", 
                  get_column_associated_with_user_name_func(googlesheet_url, user_name_input, user_name_column, selected_column),
                  placeholder = 'Enter question answer here'
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("forgot_password_question_submit", "Submit Response")
        )
      )
    }
  }
  
  ## If retrieve password answer correct, display password, if not display incorrect message
  display_password_func <- function(failed = FALSE, googlesheet_url, user_name_input, user_name_column, selected_column, variable_name, forgot_password_question_input){
    google_df <- read_sheet(googlesheet_url)
    ### tolower makes case insensitive
    if(tolower(password_general_func(googlesheet_url, user_name_input, user_name_column, selected_column, variable_name)) == tolower(forgot_password_question_input)){
      modalDialog(
        h4(paste("Password is:"), get_column_associated_with_user_name_func(googlesheet_url, user_name_input, user_name_column, selected_column))
      )
    } else{
      modalDialog(
        h4("Incorrect Retrieve Password Question Response")
      )
    }
  }
}

# Reset Password- enter current user name and password, update with new password info
{
  ## Input current user_name and password
  reset_password_func <- function(failed = FALSE){
    modalDialog(
      textInput("reset_password_user_name_input", 
                "Enter user name:",
                placeholder = 'Enter user name here'
      ),
      textInput("reset_password_password_input", 
                "Enter current password:",
                placeholder = 'Enter password here'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("reset_password_submit", "Submit Response")
      )
    )
  }
  
  ## Check password, user_name combo
  reset_password_feedback_func <- function(failed = FALSE, googlesheet_url, user_name_input, user_name_column, selected_column, existing_password_input){
    google_df <- read_sheet(googlesheet_url)
    ### If user_name doesn't exist, gives error message
    if(google_df %>% select(!!sym(user_name_column)) %>% filter(. == user_name_input) %>% nrow() != 1){
      modalDialog(
        h4("User name doesn't exist or invalid. Retry password reset")
      )
    } else if (get_column_associated_with_user_name_func(googlesheet_url, user_name_input, user_name_column, selected_column) == existing_password_input){
      # User name exists prompts retrieve question and response
      modalDialog(
        h4("Reset Password"),
        textInput("reset_password_new_input", 
                  "Enter new password",
                  placeholder = 'Enter new password here'
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("reset_password_commit", "Submit Response")
        )
      )
    } else {
      ### Combination of user_name and password incorrect
      modalDialog(
        h4("Incorrect User Name and Password Combination")
      )
    }
  }
  
  ## Rewrite password in googlesheet- url stored securely
  password_reset_commit_func <- function(new_password_input, user_name_input, googlesheet_url){
    google_df <- read_sheet(googlesheet_url)
    ### New password entered as df
    edit_df <- tibble(
      new_dat = new_password_input
    )
    # column with password is "D"
    range_write(
      ss = googlesheet_url, 
      data = edit_df, 
      range = paste0("D", which(google_df$customer_email == user_name_input) +1), 
      col_names = FALSE) # don't want df columns input
  }
  ## Confirmation of password change
  password_updated_func <- function(failed = FALSE){
    modalDialog(
      h4("Password changed. Reset app to login.")
    )
  }
  ## Invalid password-nothing entered- feedback
  unacceptable_information_func <- function(failed = FALSE){
    modalDialog(
      h4("Unacceptable input. All fields must be complete.")
    )
  }
}

# Reset Password Retrieve Question and Answer
{
  ## Prompt for user name and current password
  reset_password_retrieve_func <- function(failed = FALSE){
    modalDialog(
      textInput("reset_password_retrieve_user_name_input", 
                "Enter user name:",
                placeholder = 'Enter user name here'
      ),
      textInput("reset_password_retrieve_password_input", 
                "Enter current password:",
                placeholder = 'Enter password here'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("reset_password_retrieve_submit", "Submit Response")
      )
    )
  }
  
  ## CHeck if user_name and password combo valid
  reset_password_retrieve_feedback_func <- function(failed = FALSE, googlesheet_url, user_name_input, user_name_column, selected_column, existing_password_input){
    google_df <- read_sheet(googlesheet_url)
    ## No user name error msg.
    if(google_df %>% select(!!sym(user_name_column)) %>% filter(. == user_name_input) %>% nrow() != 1){
      modalDialog(
        h4("User name doesn't exist or invalid. Retry password retrieve question reset")
      )
    } else if (get_column_associated_with_user_name_func(googlesheet_url, user_name_input, user_name_column, selected_column) == existing_password_input){
      # User name exists prompts retrieve question and response
      modalDialog(
        h4("Reset Password Retrieve Question"),
        textInput("reset_password_retrieve_question_new_input", 
                  "Enter new password retrieve question:",
                  placeholder = 'Enter new question here'
        ),
        textInput("reset_password_retrieve_answer_new_input", 
                  "Enter new password retrieve answer:",
                  placeholder = 'Enter answer to above question here'
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("reset_password_retrieve_commit", "Submit Response")
        )
      )
    } else {
      modalDialog(
        h4("Incorrect User Name and Password Combination")
      )
    }
  }
  
  ## Check new password retrieve question and answer that neither are blank
  password_retrieve_reset_commit_func <- function(new_password_retrieve_question_input, new_password_retrieve_answer_input,
                                         user_name_input, googlesheet_url){
    google_df <- read_sheet(googlesheet_url)
    edit_df <- tibble(
      new_question_dat = new_password_retrieve_question_input,
      new_answer_dat = new_password_retrieve_answer_input
    )
    range_write(
      ss = googlesheet_url, 
      data = edit_df, 
      range = paste0("E", which(google_df$customer_email == user_name_input) +1), 
      col_names = FALSE)
  }
  
  ## Feeback on if valid and changed
  password_retrieve_updated_func <- function(failed = FALSE){
    modalDialog(
      h4("Password retrieve question and answer changed.")
    )
  }
}

#### assign googlesheet url to googlesheet_url in global----- 
# googlesheet_url <- "sheet_url_goes_here"
##### ------------ BELOW GOES IN UI uncommented ------------
# Password Reset Help -----------------------------------------------------
# source("password_reset_functions.R")
# # Forgot password retrieve question
# {
#   ## Enter user name to get retrieve question
#   observeEvent(input$forgot_password, {
#     showModal(forgot_password_modal_func())
#   })
#   ## Show retrieve question with answer field
#   observeEvent(input$forgot_password_user_name_submit,{
#     showModal(
#       forgot_password_retrieve_question_func(
#         google_df = google_df,
#         user_name_input = input$forgot_password_user_name_input,
#         user_name_column = "customer_email", 
#         selected_column = "customer_retrieve_password_question"
#       )
#     )
#   })
#   ## Displays password if correct retrieval answer input
#   observeEvent(input$forgot_password_question_submit,{
#     showModal(
#       display_password_func(
#         google_df = google_df,
#         user_name_input = input$forgot_password_user_name_input,
#         user_name_column = "customer_email",
#         selected_column = "customer_password",
#         variable_name = "customer_retrieve_password_answer",
#         forgot_password_question_input = input$forgot_password_question_input
#       )
#     )
#   })
# }
# # Reset Password Modals
# {
#   # Prompt for current user name and password
#   observeEvent(input$reset_password, {
#     showModal(reset_password_func())
#   })
#   # Display new password entry
#   observeEvent(input$reset_password_submit, {
#     showModal(
#       reset_password_feedback_func(
#         google_df = google_df,
#         user_name_input = input$reset_password_user_name_input,
#         user_name_column = "customer_email",
#         selected_column = "customer_password",
#         existing_password_input = input$reset_password_password_input
#       )
#     )
#   })
#   # New password feedback and update
#   observeEvent(input$reset_password_commit, {
#     if (trimws(input$reset_password_new_input) != ""){
#       password_reset_commit_func(
#         new_password_input = input$reset_password_new_input,
#         user_name_input = input$reset_password_user_name_input,
#         googlesheet_url = googlesheet_url
#       )
#       showModal(
#         password_updated_func()
#       )
#     } else{
#       showModal(
#         unacceptable_information_func()
#       )
#     }
#   })
# }
# # Reset Password Retrieval Question
# {
#   # Prompts for current user name and password
#   observeEvent(input$reset_password_retrieve,{
#     showModal(
#       reset_password_retrieve_func()
#     )
#   })
#   # Prompt for new retrieve question and answer
#   observeEvent(input$reset_password_retrieve_submit,{
#     showModal(
#       reset_password_retrieve_feedback_func(
#         google_df = google_df,
#         user_name_input = input$reset_password_retrieve_user_name_input,
#         user_name_column = "customer_email",
#         selected_column = "customer_password",
#         existing_password_input = input$reset_password_retrieve_password_input
#       )
#     )
#   })
#   # Feedback on new question and answer and commit
#   observeEvent(input$reset_password_retrieve_commit,{
#     if (trimws(input$reset_password_retrieve_question_new_input) != "" & 
#         trimws(input$reset_password_retrieve_answer_new_input) != ""){
#       password_retrieve_reset_commit_func(
#         new_password_retrieve_question_input = input$reset_password_retrieve_question_new_input,
#         new_password_retrieve_answer_input = input$reset_password_retrieve_answer_new_input,
#         user_name_input = input$reset_password_retrieve_user_name_input,
#         googlesheet_url = googlesheet_url
#       )
#       showModal(
#         password_retrieve_updated_func()
#       )
#     } else{
#       showModal(
#         unacceptable_information_func()
#       )
#     }
#   })
# }