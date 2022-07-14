# Shiny server info -------------------------------------------------------

# Application is at: http://ec2-18-225-7-170.us-east-2.compute.amazonaws.com:3838/sample-apps/SAND_app/
# Commands to save to rstudio server
# cd Downloads
# chmod 400 rstudio-server-shiny.pem
# ssh -i "rstudio-server-shiny.pem" ubuntu@ec2-18-225-7-170.us-east-2.compute.amazonaws.com
# sudo cp -r /home/rstudio/ShinyApps/sample-apps/SAND_app/SAND_app/ /srv/shiny-server/sample-apps/

## To do and/or consider-
  # * Abstract the gather inputs process to handle if there are multiple tabs
  # * Save and restore cut-score selection

# Libraries and options ---------------------------------------------------

library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinyauthr)
library(DT)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(rtf)
library(Hmisc)
library(gtable)
library(stringr)
library(png)
library(shinysky)
library(readxl)
library(shinyjs)
library(readxl)
library(knitr)
library(kableExtra)
library(anytime)
library(shinyBS)
library(shinyWidgets)
library(jpeg)
library(eeptools)
library(purrr)
library(toOrdinal)
library(googlesheets4)
library(googledrive)
library(rhandsontable)

# Scipen prints no scientific notation
options(stringsAsFactors = F, scipen = 999)

# Sources helper functions- 1. ui_builder- will iteratively read spreadsheet and build ui inputs 
# 2. Rmarkdown style functions for tables
source("helper_functions/shiny_helper_functions.R")

# Sources data to construct logistic models to predict probability of sensory reactivity
source("helper_functions/probability_tables.R")

# Add warning before closing app window
jscode <-
  "Shiny.addCustomMessageHandler('mymessage', function(message) { window.location = message;});"

## Get googlesheets customer data
{
  ####**** Hide following sources of database information for example app
  googlesheet_url <- ### Googlesheet url ####
  drive_auth(path = "./keys/shiny-2-service_account.json") # go to this sheet and authorize for use
  gs4_auth(token = drive_token())
  # Stop hide
  google_df <- read_sheet(googlesheet_url) 
  # Login dataframe from google sheet
  user_base <- google_df %>% 
    mutate(
      user = customer_email,
      password = customer_password,
      permissions = rep("standard", nrow(.)),
      name = paste(customer_first_name, customer_last_name)
    )
}

# Read data ---------------------------------------------------------------

## UI Demographics input fields- to sidebar
ui_demographics <- readxl::read_xlsx("data/sand_app_ui_inputs_demographics.xlsx")

###*** SAND items
{
  # SAND Observation questions, get only questions, id, and label each question as observation    
  sand_observed <- readxl::read_xlsx("data/SAND_observation_formatted.xlsx") %>% 
    # Directions between sections
    filter(item_type != "directions") %>% 
    mutate(questionnaire = rep("observation", nrow(.))
    )
  
  # Interview questions, label each question as interview
  sand_reported <- readxl::read_xlsx("data/sand_reported_formatted.xlsx") %>% 
    mutate(questionnaire = rep("interview", nrow(.))
    )
  
  # Combine both interview and observed into master df
  sand <- bind_rows(sand_observed, sand_reported) %>% 
    # Hierarchy 1 will display on load, 2 are reactive. "S" is severity items
    mutate(hierarchy = ifelse(grepl("S", item_number), 2, 1),
           # Create unique id for each item as question number and source combination- unique
           id = paste0(stringr::str_sub(item_number, 1,4), 
                       stringr::str_sub(questionnaire, 1, 1)) # Get initial of source
    ) 
}

###*** Directions, notes, section heads data set-up
{
###*** Get the directions and the ids to insert them after
  # Get ids of first item in each section (question ends with 1) to insert directions before for interview items
  ids_after_directions_reported <- sand %>% 
    filter(grepl("\\.1", item_number) & questionnaire == "interview") %>% 
    dplyr::select(id)
  
  ids_after_directions_observed<- sand %>% 
    filter(grepl("\\.1", item_number) & questionnaire == "observation") %>% 
    dplyr::select(id)
  
  # Directions to insert after those ids
  directions <- sand %>% 
    filter(grepl("directions", item_number) & questionnaire == "interview") %>% 
    dplyr::select(item)
  
  # Full data before following filter
  sand_all_items <- sand %>% 
    # Get ids to join with notes
    mutate(
      id_join = ifelse(questionnaire == "interview", 
                       paste0(item_number,"R"), 
                       paste0(item_number,"O")
      )
    )
  
  # SAND items only without directions
  sand <- sand %>% 
    dplyr::filter(item_type != "directions") 
  
  # Notes are instructions for coding to examiner
  ids_after_notes <- readxl::read_xlsx("data/directions.xlsx") %>% 
    dplyr::inner_join(., sand_all_items, by = c("id_after" = "id_join")) %>%
    dplyr::select("note", "id")
}

###*** SAND Research data to generate probabilities and outcomes
{
  # Read in data, noting sheet number
  # Finds the maximum behavior name and value for each observation
  max_col_name <- colnames(cbind.data.frame(sand_all$Hyperreactivity, sand_all$Hyporeactivity, sand_all$Seeking))[max.col(cbind(sand_all$Hyperreactivity, sand_all$Hyporeactivity, sand_all$Seeking),ties.method="random")]
  max_col_value <- apply(cbind.data.frame(sand_all$Hyperreactivity, sand_all$Hyporeactivity, sand_all$Seeking), 1, max)
  sand_all$max_col_value <- max_col_value
  sand_all$max_col_name <- max_col_name

# Gets cutoff scores where youden index is maximized and difference in sensitivity and specificity < .10 for balance
  name_scales <- c("Domain", "Modality", "Subscales")
  modalities <- c("Visual", "Tactile", "Auditory")
  domains <- c("Hyperreactivity", "Hyporeactivity", "Seeking")
  subscale_names_df <- cross_df(.l = purrr::set_names(list(domains, modalities), c("Domain", "Modality"))) %>% 
    mutate(Subscale = paste(Modality, Domain))
  
  critical_clinical_scores<- map_int(1:length(name_scales), function(i) {
    apply_accuracy_func(max_scale_all_func(i), "max_score") %>% 
      filter(abs(Sensitivity - Specificity) < .10) %>% 
      mutate(youden = Sensitivity + Specificity) %>% 
      slice_max(youden) %>% 
      dplyr::select(Score) %>% 
      pull
  })
}

## Read Data for Interpretation Text

sand_scale_explanations <- readxl::read_xlsx("data/sand_scale_explanations.xlsx")

# UI ----------------------------------------------------------------------

ui <- function(request) {
  # Initial call of shinybrowser
  dashboardPage(
  ## Header and dropdown help menu
    {
      dashboardHeader(
        title = "SAND Scoring App",
      # Adds logout button
        tags$li(
          class = "dropdown",
          uiOutput("logout_button_ui")
        ),
      # Dropdown menu of help/support options 1. Help doc, 2. Help video, 3. Webpage, 4. Email
        dropdownMenu(
          type = "tasks",
          headerText = "Support",
          icon = icon("question"),
          messageItem(
            from = "Help Document",
            message = "Instructions for using app",
            icon = icon("book"),
            time = NULL,
            href = "https://rpubs.com/syzdekbr/stroop_app_help_doc"
          ),
          messageItem(
            from = "Help Video",
            message = "Instructions for using app",
            icon = icon("video"),
            time = NULL,
            href = "https://www.youtube.com/watch?v=rRTPFmkR9XY&list=PLXpyCQ75n4Iy8DO9CX_Sq4l1pcJVN6B2o"
          ),
          messageItem(
            from = "SAND Webpage",
            message = "stoeltingco.com",
            icon = icon("question"),
            time = NULL,
            href = "https://stoeltingco.com/SAND"
          ),
          messageItem(
            from = "Email",
            message = "Email for support",
            icon = icon("envelope"),
            time = NULL,
            href = "mailto:psychtests@stoeltingco.com"
          )
        )
      )
    }, 
    
  ## Sidebar Content
    {
      # setup a sidebar menu to be rendered server-side
      dashboardSidebar(collapsed = TRUE,
                       sidebarMenuOutput("sidebar"),
                       width = 400) # Wider b/c double column
    },
    
  ## Body Content- to be rendered server-side
    {
      dashboardBody(
      # UI Options
        shinyjs::useShinyjs(), # For disable/able buttons
      # To detect browser settings to resize plots
        shinybrowser::detect(),
      # Add check to navigate from app
        tags$head(tags$script(
          HTML(
            "
              // Enable navigation prompt
                window.onbeforeunload = function() {
                    return 'Your changes will be lost!';
                };
            "
          )
        )),
      ## Busy indicator
        shinysky::busyIndicator(text = "Processing ... ", wait = 1000),
      # Pulls busy indicator to front
        tags$head(
          tags$style(HTML(
            ".shinysky-busy-indicator {z-index: 1000;}"
          )),
        # Add Stoelting logo
          tags$link(
            rel = "icon",
            type = "image/png",
            sizes = "32x32",
            href = "https://stoeltingco.com/static/images/icons/favicon.png"
          )
          
        ),
      
      # Adds bstooltips; "value" so "TRUE" doesn't show
        source("helper_functions/shinyBS_tooltips.R", local = TRUE)$value,
      
      # put the shinyauthr login ui module here
        shinyauthr::loginUI("login"),
      
      # Adds password help dropdown menu 1. Forgot password, 2. Reset password, 3. Change retrieve Q&A
      tags$div(id = "password_div",
               fluidRow(
                 column(12, align = "center",
                   tags$h4("Password Help"),
                   dropdownButton(
                     tags$h5("Password Support"),
                     tags$style(type = 'text/css',
                              ".dropdown-menu {margin-left: 50%; margin-right: 50%;}"),
                     # Dropdown options
                       actionButton("forgot_password", label = "Forgot password?"),
                       br(),
                       actionButton("reset_password", label = "Reset Password"),
                       br(),
                       actionButton("reset_password_retrieve", label = "Reset Password Retrieval Question"),
                     circle = TRUE,
                     status = "danger",
                     icon = icon("cog"),
                     width = "300px",
                     tooltip = tooltipOptions(title = "Click for forgotten password or reset password or retrieval question")
                    )
               ))), # End password div  
    ###*** setup tabs for 1. enter data, 2. report, 3. record management- UI in server to only render on password
      tabItems(
      ###*** Enter data tab has 1. Observation and 2. Interview tabs to enter form data
        tabItem("enter_data",
                uiOutput("enter_data_ui")
                ), # Close enter_data tabItem

      ### Tab 2. Interpretation has validity check and plots, table, and text interpretation for each scale
        tabItem("interpretation",
                uiOutput("interpretation_rows"),
                ), # Close Interpretation tabItem

      ### Tab 3. This tab contains data management and report formats
        tabItem("data_management",
                uiOutput("data_management_ui")
                )
        ) # Close tabItems
      ) # Close dashboard_body
    }
    
  ) # Final dashboardPage paren
}

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
# Password Set-up ----------------------------------------------------------
###*** App contains password management system to 1. grant access to use app for correct user_name/password, 
  ###* 2. retrieve forgotten password, and 3. change those values. Data is persistently stored in googlesheet
  ###* This file calls modules in helper_functions/password_reset_functions.R

###********* Password Set-up ***********###

###*** Following sets up dataframe from collected spreadsheet with user information
  ### credentials()$user_auth will then be true when correct combo entered, use to validate
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
# call the logout module with reactive trigger to hide/show the module based on password success
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
###*** Logout button and action- will refresh page- used instead of logoutServer b/c rest of ui needs to update as on load
  
  output$logout_button_ui <- renderUI({
    req(credentials()$user_auth)
    shiny::actionButton("logout_button", "Log Out/Manage Password", 
                        class = "btn-danger", style = "color: white; padding: 8px;")
  })
  
  observeEvent(input$logout_button,{
    refresh()
  })

###*** Close password page and open sidebar and mainpage
  observe({
    if(credentials()$user_auth) {
    # Removes password page upon success
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::hide("password_div") # password help hide/show only at login
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show("password_div") # password help hide/show only at login
    }
  })

# Sidebar UI Set-up -------------------------------------------------------

# only when credentials()$user_auth is TRUE, render sidebar menu
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(id = "sidebar_tabs",
      # Following are all tabs, shown in sidebar "menu"
      menuItem("Enter Data",
        tabName = "enter_data",
        icon = shiny::icon("clipboard-list")
      ),
      menuItem("Interpretation",
        tabName = "interpretation",
        icon = shiny::icon("chart-bar")
      ),
      menuItem("Reports and Records",
        tabName = "data_management",
        icon = shiny::icon("book-open")
      ),
    ### Following is demographic ui widgets, put into two columns
      fluidRow(
        column(6, offset = 0, # style='padding:0px;',
        ## slides over each row of ui_demographics df, applies remove_na_func, from shiny_helper_functions.R
          slider::slide(ui_demographics[1:which(ui_demographics$inputId == "test_date"),], remove_na_func),
          tableOutput("age_out") # Calculates age if valid, or prompts if not
        ),
      column(6, offset = 0, style='padding-left:0px;',
          slider::slide(ui_demographics[-(1:which(ui_demographics$inputId == "test_date")),], remove_na_func)))
    )
  })
  
# Password Reset Help -----------------------------------------------------

# Forgot password retrieve question Modals; modals in password_reset_functions.R; googlesheet queried each query to accept updates
  # For each selection modal window will generate that prompts. Appropriate modal response generates based on response
  source("helper_functions/password_reset_functions.R")
  
###*** Retrieve password ***### 
  {
  ## I. Enter user name to get retrieve question
    observeEvent(input$forgot_password, {
      showModal(forgot_password_modal_func())
    })
    
    ## IA. If valid user_name -> Show retrieve question with answer field or
      # IB. if invalid user_name -> display msg. stating invalid
      observeEvent(input$forgot_password_user_name_submit,{
        showModal(
          forgot_password_retrieve_question_func(
            googlesheet_url = googlesheet_url,
            user_name_input = input$forgot_password_user_name_input,
            user_name_column = "customer_email",
            selected_column = "customer_retrieve_password_question"
          )
        )
      })
      
    ## IA1. If correct retrieval answer input -> Display password or
      # IA2. If incorrect retrieval answer -> Display msg.
    observeEvent(input$forgot_password_question_submit,{
      showModal(
        display_password_func(
          googlesheet_url = googlesheet_url,
          user_name_input = input$forgot_password_user_name_input,
          user_name_column = "customer_email",
          selected_column = "customer_password",
          variable_name = "customer_retrieve_password_answer",
          forgot_password_question_input = input$forgot_password_question_input
        )
      )
    })
  }
  
###*** Reset password ***###
  {
  # I. Prompt for current user name and password
    observeEvent(input$reset_password, {
      showModal(reset_password_func())
    })
    
  # IA. If correct current user name and password entered -> display new password entry input, or
    # IB. If incorrect user name or password -> display msg.
    observeEvent(input$reset_password_submit, {
      google_df <- read_sheet(googlesheet_url) 
      showModal(
        reset_password_feedback_func(
          googlesheet_url = googlesheet_url,
          user_name_input = input$reset_password_user_name_input,
          user_name_column = "customer_email",
          selected_column = "customer_password",
          existing_password_input = input$reset_password_password_input
        )
      )
    })
    # New password feedback and update
      # 1A1. If new password acceptable -> password changed msg
      # 1A2. If new passord problem (blank) -> password not changed msg.
      observeEvent(input$reset_password_commit, {
        if (trimws(input$reset_password_new_input) != ""){
          password_reset_commit_func(
            new_password_input = input$reset_password_new_input,
            user_name_input = input$reset_password_user_name_input,
            googlesheet_url = googlesheet_url
          )
          showModal(
            password_updated_func()
          )
        } else{
          showModal(
            unacceptable_information_func()
          )
        }
      })
  }
  
###*** Reset Password Retrieval Question ***###
  {
  # 1. Prompts for current user name and password
    observeEvent(input$reset_password_retrieve,{
      showModal(
        reset_password_retrieve_func()
      )
    })
    # 1A. If correct user_name and password -> Prompt for new retrieve question and answer, or
        # 1B. if incorrect user_name and password -> display incorrect msg.
      observeEvent(input$reset_password_retrieve_submit,{
        google_df <- read_sheet(googlesheet_url) 
        showModal(
          reset_password_retrieve_feedback_func(
            googlesheet_url = googlesheet_url,
            user_name_input = input$reset_password_retrieve_user_name_input,
            user_name_column = "customer_email",
            selected_column = "customer_password",
            existing_password_input = input$reset_password_retrieve_password_input
          )
        )
      })
          # Feedback on new question and answer and commit
            # 1A1. If valid new question and answer (not blank) -> changed msg.
            # 1A2. If invalid -> not changed msg.
          observeEvent(input$reset_password_retrieve_commit,{
            if (trimws(input$reset_password_retrieve_question_new_input) != "" &
                trimws(input$reset_password_retrieve_answer_new_input) != ""){
              password_retrieve_reset_commit_func(
                new_password_retrieve_question_input = input$reset_password_retrieve_question_new_input,
                new_password_retrieve_answer_input = input$reset_password_retrieve_answer_new_input,
                user_name_input = input$reset_password_retrieve_user_name_input,
                googlesheet_url = googlesheet_url
              )
              showModal(
                password_retrieve_updated_func()
              )
            } else{
              showModal(
                unacceptable_information_func()
              )
            }
          })
  }
  
  
# Generate Mainpage UI inputs- Set Up Initial Questions and Prompts ------------------------------------------------------
  ### "shiny_helper_functions.R" already loaded. in Load Data and options section. Used here
  ### Calculate Age from birth date to test date and output in table
  source("helper_functions/shiny_age_functions.R", local = TRUE)
  
  #### Generate UI for different sections function
  
  ###*** Additional ui (directions) for interview and observation tabs. 
  {
    additional_items_observation <- tagList(
      h3("Directions"),
      hr(),
      h4(
        "Present the manipulatives one at a time and allow
                                                  the child to explore each object independently.
                                                  For each item, circle “Yes” or “No” based on the
                                                  presence or absence of the specified behavior. If at
                                                  least one item in any subscale receives a score of
                                                  1, complete the severity item for that subscale."
      )
    )
    
    additional_items_interview <- tagList(
      h3("Instructions"),
      hr(),
      h4(
        "I am going to ask you several questions about your child's sensory preferences. I am interested in hearing about your child’s responses to visual, tactile, and
                                                  auditory stimuli. The word ‘stimuli’ refers to any objects or toys that your child comes across during his/her everyday life. I will ask you a series of ‘Yes’ and ‘No’
                                                  questions about your child’s sensory responses over the past month. If your child engaged in a sensory behavior in the past that is no longer present, please let me
                                                  know and I will note it separately."
      ),
      h4(
        "The first set of questions is about [your child’s] visual responses."
      )
    )
  }
  
  ######****** Following is function to filter dynamically with condition as argument
  ######* One way is with explicit arguments for condition
  ######* Second way is to pass condition as string. First way is preferred if one condition, not changing

######* BEGIN
  # filter_df_generate_ui_func <- function(df, op, var, value){
  # df %>%
  #   filter(op(.data[[var]], .env$value)) %>%
# To use function with each condition part as argument, uncomment above and comment here to
  filter_df_generate_ui_func <- function(df, condition) {
    # condition_call <- substitute(text = condition) # Use this if unquoted expression
    condition_call <- parse(text = condition)
    r <- eval(condition_call, df, parent.frame())
    df %>%
      filter(r) %>%
      # filter(eval(parse(text = condition))) %>% #
    # here. Above is filter function, rest is preparing data- specific to this app
      rowwise() %>% # Necessary to combine to list columns by row
    # Get data column names as names of SHINY widget arguments
      mutate(
        item = paste(item_number, item), # question number and label
        inputId = id, # rename
    # When choices are in separate columns-> list with vectors of choices
        choices = list(c(response_2, response_1)),
        inline = T
      ) %>%
    # Only select columns that are in shiny widget arguments
      dplyr::select(ui_type = item_type, inputId, label = item, choices, inline) %>%
    # Apply na_func by row
      slider::slide(., remove_na_func)
  }
  
  # ####**** Function that extracts from df based on condition ("conditional_value") and renders ui to a named 
          # section ("set_of_items")- use to build different tabs for example
  assign_ui_to_output_func <- function(set_of_items, conditional_value, additional_items){
    output[[set_of_items]] <- renderUI({
      req(credentials()$user_auth)
    # If use supply each value conditional in above func then uncomment next, supply args, and comment after
      # filter_df_generate_ui_func(df, op, var, value) # like (sand, `==`, "questionnaire", conditional_value)
      tagList(
        additional_items, # Separate ui not generated with function
        filter_df_generate_ui_func(df = sand, condition = conditional_value)
      )
    })
  }
  
###*** Binds additional ui (directions) with generated ui
  pmap(
    .l = list(
      set_of_items = c("observed_items_ui", "reported_items_ui"),
      conditional_value = c("questionnaire == 'observation'",
                             "questionnaire == 'interview'"),
      additional_items = list(additional_items_observation, additional_items_interview)
    ),
    .f = assign_ui_to_output_func
  )
  
###*** Generate UI for tabItems here in server to req password input
{
## First tab- form with two tabs to enter data
  output$enter_data_ui <- renderUI({
    req(credentials()$user_auth)
    tabsetPanel(id = "tabs", 
    ##** Observation tab
      tabPanel("Observation",
               fluidRow(
                 box(
                   # Generate ui items server-side for all observation items
                   uiOutput("observed_items_ui"),
                   width = 12 # Use full space
                 ))), # Close Observation tabPanel
    ##** Interview tab
      tabPanel("Interview",
               fluidRow(
                 box(
                   # Generate ui items server-side for all interview items
                   uiOutput("reported_items_ui"),
                   width = 12
                 ))) # Closes Interview tabPanel
    )
  })
  
## Third tab (second tab does not have additional UI, so generated in ui) for managing data and reports
  output$data_management_ui <- renderUI({
    req(credentials()$user_auth)
  ### Tab 3. This tab contains data management and report formats
    tabItem("data_management",
            h2("Reports and Record Management"),
            
            # This row allows current record 1. Generate report, 2. Save data
            fluidRow(
              h3("Current Session Report and Save"),
              box(
                uiOutput("report_selectors")
              ),
              box(
                uiOutput("save_client")
              )
            ),
            
            # This row has restore options 1. Folder of records, 2. Individual, 3. Multiple
            fluidRow(
              uiOutput("restore_previous_session_ui"),
              
              # Displays selected client output by 1. all records, 2. selected record, 3, invalid record and
              # 4. has options to a. restore client session, b. score multiple records, c. download template
              uiOutput("load_client_ui")
            )
    ) # Close data_management tabItem
  })
}
  
###*** Adds directions and hides follow-up questions where appropriate after inputs ###***
{
  observeEvent(input[[ids_after_directions_reported$id[1]]],{ # Wait until all renderUI inputs are loaded
  # Add directions to introduce sections
    pmap(
      .l = list(
        ids_after_directions_reported_var = c(rep(ids_after_directions_reported$id, 3), 
                                              ids_after_notes$id),
        placement_location_var = c(rep(rep("beforeBegin", length(ids_after_directions_reported$id)), 3),
                                    rep("afterEnd", length(ids_after_notes$id))),
        content_var = c(rep("", length(ids_after_directions_reported$id)), toupper(subscale_names_df$Subscale),
                        directions$item, paste("NOTE:", ids_after_notes$note)),
        header_level_var = c(rep("hr", length(ids_after_directions_observed$id)), rep("h4", length(ids_after_directions_reported$id)),
                            rep("h5", length(ids_after_directions_reported$id)), rep("h4", length(ids_after_notes$id)))
      ),
      .f = function(
        ids_after_directions_reported_var, # Ids to place after
        placement_location_var, # Location to place content- before, after...
        content_var, # Content to insert
        header_level_var){ # Header level
        # gsub necessary to replace period in id with "\\.", which works with selector
          insertUI(
            req(credentials()$user_auth),
            selector = paste0("#", gsub("\\.", "\\\\.", ids_after_directions_reported_var)),
            where = placement_location_var,
            ui = tags$div(tags[[header_level_var]](content_var))
          )
      })
    
  # Hide follow-up questions on load, will show upon conditional, later
    lapply(sand$id[sand$hierarchy == 2 & sand$questionnaire == "interview"], hide_function)
  }, once = TRUE) # Only add once, at load

# Same as previous function, with observation questions
  observeEvent(input[[ids_after_directions_observed$id[1]]],{
    pmap(
      .l = list(
        ids_after_directions_observed = c(ids_after_directions_observed$id, ids_after_directions_observed$id),
        content_var = c(rep("", length(ids_after_directions_observed$id)), toupper(subscale_names_df$Subscale)),
        header_level = c(rep("hr", length(ids_after_directions_observed$id)), rep("h4", length(ids_after_directions_observed$id)))
      ),
      .f = function(ids_after_directions_observed_var, content_var, header_level){
        insertUI(
          req(credentials()$user_auth),
        # gsub necessary to replace period in id with "\\.", which works with selector
          selector = paste0("#", gsub("\\.", "\\\\.", ids_after_directions_observed_var)),
          where ="beforeBegin",
          ui = tags$div(tags[[header_level]](content_var))
        )
      })
    # Hide follow-up questions on load
    lapply(sand$id[sand$hierarchy == 2 & sand$questionnaire == "observation"], hide_function)
  }, once = TRUE)
}
  
# Follow-up Severity Questions Functions --------------------------------------------
  
  # Hide and Show questions functions
  hide_function <- function(hide_id){
    shinyjs:: hide(
      id = hide_id
    )
  }
  show_function <- function(show_id){
    shinyjs:: show(
      id = show_id
    )
  }
  
# Gather Inputs -----------------------------------------------------------
  
# Gather inputs- reactive values to store inputs and convert to points, appended to sand df
  sand_inputs <- reactiveValues(
    df = sand %>% 
      mutate(
        input_value = rep(NA, nrow(sand)),
        points = rep(NA, nrow(sand))
      )
  )
  
###*** Gather inputs, convert values to points, assign to sand_inputs$df
   observe({
  # Only if record is either not restored or if interview tab has been visited (otherwise will update with NA)
    # This is b/c ui doesn't render immediately, so restored record will not update tabs not visited
    # Do this for any tabs that are not landing page tabs
     if(update_tabs_reactive_counters$interview == 1){ # Counter for if record has been restored
      sand_inputs$df <- sand_inputs$df %>%
        filter(questionnaire != "interview") %>% # Make all this variable depending on input$tabs for more tabs
        rowwise() %>%
        mutate(
          input_value = list(input[[id]]), # Prompted for this
          input_value = ifelse(is.null(unlist(input_value)), NA, input_value),
          points = case_when(input_value == "No" ~ 0,
                             input_value == "Yes" ~ 1,
                             input_value == "Mild" ~ 1,
                             input_value == "Moderate-to-Severe" ~ 2
          )
        ) %>%
        ungroup() %>%
        bind_rows(.,
                  sand_inputs$df %>%
                    filter(questionnaire == "interview"))
      # Else don't filter and gather all inputs
     } else {
       sand_inputs$df <- sand %>%
      rowwise() %>%
      mutate(
        input_value = list(input[[id]]), # Prompted for this
        input_value = ifelse(is.null(unlist(input_value)), NA, input_value),
        points = case_when(input_value == "No" ~ 0,
                           input_value == "Yes" ~ 1,
                           input_value == "Mild" ~ 1,
                           input_value == "Moderate-to-Severe" ~ 2
        )
      ) %>%
      ungroup()
     } 
  })
  
# Show/Hide Follow-up Questions -------------------------------------------
  
##** Function to get ids where any hierarchy 1 questions in same parent_class are endorsed
  follow_up_parent_class <- reactive({
    sand_inputs$df %>% 
      filter(!is.na(parent_class)) %>% # Only hierarchy 1
      rowwise() %>% 
      mutate(equal_to_critical = list(input_value == critical_value)) %>% # If equal to critical value
      ungroup() %>% 
      group_by(parent_class) %>% # Same parent_class
    # TRUE for any group of parent_class with an item endorsed
      dplyr::summarize(
        show_child = any(equal_to_critical == TRUE)
      ) %>% 
      filter(show_child == TRUE) %>% # Just those that will have follow-ups
      inner_join(., sand, by = c("parent_class" = "child_class")) %>%
      dplyr::select(id)
  })
  
##** Show or hide hierarchy 2 child items if any hierarchy 1 parents meet critical values
  observe({
    req(follow_up_parent_class()) # Only if there's been responses
  # Check if each hierachy 2 item bas been identified in follow_up_parent_class func
    lapply(sand$id[sand$hierarchy ==2], function(ids){
      if (ids %in% follow_up_parent_class()$id){
        show_function(ids)
      } else {
        hide_function(ids)
      }
    })
  })
  
# Sum points by section ---------------------------------------------------
  
  ###*** Following gets all active points- hierarchy 1 points and hierarchy 2 points when hierarchy 1 item
  ###* endorsed. Also gets parent/child class as subscale, to group by subscale
  
# Get all hierarchy 1 points
  hierarchy_1_points_func <- function(){
    sand_inputs$df %>% 
      filter(hierarchy == 1) %>% 
      dplyr::select(subscale = parent_class, points)
  }
  
# Get all relevant hierarchy 2 points and bind with hierarchy 1 points
  points_subscale_func <- function(){
    req(follow_up_parent_class())
    inner_join(follow_up_parent_class(), sand_inputs$df, by = "id") %>% 
      dplyr::select(subscale = child_class, points) %>% 
      bind_rows(., hierarchy_1_points_func()) 
  }
  
# Function passed to plots and tables to obtain elevated cutoff, based on critical cutoff- 1SD away
  elevated_score_func <- function(table_of_accuracies, 
                                  critical_row_index){
    table_of_accuracies %>% 
      tibble::rowid_to_column(., var = "rowid") %>% {
        . -> tmp
        # Get the Lower Bound CI at the cutoff score and find the least score who's Upper CI is less than that-
        # it is statistically within one SD
        tmp %>% 
          filter(., Score == critical_row_index) %>% 
          dplyr::select(`CI Low`) %>% pull -> ci_low_min
        tmp %>% 
          dplyr::filter(ci_low_min <= `CI High`) %>% 
          dplyr::slice_min(Score) %>% 
          dplyr::select(Score) %>% pull 
      }
  }

# Plots -------------------------------------------------------------------

###*** Bar plots of total scores, grouped by subscale, with critical scores marked

## These are conditions for grouping the data that will be the plot groups
  case_when_pattern_list <- list(
  # By source
    "grepl('r', subscale) ~ 'Interview';
        TRUE ~ 'Observation'",
  # Total
    "TRUE ~ 'Total'",
    
  # Domain
    "grepl('1', subscale) ~ 'Hyperreactivity';
         grepl('2', subscale) ~ 'Hyporeactivity';
         grepl('3', subscale) ~ 'Seeking'",
    
  # Modality
    "grepl('v', subscale) ~ 'Visual';
         grepl('t', subscale) ~ 'Tactile';
         grepl('a', subscale) ~ 'Auditory'",
    
  # Subscale
    "grepl('v1', subscale) ~ 'Visual Hyperreactivity';
         grepl('v2', subscale) ~ 'Visual Hyporeactivity';
         grepl('v3', subscale) ~ 'Visual Seeking';
         grepl('t1', subscale) ~ 'Tactile Hyperreactivity';
         grepl('t2', subscale) ~ 'Tactile Hyporeactivity';
         grepl('t3', subscale) ~ 'Tactile Seeking';
         grepl('a1', subscale) ~ 'Auditory Hyperreactivity';
         grepl('a2', subscale) ~ 'Auditory Hyporeactivity';
         grepl('a3', subscale) ~ 'Auditory Seeking';"
  )
  
# Function to apply conditional, plot, and add options
  general_plot_func <- function(case_when_pattern, # Conditions
                                table_of_accuracies, # Accuracy measures from research
                                critical_row_index, # Clinical cutoff scores
                                plot_title,
                                y_limit_low, # Y boundaries
                                y_limit_upper,
                                output_name){ 
    
  # Yellow zone shows elevated
    yellow_boundary <- elevated_score_func(table_of_accuracies, 
                                           critical_row_index)

  # Used to set up the critical and elevated score boundaries, put one that will appear on top last ("red")
    significance_df <- tibble(
      y_value = rep(critical_row_index, 2),
      slope = rep(0, 2),
      color = c("yellow", "red")
    )
    
  # Get all the points with subscale, group by subscale, based on condition, and plot
    points_subscale_func() %>% # Get all points and subscale
      mutate(
        sorting_var = case_when(!!! rlang::parse_exprs(case_when_pattern)) %>% # Group by condition
          gsub(" ", "\n", .) %>% # To line break long subscales
          forcats::fct_inorder(.)
      ) %>% 
      ggplot(., aes(x = sorting_var, y = points)) + # Total points for each subscale
      geom_bar(stat = "identity") +
      ggtitle(plot_title) +
      labs(x = "", y = "Score") +
      scale_y_continuous(breaks = seq(2,72,2)) +
    # Plot cutoff score that varies by scale, except for validity plot ("Source" is title)
      {if (plot_title != "Source") geom_abline(data = significance_df,
                                                 aes(intercept = y_value, slope = slope, color = color))} +
      # Plot cutoff score that varies by scale, except for validity plot ("Source" is title)
      {if (plot_title == "Subscale") scale_x_discrete(guide = guide_axis(angle = 45))} +
      theme(plot.title = element_text(hjust = 0.5)) +
    # Changes legend labels
      scale_color_identity(labels=c("Critical", "Elevated"), guide="legend") +
      labs(color = "Significance") +
      expand_limits(y = c(y_limit_low, y_limit_upper)) + # Fit according to record forms
    # Create elevated zone
      annotation_raster(alpha("yellow", .2),
                        xmin = -Inf, xmax = Inf,
                        ymin = yellow_boundary, ymax = critical_row_index)
  }
# Not used here, but useful for dynamically resizing plots
  # output$total_plot <- renderPlot({
  #   total_plot_function()
  # }, width = shiny:::exprToFunction(<function that gets number of groups in plot>))
  
###*** Function to call above general_plot_func and dynamically generates output- named renderPlot ***###
  plots_func <- function(case_when_pattern, 
                         table_of_accuracies,
                         critical_row_index,
                         plot_title,
                         y_limit_low,
                         y_limit_upper,
                         output_name){
    
  # Dynamic size plot width by number of groups
    plot_width <- points_subscale_func() %>% 
      mutate(
        sorting_var = case_when(!!! rlang::parse_exprs(case_when_pattern)) %>% 
          forcats::fct_inorder(.)
      ) %>% 
      dplyr::summarize(plot_width = length(unique(sorting_var))) %>% pull
    
    general_plot_func(case_when_pattern, 
                      table_of_accuracies,
                      critical_row_index,
                      plot_title,
                      y_limit_low,
                      y_limit_upper,
                      output_name) -> p
  # Plots resize dynamically by 1) browser size- get_width() and 2) number of groups- plot_width
    output[[paste0(output_name, "_plot")]] <- renderPlot(p, width = ((shinybrowser::get_width()/5) + 75*plot_width))
  }
  
###*** Call function to generate plots, they are then passed to renderUi (separate call for rmarkdown [without renderPlot])
  observe({
    req(input$sidebar_tabs == "interpretation") # Only when viewing interpretation tab
    pmap(
      .l = list(
        case_when_pattern = case_when_pattern_list,
      # These are generated in probability_tables.R
        table_of_accuracies = c(list(accuracy_table, accuracy_table), subscale_probability_list),
      # Cutoff scores are user selected and from probability functions-- 4 is to suppress cutoff on validity
        critical_row_index = as.list(c(4, cut_select$score, critical_clinical_scores)),
        plot_title = list("Source", "Total", "Domain", "Modality", "Subscale"),
        y_limit_low = list(0,0, 0, 0, 0),
      # To match paper forms
        y_limit_upper = list(20, 20, 10, 10, 10),
        output_name = list("Source", "Total", "Domain", "Modality", "Subscale")
      ),
      .f = plots_func
    )
  })
  
# Section Tables ----------------------------------------------------------

###*** Construct tables to report summary score, significance, and percentile for each score
  table_calculation_func <- function(case_when_pattern, 
                                     table_of_accuracies, 
                                     critical_row_index, 
                                     percentile_list){
    
  # Gets elevated score cutoff
    elevated_score <- elevated_score_func(table_of_accuracies, critical_row_index)

  # Similar to plots- get total points, group by scales, then summarize
    points_subscale_func() %>% 
      mutate(
        sorting_var = case_when(!!! rlang::parse_exprs(case_when_pattern)) %>% 
          forcats::fct_inorder(.)
      ) %>% 
      group_by(sorting_var) %>% 
      dplyr::summarize(
        total = sum(points)
      ) %>% 
      rowwise() %>% 
      mutate(
        above_critical = 
          case_when(
            total >= critical_row_index ~ "Clinical", 
            total >= elevated_score ~ "Elevated",
            TRUE ~ "Typical"
          )
      ) %>% 
    # Get percentiles from percentile_list and join
      inner_join(., percentile_list %>% 
                   dplyr::select(Score, percentile = Percentiles), 
                 by = c("total" = "Score")) %>%
      ungroup() -> df
    
  # If-then to handle the Validity calculation, which is different than others
    # Validity calc will have "Observation" as sorting_var, and "Interview" if that form is completed
    if(df %>% 
        dplyr::summarize(across(sorting_var, ~any(grepl("Observation", .x)))) %>% 
        pull){
          req(df %>% 
                dplyr::summarize(across(sorting_var, ~any(grepl("Interview", .x)))) %>% 
                pull)
        # Get the abs difference. If greater than 14, invalid 
          df %>% 
            dplyr::select(-c(above_critical, percentile)) %>% 
            pivot_wider(., names_from = sorting_var, values_from = total) %>%
            mutate(Valid = ifelse(abs(Observation - Interview) >= 14, 
                                  "Discrepancy", 
                                  "No Discrepancy"))
          
      # If not validity check, just compare score to cutoff
      } else {
        df %>% 
          dplyr::rename(
            "Scale" = "sorting_var",
            "Raw Score" = "total",
            "Significant" = "above_critical",
            "Percentile" = "percentile"
          )
        }
    }
  
###*** Calls table_calculation_func and renders DT
  table_sum_func <- function(case_when_pattern, 
                             table_of_accuracies, 
                             critical_row_index, 
                             output_name, 
                             percentile_list){
    t<- table_calculation_func(
      case_when_pattern, 
      table_of_accuracies, 
      critical_row_index,
      percentile_list)
    output[[paste0(output_name, "_table")]] <- renderDT(
    # Give simple DT
      DT::datatable(
        t, 
        rownames = FALSE,
        options = list(
          searching = FALSE,
          paging = FALSE,
          selection = 'single',
          dom = 't'
        )
      )
    )
  }
  
# There is a separate call for rmarkdown. This call renders in interpretation panel
  # observeEvent(req(input$sidebar_tabs == "interpretation"),{
  observe({
    req(input$sidebar_tabs == "interpretation")
    pmap(
      .l = list(
        case_when_pattern = case_when_pattern_list,
      # Accuracy tables from probability_tables.R to get elevated score range
        table_of_accuracies = c(list(accuracy_table, accuracy_table), subscale_probability_list),
      # 5 is lowest value to suppress cut score in Source table; cut_select$score to allow user input
        critical_row_index = as.list(c(5, cut_select$score, critical_clinical_scores)), 
        output_name = list("Source", "Total", "Domain", "Modality", "Subscale"),
      # From probability_tables.R, percentile lists
        percentile_list = percentile_lists
      ),
      .f = table_sum_func
    )
  })
  
# Textual interpretation output -----------------------------------------
  
###*** Functions to generate dynamic text for interpretation ***###

## Name generator- options to select name style; based on what is entered, returns that format or more generic
  # name_options is c("first", "last", "full") person is c("examinee", "examiner")
  name_generator <- function(name_options = "first", person = "examinee"){
    if (person == "examinee") {
      if (input$examinee_first_name == "" & input$examinee_last_name == ""){ # Nothing entered
        "Examinee"
      } else if(input$examinee_first_name != "" & input$examinee_first_name == "") { # only first name
        input$examinee_first_name
      } else if(input$examinee_first_name == "" & input$examinee_last_name != "") { # only last name
        paste("Examinee", input$examinee_last_name)
      } else if(input$examinee_last_name != "" & input$examinee_first_name != "" # first or last name, with option "last"
                & name_options == "last") {
        paste("Examinee", input$examinee_last_name)
      } else if(input$examinee_last_name != "" & input$examinee_first_name != "" 
                & name_options == "first") { # first or last, with name_option == "first"
        input$examinee_first_name
      } else if (name_options == "full") { # first or last, with name_option == "full"
        paste(input$examinee_first_name, input$examinee_last_name)
      } else{
        "Examinee" # Fallback 
      }
    # Same options as above, but with examiner
    } else if (person == "examiner") {
      if (input$examiner_first_name == "" & input$examiner_last_name == ""){
        "Examiner"
      } else if(input$examiner_first_name != "" & input$examiner_first_name == "") {
        input$examiner_first_name
      } else if(input$examiner_first_name == "" & input$examiner_last_name != "") {
        paste("Examiner", input$examiner_last_name)
      } else if(input$examiner_last_name != "" & input$examiner_first_name != "" 
                & name_options == "last") {
        paste("Examiner", input$examiner_last_name)
      } else if(input$examiner_last_name != "" & input$examiner_first_name != "" 
                & name_options == "first") {
        input$examiner_first_name
      } else if (name_options == "full") {
        paste(input$examiner_first_name, input$examiner_last_name)
      } else{
        "Examiner"
      }
    }
  }
  
###*** Narrative introductory text to describe administration
  introductory_text_func <- reactive({
    paste0(name_generator(name_options = "full", person = "examinee"), " was administered the Sensory Assessment for Neurodevelopmental Disorders (SAND) on ", 
           toOrdinalDate(input$test_date), ", by ", 
           name_generator(name_options = "full", person = "examiner"),
           ". ", "At the time of testing, ", 
           name_generator(name_options = "first", person = "examinee"), " was ", 
           floor(age_decimal(birth_date = input$birth_date, test_date = input$test_date)), 
           " years old and had begun ",
           toOrdinal(input$grade), 
           " grade. The SAND identifies presence of clinically significant sensory symptoms commonly observed in children with autism and related neurodevelopmental disorders. The SAND is a clinician-administered observational assessment and corresponding caregiver interview for quantifying sensory symptoms with low verbal content. It directly examines sensory hyperreactivity, hyporeactivity, and seeking behaviors (i.e., unusual sensory interests) across visual, tactile, and auditory modalities")
  })
  
###*** Source validity check interpretation

  validity_text_func <- reactive({
    table_calculation_func(
      case_when_pattern = case_when_pattern_list[[1]],
    # Accuracy tables from probability_tables.R to get elevated score range
      table_of_accuracies = accuracy_table,
    # 5 is lowest value to suppress cut score in Source table; cut_select$score to allow user input
      critical_row_index = 5, 
    # From probability_tables.R, percentile lists
      percentile_list = percentile_lists[[1]]
    )  %>%
      dplyr::summarize(out = ifelse(Valid == "No Discrepancy", 
                                    "There was no discrepancy", 
                                    "There was a significant discrepancy")) %>%
      dplyr::select(out) %>% 
      pull %>%
      paste(., 
            "between Total Observation and Interview scores.",
            sand_scale_explanations %>%
              filter(source == "Validity") %>%
              dplyr::select(explanation_of_scale)
      )
  })
  
  observe({
    req(input$sidebar_tabs == "interpretation")
    output[["Source_text"]] <- renderText({
      validity_text_func()
    })
  })
  
  
###*** Following is for text intrepretation identifying significant individual scales within groups, 
  ###*    by level of significance. Multiple options that adjust language based on number of scales.
{
  ## Function goes through each group of scales, identifies significant scales and constructs with appropriate language
    scale_text_significance_func <- function(df,
                                             colname_scores,
                                             colname_names,
                                             significance_level,
                                             significance_label) {
    # If any scores are above significance level
      if (df %>%
           filter(!!sym(colname_scores) == significance_level) %>%
          nrow(.) >= 1) {
      # Get the names of scales that are at that significance level
        df %>%
          filter(!!sym(colname_scores) == significance_level) %>%
          dplyr::select(!!sym(colname_names)) %>%
        # Pivot to wide to paste the scale names together, depending on number of scales
          pivot_wider(
            .,
            values_from = !!sym(colname_names),
            names_from = !!sym(colname_names),
            names_glue = sprintf("{%s}_{.value}", colname_names)
          ) %>% {
            . -> tmp # tmp is wide form data with significant scales as values with that scale name_value as head
        ## Gives appropriate language based on number of significant scales: 1, 2, 3+
          # 1 significant scale
            if (length(tmp) == 1) {
              tmp %>%
              # Get scale name, use singular language
                mutate(out = .[[1]], .keep = "none") %>%
                mutate(out = paste0("Scale ", out, " was ", significance_label, "."))
          # 2 significant scales
            } else if (length(tmp) == 2) {
              tmp %>%
              # Paste items together as "out" separated by "and", only keep "out"
                unite("out",
                      everything(),
                      sep = " and ",
                      remove = TRUE) %>%
              # Plural language
                mutate(out = paste0("Scales ", out, " were ", significance_label, "."))
            } else {
          # 3+ significant scales
              tmp %>%
              # Paste items together except last as "out" separated by ",", keep "out" and last item
                unite("out",-length(.), sep = ", ", remove = TRUE) %>%
              # Paste "out" and last item, separated by "and" (e.g., a, b, and c)
                unite("out", c(1, 2), sep = ", and ", remove = TRUE) %>%
              # Plural language
                mutate(out = paste0("Scales ", out, " were ", significance_label, "."))
            }
          }
      } %>%
      # Pulls message
        dplyr::select(out) %>% pull %>% return(.)
    # If no scales are significant, return message
      else{
        tibble(out = paste0("No scales were ", significance_label, "."))
      } %>%
      # Pulls message
        dplyr::select(out) %>% pull %>% return(.)
  }
    
  ## Apply scale_text_significance_func to test for both clinical and elevated significance
    text_interpretation_func <- function(case_when_pattern, 
                                         table_of_accuracies, 
                                         critical_row_index, 
                                         output_name, 
                                         percentile_list){
    # Evaluate table_calculation_func to get summed scores for each scale item
      t<- table_calculation_func(
        case_when_pattern,
        table_of_accuracies,
        critical_row_index,
        percentile_list)
    # Pastes together text results when evaluating scales for clinical and elevated significance and scale description
      paste (
      # This is explanation text of what each scale measures
        sand_scale_explanations %>%
          filter(source == output_name) %>%
          dplyr::select(explanation_of_scale),
      # t is scale scores, 'significant' is significant scales value, 'scale' is the significant scale name,
        # next is level of significance and interpretation
        # ifelse combines sentences into one to avoid redundancy
        ifelse(paste(scale_text_significance_func(t, "Significant", "Scale", "Clinical", "clinically significant"),
                      scale_text_significance_func(t, "Significant", "Scale", "Elevated", "elevated")) == "No scales were clinically significant. No scales were elevated.", 
               "No scales were clinically significant or elevated.", 
               paste(scale_text_significance_func(t, "Significant", "Scale", "Clinical", "clinically significant"),
                      scale_text_significance_func(t, "Significant", "Scale", "Elevated", "elevated"))
        )
      )
    }
    
  ## Outputs text output as renderText, not used in Rmarkdown
    text_interpretation_render <- function(case_when_pattern, 
                                           table_of_accuracies, 
                                           critical_row_index, 
                                           elevated_score,
                                           output_name, 
                                           percentile_list){
      output[[paste0(output_name, "_text")]] <- renderText(
        text_interpretation_func(case_when_pattern, 
                                 table_of_accuracies, 
                                 critical_row_index, 
                                 output_name, 
                                 percentile_list)
      )
    }

  ## When interpretation tab chosen, generates interpretive text
    observe({
      req(input$sidebar_tabs == "interpretation")
      pmap(
        .l = list(
          case_when_pattern = case_when_pattern_list[-1],
          table_of_accuracies = c(list(accuracy_table), subscale_probability_list),
          critical_row_index = list(cut_select$score, 8, 8, 5),
          output_name = list("Total", "Domain", "Modality", "Subscale"),
          percentile_list = percentile_lists[-1]
        ),
        .f = text_interpretation_render
      )
    })
  }
  

# Cut Score Select --------------------------------------------------------

## cut_select$score is a reactiveValues that user can select to be cut score for total
  cut_select = reactiveValues(
    score = 16
  )
  
## Table of scores and their associated probabilities and accuracies
  output$cut_select_table <- DT::renderDT(
  # Filter because table is filtered
    accuracy_table %>% 
      dplyr::filter(Sensitivity < 99.99 & Specificity < 99.99), 
    rownames = FALSE,
    selection = list(mode = 'single', selected = 12),
    options = list(searching = FALSE,
                   pageLength = 5,
                   paging = TRUE,
                   selection = list(mode = 'single', selected = 10),
                   displayStart = 11)
  )
  
## Assign score user selects to cut_select$score
  observe({
  # Upon user choice
    req(!is.null(input$cut_select_table_rows_selected))
    cut_select$score <- accuracy_table %>% 
      dplyr::filter(Sensitivity < 99.99 & Specificity < 99.99) %>% 
      slice(input$cut_select_table_rows_selected) %>% 
      dplyr::select(Score) %>% 
      pull
  })
  
# Combined interpretive output --------------------------------------------

###*** Puts all the output together in renderUI- validity and scales- plots, tables, text
  output$interpretation_rows <- renderUI({
  # User must be logged in
    req(credentials()$user_auth)
  # Validate that age is valid, can be between 0 and 150
    validate(
      need(input$test_date > input$birth_date, "Test date must be after birth date")
    )
    validate(
      need(age_decimal(birth_date = input$birth_date, test_date = input$test_date) %>%  between(., 0, 150), "Age must be between 0 and 150")
    )
  # tagList for multiple fluidRow
    tagList(
    # First section is validity check ("Source"), a little different than scales, so not in loop
      fluidRow(h3("Validity Check"),
               hr(),
               box(
                 plotOutput("Source_plot")
               ),
               fluidRow(
                 box(
                   dataTableOutput("Source_table")
                 ),
                 box(
                   textOutput("Source_text")
                 )
               )
      ),
      hr(),
    # For each scale, create a row with plot, and row with two boxes for table and text
      mapply(function(name, plot_width) {
      # Plot, adjust box width to plot width
        tagList (
          fluidRow(h3(name),
                   box(
                     title = paste(name, "Plot"), 
                     status = "primary", solidHeader = F, 
                     collapsible = T, width = plot_width, 
                     plotOutput(paste0(name, "_plot"))
                   )
          ),
          fluidRow(
            # Table
            box(
              title = paste(name, "Scores"),
              status = "primary", solidHeader = F,
              collapsible = T, width = 6,
              column(12, align="right", 
                     div(
                       dataTableOutput(paste0(name, "_table")), 
                       style = "font-size: 85%; width: 85%; margin-right:auto; margin-left:0px")
                     )
            ),
            # Text
            box(
              title = paste(name, "Interpretation"),
              status = "info", solidHeader = F,
              collapsible = T, width = 6,
              column(12, align="left", textOutput(paste0(name, "_text")))
            )
          )
        )
      }, name = c("Total", "Domain", "Modality", "Subscale"), 
      plot_width = c(6, 7, 7, 13)), # Just trial-and-error
    # Cut score table
      fluidRow(h2("Total Cut Score Select"),
               tipify(
                 div(dataTableOutput("cut_select_table"), 
                  style = "font-size: 85%; width: 85%; margin-left: auto; margin-right: auto"), 
                title = "Select Cut Score for Total by clicking row", placement = "top")
      )
    )
  })
  
# Save session Data- Third Tab -----------------------------------------------------------
  
###*** Construct UI and functionality for third tab to generate reports, report content in rmarkdown section
  
## UI for saving client information- individual record
  output$save_client <- renderUI({
    req(credentials()$user_auth)
    h4("Save Current Session")
    tipify(
      downloadButton("download_data", "Save Data", icon = shiny::icon("file-csv")),
      title = "Save current input to individual .csv file. Save in a dedicated folder.",
      placement = "right"
    )
  })
  
## Gives exact time to avoid duplicate file names
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  
## Function to gather all input information
  gather_inputs <- function(input_id, input_labels = NULL){
    map(input_id, ~ input[[.x]] %||% NA) %>% # NA if no input
      purrr::set_names(., input_labels)
  }
  
## Download Examinee Data as file- will use pattern to recognize legit files
  output$download_data <- downloadHandler(
    filename = function() {
    # Unique but identifiable file name
      paste(sprintf('%s_%s_%s',input$examinee_first_name, input$examinee_last_name, humanTime()), "_sand_data.csv", sep = "")
    },
    content = function(file) {
    # Get demographic information
      cd<- gather_inputs(ui_demographics$inputId, ui_demographics$inputId) %>% 
        bind_cols(., 
                # Bind with item responses
                  sand_inputs$df %>%
                    dplyr::select(id, input_value) %>%
                    pivot_wider(names_from = id, values_from = input_value)
        ) %>% 
      # Output dates in format used in app
        mutate(test_date = as.Date(test_date, format = "%m-%d-%Y"),
               birth_date = as.Date(birth_date, format = "%m-%d-%Y"))
      write.csv(cd, file, row.names = F)
    }
  )
  
# Rmarkdown Report --------------------------------------------------------
  
###*** Generates rmarkdown report and functionality for user to select and generate report
## UI to select report format
  output$report_selectors <- renderUI({
    req(credentials()$user_auth)
    ## Taglist for multiple ui in one render
    tagList(
      ## Choose output format
      h4("Report"),
      tipify(
        radioButtons(
          "report_format",
          "Choose format for report",
          choices =
            c(
              "HTML" = "html",
              "PDF" = "pdf",
              "Word/DOCX" = "docx"
            ),
          inline = TRUE
        ),
        title = "HTML- nice interactive format. PDF- nice static format. Word/DOCX- editable doc.",
        placement = "below"
      ),
      # Start generate_report as disabled until valid data
        disabled(
          tipify(
            downloadButton(
              "generate_report",
              "Generate Report",
              icon = shiny::icon("file-contract")
            ),
            title = "Choose desired report format and click to download a report",
            placement = "right"
            )
          )
    )
  })
  
# Age must be valid to enable report button
  age <- reactiveValues(
    val = 20
  )
  
# Multiple event listen
  date_listen <- reactive({
    list(input$birth_date, input$test_date, input$restore_session)
  })
  
  execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
    observeEvent(once = TRUE, reactiveValuesToList(session$input), {
      force(expr)
    }, ignoreInit = TRUE)
  }
  
# Toggle report button depending if age is valid and all inputs have data
  observeEvent(req(input$sidebar_tabs == "data_management"),{
    req(c(input$test_date, input$birth_date))
    # Disable if invalid age
    if(input$test_date < input$birth_date){
      shinyjs::disable("generate_report")
    }
    req(input$test_date > input$birth_date)
    age$val = age_decimal(birth_date = input$birth_date, test_date = input$test_date)
    # If valid age and inputs all valid, enable
    if (age$val %>%  between(., 0, 150) & all(!is.na(sand_inputs$df$input_value))){
      shinyjs::enable("generate_report")
    } else {
      shinyjs::disable("generate_report")
    }
    execute_at_next_input({
      req(c(input$test_date, input$birth_date))
      # Disable if invalid age
      if(input$test_date < input$birth_date){
        shinyjs::disable("generate_report")
      }
      req(input$test_date > input$birth_date)
      age$val = age_decimal(birth_date = input$birth_date, test_date = input$test_date)
    # If valid age and inputs all valid, enable
      if (age$val %>%  between(., 0, 150) & all(!is.na(sand_inputs$df$input_value))){
        shinyjs::enable("generate_report")
      } else {
        shinyjs::disable("generate_report")
      }
    })
  })
  
## Download report details
  output$generate_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste('sand_report', input$examinee_last_name, input$examinee_first_name, format(Sys.time(), "%Y%m%d"), 
            sep = '_', paste0('.', input$report_format))
    },
    content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "sand_report.Rmd")
      file.copy("SAND_output.Rmd", tempReport, overwrite = TRUE)
    # I don't think needed because sourced in global, but a way to pass file to rmarkdown
      src2 <- file.path(tempdir(), 'shiny_helper_functions.R')
      file.copy("shiny_helper_functions.R", src2)

      params <- list(report_format = input$report_format,
                     plots = pmap(
                       .l = list(
                         case_when_pattern = case_when_pattern_list,
                         table_of_accuracies = c(list(accuracy_table, accuracy_table), subscale_probability_list),
                         critical_row_index = list(4, cut_select$score, 8, 8, 5),
                         plot_title = list("Source", "Total", "Domain", "Modality", "Subscale"),
                         y_limit_low = list(0,0, 0, 0, 0),
                         y_limit_upper = list(20, 20, 10, 10, 10),
                         output_name = list("Source", "Total", "Domain", "Modality", "Subscale")
                       ),
                       .f = general_plot_func
                     ),
                     tables = pmap(
                       .l = list(
                         case_when_pattern = case_when_pattern_list,
                         table_of_accuracies = c(list(accuracy_table, accuracy_table), subscale_probability_list),
                         critical_row_index = as.list(c(5, cut_select$score, critical_clinical_scores)), 
                         percentile_list = percentile_lists
                       ),
                       .f = table_calculation_func
                     ),
                     text_interpretation = pmap(
                       .l = list(
                         case_when_pattern = case_when_pattern_list[-1],
                         table_of_accuracies = c(list(accuracy_table), subscale_probability_list),
                         critical_row_index = list(cut_select$score, 8, 8, 5),
                         output_name = list("Total", "Domain", "Modality", "Subscale"),
                         percentile_list = percentile_lists[-1]
                       ),
                       .f = text_interpretation_func
                     ),
                     introductory_text = introductory_text_func(),
                    # For significant responses
                     sand_inputs = sand_inputs$df %>% filter((hierarchy == 1 & input_value == critical_value) | id %in% follow_up_parent_class()$id) %>% 
                       mutate(
                         subscale = tolower(item_number),
                         Subscale = case_when(!!! rlang::parse_exprs(case_when_pattern_list[[5]])),
                         Modality = case_when(!!! rlang::parse_exprs(case_when_pattern_list[[4]])),
                         item = gsub(".*symptoms). ", "", item)
                       ),
                     validity_text = validity_text_func(),
                     output_name = list("Source", "Total", "Domain", "Modality", "Subscale"),
                     dates = data.frame(test_date = input$test_date, birth_date = input$birth_date) %>% 
                       mutate(age = floor(age_decimal(birth_date = birth_date, test_date = test_date))),
                     demographics_inputs = as.data.frame(gather_inputs(ui_demographics$inputId, ui_demographics$inputId)),
                     licensed_user = user_base$name
                     
      )
   # # Knit the document, passing in the `params` list, and eval it in a
      # # child of the global environment (this isolates the code in the document
      # # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
      # Get yaml output format language
        output_format = switch(input$report_format,
            pdf = 'pdf_document',
            html = 'html_document',
            docx = 'word_document'
        ),
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
# Restore Past Records- UI- Choose method, tables to display, options to restore ----------------------------------------------------

## Restore previous session ui
  
  output$restore_previous_session_ui <- renderUI({
    req(credentials()$user_auth)
    h3("Restore Previous Session(s)")
    radioButtons(
      "upload_format",
      "Choose Upload Format",
      choices = c("Folder", "Individual", "Multiple Records One File"),
      inline = T
    )
  })
## Client chooses method to upload records and will do so
  output$load_client_ui <- renderUI({
    req(credentials()$user_auth)
    req(input$upload_format)
    fluidRow(column(4,
                  ## Client chooses folder and all stroop records will be loaded to table in app
                    if (input$upload_format == "Folder")
                    {
                      wellPanel(
                        tags$div(
                          class = "form-group shiny-input-container",
                          tags$div(tags$label("Choose Folder with Individual Records")),
                          tipify(
                            tags$div(
                              tags$label(
                                "Choose folder",
                                class = "btn btn-primary",
                                tags$input(
                                  id = "fileIn",
                                  webkitdirectory = TRUE,
                                  type = "file",
                                  style = "display: none;",
                                  onchange = "pressed()"
                                )
                              )
                            ),
                            "Choose a dedicated folder with individual records, which will display in table."),
                          tags$label("No folder chosen", id = "noFile"),
                          tags$div(
                            id = "fileIn_progress",
                            class = "progress progress-striped active shiny-file-input-progress",
                            tags$div(class = "progress-bar")
                          )
                        )
                      )
                  ## Can choose an individual record
                    } else if (input$upload_format == "Individual") {
                      tipify(
                        fileInput("upload_individual_file", "Choose Individual Record"),
                        "Choose an individual .csv file that was saved"
                      )
                    } else {
                  ## Can choose a file with multiple records
                      tipify(
                        fileInput("uploadFile", "XLSX file"),
                        "Choose a .xlsx file of multiple records that was downloaded from this app."
                      )
                    }
    ),
    column(8,
           tabsetPanel(
            ## Displays all records chosen from input method
             tabPanel(
               "Previous Session Data",
               h3("Available Records"),
               dataTableOutput("full_patient_record"),
               bsTooltip(
                 "full_patient_record",
                 "All records available. Select individual or multiple records to load into Selected Record table.",
                 "left",
                 options = list(container = "body")
               ),
               hr(),
            ## Can choose one record to restore or download
               h3("Selected Record"),
               rHandsontableOutput("selected_patient"),
            bsTooltip(
              "selected_patient",
              "Records available to process. Click row header to select individual record to restore or all records in table will be scored for 'Score and Download Records.'",
              "left",
              options = list(container = "body")
            ),
            ## If any invalid values, will identify
               dataTableOutput("invalid_df"),
               bsTooltip(
                 "invalid_df",
                 "Records with invalid values will show with problem cells identified. Edit in table to fix.",
                 "left",
                 options = list(container = "body")
               ),
               br(),
              ## Options of what to do with records
               fluidRow(
                ## Restore one record as current session
                 column(4,
                        disabled(shiny::actionButton("restore_session", "Restore Selected Record")),
                        bsTooltip(
                          "restore_session",
                          "Click on desired record row header above and click button to restore an individual session.",
                          "right",
                          options = list(container = "body")
                        )
                 ),
                ## Can download multiple records
                 column(4,
                        disabled(downloadButton("save_all_records", 
                                                HTML("Score and Download </br> All Records"),
                                 )),
                        bsTooltip(
                          "save_all_records",
                          "Will calculate composite scores and save all selected records to .csv.",
                          "right",
                          options = list(container = "body")
                        )
                 ),
                ## Can download an appropriate template
                 column(4,
                        tipify(
                          shiny::a(h4(HTML("Download Multiple </br> Record Template"), class = "btn btn-default action-button" ,
                                      style = "fontweight:600;"), 
                                      target = "_blank",
                                   ## File must be in www folder
                                   href = "sand_multiple_records_template.xlsx",
                                   download = NA),
                          "Download an .xlsx template to enter raw data, then upload to perform scoring analysis.",
                          "left"
                        )
                 )
               )
             )
           ))
    )
  })
  
# Restore Past Records- Load Record ----------------------------------------------------
  
# *** Select client record and restore a session or get all client records
  {
  # Following adds a blank row with same data.frame structure- used to initialize the whole record dt and to add a blank row at end
    df_row_maker <- function(){
      cols <- c(ui_demographics$inputId, sand$id) # all inputs
      data.frame(
        matrix(as.character(NA), ncol = length(cols))
      ) %>% setNames(., cols) %>% 
        mutate(across(ends_with("_date"), ~ as.Date(.x)))
    }
    
  # Will hold uploaded records- either all or one sheet
    all_records <- reactiveValues(
      df = df_row_maker()
    )
  }
  
  # Assigns whatever method files are input to all_records$df reactive
  {
  ## 1. Upload folder
    # Select a folder and upload all csv files in it
    ## To display all
    df_selected_upload <- eventReactive(input$fileIn, {
      inFiles <- input$fileIn
      df <- data.frame()
      if (is.null(inFiles))
        return(NULL)
      inFiles <- inFiles[grepl("_sand_data.csv$", inFiles$name),] # only those files from app
      for (i in seq_along(inFiles$datapath)) {
        tmp <- read.csv(inFiles$datapath[i][grepl("_sand_data.csv$", inFiles[,1][i])], header = T) %>%
        # Ensure only item responses
          dplyr::select(examinee_first_name:A3.Si) %>% 
        # Dates from excel
          mutate(
            birth_date = as.Date(birth_date, format = "%Y-%m-%d"),
            test_date = as.Date(test_date, format = "%Y-%m-%d")
          )
        df <- rbind.data.frame(df, tmp) 
      }
      df
    })
    observeEvent(input$fileIn, {
      all_records$df <- df_selected_upload()
    })
    
    ## 2. Upload multiple records
    observeEvent(input$uploadFile,{
      all_records$df <- whole_dataset()
    })
    
    ## 3. Upload one record
    observeEvent(input$upload_individual_file,{
      file <- input$upload_individual_file
    # Get the file extension to use appropriate read method or output warning
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      all_records$df <- read.csv(file$datapath) %>% 
        dplyr::select(examinee_first_name:A3.Si) %>% 
        mutate(
          birth_date = as.Date(birth_date, format = "%Y-%m-%d"),
          test_date = as.Date(test_date, format = "%Y-%m-%d")
        )
    })
  }
  
# Restore Record- Allow user to select and display selected, restore data for selected ------------------------
  
# Datatable with all the records uploaded in df_selected_upload function- this presents all possible records to user
  output$full_patient_record <- DT::renderDataTable({
    datatable(all_records$df,
              options = list(
                scrollX = TRUE
              ),
              rownames= FALSE
    ) %>% 
    # Put date in local format
      formatDate(c("birth_date", "test_date"),'toLocaleDateString')
  })
  
# Reactive values df selected_df$df that will hold records selected from all_records$df 
  # and be passed to selected records table
  selected_df <- reactiveValues(
    df = df_row_maker()
  )
  
  # Identifies records the user chooses from full list of records and assigns to selected_df$df
  observeEvent(input$full_patient_record_rows_selected, {
    req(any(!is.na(all_records$df)))
    if(!is.null(input$full_patient_record_rows_selected)){
    # This adds an extra blank row at end if the last row has any data- so there's always a blank row at end
      req(nrow(all_records$df[input$full_patient_record_rows_selected,]) > 0)
      selected_df$df <- rbind.data.frame(all_records$df[input$full_patient_record_rows_selected,],
                                         df_row_maker())
    }
  })
  
# Sends what user selects in uploaded table to the reactive value that holds selected records
  observeEvent(input$selected_patient,{
    out <-  hot_to_r (input$selected_patient) %>% 
      na_if(., "") %>% # 
      filter(if_any(everything(), ~ !is.na(.))) # Gets rid of blank row at end
    
  # This adds on a row if user enters data in final row
    selected_df$df <- if(
      out %>%   
      ungroup() %>%
      slice_tail(n = 1) %>%
      apply(., 2, function(value) !is.na(value)) %>% any()){
      rbind.data.frame(out, df_row_maker())
    } else {
      out
    }
  })
  
## Outputs selected records to selected records table
  output$selected_patient <- renderRHandsontable(
    rhandsontable(
      selected_df$df,
      selectCallback = TRUE,
      readOnly = FALSE,
      rowHeaders = "Click"
    )  %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  )
  
###*** Update inputs with selected record
  # Function that lists all update inputs that are used.
  {
    update_functions <- list(
      update_date_input <- function(vars, val) {
        date_value <- as.Date(val)
        updateDateInput(session, vars, value = date_value)
      },
      update_numeric_input <- function(vars, val) {
        updateNumericInput(session, vars, value = val)
      },
      update_text_input <- function(vars, val) {
        updateTextInput(session, vars, value = val)
      },
      update_text_area_input <- function(vars, val) {
        updateTextAreaInput(session, vars, value = val)
      },
      update_select_input <- function(vars, val) {
        updateSelectInput(session, vars, selected = val)
      },
      update_radio_buttons <- function(vars, val) {
        updateRadioButtons(session, vars, selected = val)
      }
    ) %>%
      setNames(c("dateInput", "numericInput", "textInput", "textAreaInput", "selectInput", "radioButtons"))
  }
  
## Gets inputId and value from selected past records, including if modified- used to update inputs and to hold data
  selected_record_input_id_value_func <- function(){
    out <-  hot_to_r (input$selected_patient) %>% 
      na_if(., "") %>% # 
      filter(if_any(everything(), ~ !is.na(.)))
    req(nrow(out[input$selected_patient_select$select$r, ] %>% # Returns selected row data
               filter(if_any(everything(), ~ !is.na(.)))) > 0) # Need data with inputs
    out[input$selected_patient_select$select$r, ] %>% # Returns selected row data
    # req(nrow(selected_df$df[input$selected_patient_select$select$r, ] %>%
    #            filter(if_any(everything(), ~ !is.na(.)))) > 0) # Need data with inputs
    # selected_df$df[input$selected_patient_select$select$r, ] %>% # Returns selected row data
      na_if(., "") %>% 
      t() %>%
      as_tibble(., rownames = "inputId") %>%
      setNames(., c("inputId", "value"))
  }
  
### Function to update all inputs. Ensures data is valid, applies update function
  update_inputs_conditional_filtered_func <- function(questionnaire_value = NA){
    out <- selected_record_input_id_value_func()
    # Get input ids and ui_type, then join with inputs (out)
    sand %>% 
    # Conditional filter, can assign questionnaire_value to be whatever tabs are selected
      filter(if(!is.na(questionnaire_value)) questionnaire == questionnaire_value else TRUE) %>% 
      dplyr::select(ui_type = item_type, inputId = id) %>%
      rbind.data.frame(., ui_demographics %>% dplyr::select(ui_type, inputId)) %>% # Gets information about fields
      inner_join(., out, by = "inputId") %>%
      filter(!is.na(value)) %>% 
      # Group by ui_type to apply ui_func
      group_by(ui_type) %>%
      nest() -> out
    
    # choose the update_ui functions from update_functions that match names of those in data
    update_functions <- update_functions[order(match(names(update_functions), out$ui_type))]
    
    # Go through each group of updatefunctions and apply each row of data toupdate
    lapply(1:length(out$data), function(i){
      out$data[[i]] %>%
        rowwise() %>%
        group_map(~ update_functions[[i]](vars = .$inputId, val = .$value))
    })
  }
  
  # Create reactive value for each tab "update_<tab_name>"
  # Reactive value that is 1 if restore_record button selected. This will keep from updating if user just selects
  # Interview tab without selecting restore_record (value = 0)
    # Name of column should be same as filter_condition in update_captured_data_function below
  update_tabs_reactive_counters <- reactiveValues(interview = 2,
                                                  observation = 2)
  
# Apply the update function to selected data on restore. Get all inputs and update
  observeEvent(input$restore_session, {
    update_inputs_conditional_filtered_func() # Update all inputs, no filter
    update_tabs_reactive_counters$interview <- 1 # sets to update interview tab items on that page view, list all reactives here
    update_tabs_reactive_counters$observation <- 1
    if(selected_df$df[input$selected_patient_select$select$r, ] %>%
       dplyr::summarize(if_all(everything(), ~ !is.na(.))) %>% pull){
       shinyjs::enable("generate_report") # Can download report if record valid
      out <- selected_record_input_id_value_func() %>% rename(input_value = value)
      # sand_inputs$df <- inner_join(sand_inputs$df, out, by = c("id" = "input_value")) %>% 
        # sand %>% 
        # dplyr::select(ui_type = item_type, inputId = id) %>%
        # rbind.data.frame(., ui_demographics %>% dplyr::select(ui_type, inputId)) %>% # Gets information about fields
        # inner_join(., out, by = "inputId")
      # Get just input values, not demographics. They are in sidebar so no need to update separately
      sand_inputs$df <- sand_inputs$df %>% 
        mutate(
          input_value = selected_record_input_id_value_func() %>% 
            anti_join(., ui_demographics, by = "inputId") %>% 
            dplyr::select(value) %>% pull,
          points = case_when(input_value == "No" ~ 0,
                             input_value == "Yes" ~ 1,
                             input_value == "Mild" ~ 1,
                             input_value == "Moderate-to-Severe" ~ 2
          )
        ) %>% 
        ungroup() 
    } else{
      shinyjs::disable("generate_report")
    }
  })
  
  # Apply the update function as individual tab items are selected. 
    # Tabs may not have rendered if tab was not selected before upload
    # So, need to hold all uploaded data, update that held data in tabs that are selected only, retaining
    # unselected data, otherwise that data will be updated as null. Then update inputs with that held data
    # Reactive values, filter condition, and tabs name are same- tolower
  observe({
      # Only run when active tab is one of form data
    req(tolower(input$tabs) %in% c("observation", "interview"))
    # If reactiveValue counter has been triggered by restore_session, then hold data and update inputs as each tab selected
      if(update_tabs_reactive_counters[[tolower(input$tabs)]] == 1){
      # Only update that tab ("filter_condition" is tab name)
        update_inputs_conditional_filtered_func(questionnaire_value = tolower(input$tabs))
        sand_inputs$df <- sand_inputs$df  %>%
        # Only update that section of data ("filter_condition" is tab name)
          filter(questionnaire == tolower(input$tabs)) %>%
          rowwise() %>%
          mutate(
            input_value = list(input[[id]]), # Prompted for this
            input_value = ifelse(is.null(unlist(input_value)), NA, input_value),
            points = case_when(input_value == "No" ~ 0,
                               input_value == "Yes" ~ 1,
                               input_value == "Mild" ~ 1,
                               input_value == "Moderate-to-Severe" ~ 2
            )
          ) %>%
          ungroup() %>%
          bind_rows(.,
                    sand_inputs$df %>%
                      filter(questionnaire != tolower(input$tabs))) # Bind with data from restore that hasn't updated
      }
    # Reset, so need to click restore_session button again.
      update_tabs_reactive_counters[[tolower(input$tabs)]]<- 0
  })

# Multiple Record File- Problem Values and Sum Scores ---------------------

###*** Calculate composite and download for multiple records
{ 
###*** Calculates composite raw scores for each scale
  composite_score_func <- reactive({
    selected_df$df %>% 
      filter(if_any(everything(), ~ !is.na(.))) %>%
      mutate(record = 1:nrow(.)) %>%
      dplyr::select(-(examinee_first_name:additional)) %>%
      # By row
      group_split(record) %>%
      map(~ .x %>%
            pivot_longer(-record, names_to = "scales", values_to = "values") %>%
            inner_join(., sand, by = c("scales" = "id")) %>%
            # This is hierarchy 1 items
            {. -> tmp
              bind_rows(
                # This gets hierarchy 2 items with parent items endorsed
                tmp %>%
                  filter(!is.na(parent_class)) %>%
                  rowwise() %>%
                  mutate(equal_to_critical = list(values == critical_value)) %>%
                  ungroup() %>%
                  group_by(parent_class) %>%
                  dplyr::summarize(
                    show_child = any(equal_to_critical == TRUE)
                  ) %>%
                  filter(show_child == TRUE) %>%
                  inner_join(., tmp, by = c("parent_class" = "child_class")) %>%
                  dplyr::select(item_number, parent_class, values),
                # Binds to hierarchy 1 items
                tmp %>%
                  filter(!is.na(parent_class)) %>%
                  dplyr::select(item_number, parent_class, values)
              )
            }    %>%
            # Responses to points
            mutate(
              points = case_when(values == "No" ~ 0,
                                 values == "Yes" ~ 1,
                                 values == "Mild" ~ 1,
                                 values == "Moderate-to-Severe" ~ 2
              )
            ) %>%
            dplyr::select(points, subscale = parent_class) %>%
            # Now we have the appropriate points for each record, will sum by case_when pattern to filter for scales
            {. -> tmp
              map(.x = case_when_pattern_list, .f = function(case_when_pattern){
                # Filter if the items meet conditions, then group by factors, then sum by groups, then pivot_wide
                tmp %>%
                  mutate(
                    sorting_var = case_when(!!! rlang::parse_exprs(case_when_pattern))
                  ) %>%
                  group_by(sorting_var) %>% # group by factors
                  dplyr::summarize(total = sum(points)) %>% # sum by groups
                  # give a column for each summed section
                  pivot_wider(names_from = sorting_var, values_from = total,
                              names_glue = "{sorting_var}_{.value}"
                  )
              })
            } %>%
            bind_cols # Put each group together for each record
      ) %>%
      bind_rows %>% # Put each record together
      bind_cols(selected_df$df %>% # Bind the summed records to end of input df set
                  filter(if_any(everything(), ~ !is.na(.))),.)
  })
  
## User can download multiple records. This will sum the records by scale and then download as .csv
  output$save_all_records <- downloadHandler(
    filename = function() {
      paste("full_record_", humanTime(), "_sand_wide_data.csv", sep = "")
    },
    content = function(file) {
      cd <- composite_score_func()
      write.csv(cd, file, row.names = F)
    }
  )
}
  
###*** Gets problem values, displays them, and enables buttons that depend on valid data
{
  # in selected data, finds rows with non-na, finds if they are in range of acceptable answers
  invalid_df_func <- reactive({
    req(selected_df$df)
    req(nrow(selected_df$df %>% 
               filter(if_any(everything(), ~ !is.na(.)))) > 0)
    selected_df$df %>% 
      filter(if_any(everything(), ~ !is.na(.))) %>% # Get only rows with values
      mutate(record = seq(1, nrow(.))) %>%  # Get record #
      dplyr::select(-(examinee_first_name:additional)) %>% # Only inputs, not demographics
      pivot_longer(-record, names_to = "scales", values_to = "values") %>% # scales is item
    # Gets acceptable responses for each id from master data
      inner_join(., sand %>% dplyr::select(id, response_1, response_2), by = c("scales" = "id")) %>% 
      rowwise() %>% 
    # If values in acceptable range, then output as values, otherwise flag
      mutate(validated = case_when(
        values %in% c(response_1, response_2) ~ values,
        TRUE ~ "problem_value"
      ), .keep = "unused") %>% # Keep all
      ungroup() %>% 
    # Put back in format by record, with values and a reviewed set of columns ("_validated")
      pivot_wider(id_cols = record, names_from = scales, 
                  values_from = c(validated), names_glue = "{scales}_validated") %>% 
      dplyr::select(-record)
  })
  
## Reactive value df to hold invalid records
  invalid_df <- reactive({ 
    req(invalid_df_func())
    tmp <- invalid_df_func() %>% 
      filter(if_any(ends_with("validated"), ~ .x == "problem_value"))
    if(nrow(tmp) > 0){
      dat <- tmp %>% 
        filter(if_any(ends_with("validated"), ~ .x == "problem_value"))
    } else {
      dat <- NULL
    }
    validate(
      need(!is.null(dat), "No invalid records")
    )
    return(dat)
  })
  
## Prints invalid data
  output$invalid_df <- renderDataTable(
    datatable(
      invalid_df(),
      caption = "Invalid Values",
      options = list(searching = FALSE,
                     paging = FALSE,
                     scrollX = TRUE),
      rownames = FALSE
    )
  )

## Checks if there are any invalid data before enabling save all records
  observeEvent(input$selected_patient,{
  # Checks if there are any invalid records
    invalid_df_func() %>% 
      filter(if_any(ends_with("validated"), ~ .x == "problem_value")) -> tmp
    if(nrow(tmp) == 0){ 
      shinyjs::enable("save_all_records")
    } else{
      shinyjs::disable("save_all_records")
    }
  })
  
## Checks if any invalid data before enabling restore session
  observeEvent(selected_df$df[input$selected_patient_select$select$r, ],{
    req(nrow(selected_df$df %>%
               filter(if_any(everything(), ~ !is.na(.)))) > 0)
    req(nrow(selected_df$df[input$selected_patient_select$select$r, ] %>%
               filter(if_any(everything(), ~ !is.na(.)))) > 0)
    invalid_df_func() %>% 
      filter(if_any(ends_with("validated"), ~ .x == "problem_value")) -> tmp
    if(nrow(tmp) == 0){ 
      shinyjs::enable("restore_session")
    } else{
      shinyjs::disable("restore_session")
    }
  })
}
  
## User can upload a file with multiple records
  whole_dataset<-eventReactive(input$uploadFile, {
    inFile <- input$uploadFile
    ext <- tools::file_ext(inFile$datapath)
    req(inFile)
    req(ext == "xls" | ext == "xlsx" | ext == "csv")
    dat<-if (ext == "xls" | ext == "xlsx") {
      read_excel(inFile$datapath, 1) %>% 
        dplyr::select(examinee_first_name:A3.Si) %>% 
        mutate(
          # across(everything(), as.character),
          birth_date = as.Date(birth_date, format = "%m/%d/%Y"),
          test_date = as.Date(test_date, format = "%m/%d/%Y")
        )
    } else if (ext == "csv") {
      read.csv(inFile$datapath) %>% 
        dplyr::select(examinee_first_name:A3.Si) %>% 
        mutate(
          birth_date = as.Date(birth_date, format = "%m/%d/%Y"),
          test_date = as.Date(test_date, format = "%m/%d/%Y")
        )
    } 
    all_records$df <- dat
    return(dat)
  })
} # Final server bracket

# Run the application
shinyApp(ui = ui, server = server)
