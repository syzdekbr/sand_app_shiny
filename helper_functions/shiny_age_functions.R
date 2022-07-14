### Calculate ages from date to date and output in table
# Needs input$birth_date, input$test_date, output$age_out table- source in server.R

output$age_out<-renderTable({
  req(credentials()$user_auth)
  validate(
    need(input$test_date > input$birth_date, "Test date must be after birth date")
  )
  ager<-as.data.frame(age_out_function(), stringsAsFactors=F)
  colnames(ager)<-c("Examinee Age at Testing")
  ager
})

age_out_function<-function(){
  0
  if (length(input$birth_date) > 0 & !is.null(input$birth_date) & !is.null(input$test_date) & length(input$test_date) > 0){
    years<- floor(age_calc(input$birth_date, enddate = input$test_date, units = "years")) #ifelse(as.numeric(as.double(dates())) %% 365.25 < 4, ceiling(as.numeric(as.double(dates(), units="weeks")/52.25)),
    #    round(as.numeric(as.double(dates(), units="weeks")/52.25),2))
    months<- floor(age_calc(input$birth_date, enddate = input$test_date, units = "months") %% 12) # round((years - as.integer(years))*12)
    if (years <= 0 | years > 200){
      "Please enter a valid age"
    }
    else{
      sprintf("%s years and %s months", trunc(years), months)
    }
  } else {
    "Enter Valid Age"
  }
}

age_decimal <- function(birth_date, test_date){
  0
  req(birth_date)
  req(test_date)
  months<- floor(age_calc(birth_date, enddate = test_date, units = "months"))
  months/12
}
