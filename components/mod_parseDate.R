# load libraries ----
library(lubridate, warn.conflicts = FALSE)

# import component(s) ----
source("./components/variables.R")

# UI ----
parseDate_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_parse_date"))
  )
}

# Server ----
parseDate_Server <- function(id, data) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      # Initialize data_rv ----
      data_rv <- reactiveValues(
        data = data.frame(),
        colN = "",
        str_colN = ""
      )
      
      # update data_rv values ----
      observeEvent(data(), {
        data_rv$data <- data()
        data_rv$colN <- names(data())
        data_rv$str_colN <- str_which(names(data()), colN_keyword)
      })
      
      # renderUI: ui_parse_date ----
      output$ui_parse_date <- renderUI({
        req(nrow(data_rv$data) > 0 && length(data_rv$colN) > 0)
        
        list(
          line_sep,
          h5(HTML("<b>EXPERIMENT DATE</b>")),
          pickerInput(
            inputId = ns("colN_exp"),
            label = "Column name",
            choices = data_rv$colN[data_rv$str_colN]
          ),
          radioGroupButtons(
            inputId = ns("date_format"),
            label = "Date format",
            choices = c("ymd", "mdy", "dmy"),
            justified = TRUE
          ),
          actionBttn(
            inputId = ns("btn_parse_date"),
            label = "Parse dates",
            style = btn_style, 
            color = "primary",
            size = "sm",
            block = TRUE
          )
        )   
      })
      
      # parse experiment dates ----
      observeEvent(input$btn_parse_date, {
        req(length(input$date_format) > 0)
        
        # get function for parse date 
        fun_date_format <- function(x) {
          if (input$date_format == "ymd") {
            ymd(x)
          } else if (input$date_format == "mdy") {
            mdy(x)
          } else if (input$date_format == "dmy") {
            dmy(x)
          }
        }
        
        # replicate experiment date column if the name is not "Exp_Date" 
        if (!"Exp_Date" %in% data_rv$colN) {
          data_rv$data <- data_rv$data %>% mutate("Exp_Date" = .data[[input$colN_exp]])
        }
        
        # convert column to the correct date format
        data_rv$data["Exp_Date"] <- lapply(data_rv$data["Exp_Date"], fun_date_format)
        
        # filter out N/A values and parse dates
        data_rv$data <- data_rv$data %>% filter(!is.na(Exp_Date)) %>%
          mutate(Exp_Yr = year(Exp_Date),
                 Exp_Qt = quarter(Exp_Date),
                 Exp_Mth = month(Exp_Date, label = TRUE),
                 Exp_Yr_Qt = quarter(Exp_Date, with_year = TRUE))
      })
      
      # return value
      return(reactive(data_rv$data))
      
    })
}
