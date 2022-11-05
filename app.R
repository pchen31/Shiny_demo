library(shiny)
library(shinyWidgets)
library(datamods)
library(DT)
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(openxlsx)
library(esquisse)

# shinyWidgets::shinyWidgetsGallery()

# Create variables ----
val_app_title <- "My first Shiny app!"                 # title of the shiny app
df_example <- read.csv("DataVis_Example.csv")          # import csv file
line_sep <- tags$hr(style = "border-color: grey")      # a solid line as separator
btn_style <- "gradient"                                # button style
keyword_colN_exp <- "(D|d)ate"                             # Keyword in experiment date column

# export
val_fileName <- "Data"                        # file name for excel

val_p_title <- "Intrinsic Clearance (Hepatocytes)"     # plot title
val_p_xlab <- "Month"                                  # plot x-axis label
val_p_ylab <- "Clint (μL/min/million cells)"           # plot y-axis label
val_p_w <- 30                                          # plot width for export
val_p_h <- 15   

val_sysDate <- Sys.Date()                              # get the current date 

# UI section ----
ui <- fluidPage(
  # App title ----
  titlePanel(val_app_title),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
      # *label: data source ----
      h5(HTML("<b>DATA SOURCE</b>")),
      # *button: import data ----
      actionBttn(
        inputId = "btn_launch_modal",
        label = "Import",
        style = btn_style, 
        color = "primary",
        size = "sm",
        block = TRUE
      ),
      # an empty line
      br(),
      # *button: load example ----
      actionBttn(
        inputId = "btn_example",
        label = "Example",
        style = btn_style, 
        color = "default",
        size = "sm",
        block = TRUE
      ),
      br(),
      # *slider: page length ----
      sliderInput(
        inputId = "pageLen",
        label = "Number of entries per page",
        min = 5,
        max = 50, 
        value = 20,
        step = 1
      ),
      uiOutput("ui_btn_rawdata"),
      # an empty line
      uiOutput("ui_parse_date"),
      uiOutput("ui_download")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          title = "Table",
          DTOutput("dt"),
        ),
        tabPanel(
          title = "Plot"
        )
      )
    )
  )
)

# Server section ----
server <- function(input, output, session) {
  
  # import using datamods ----
  observeEvent(input$btn_launch_modal, {
    import_modal(
      id = "rawdata",
      from = c("file", "copypaste"),
      title = "Import"
    )
  })
  
  imported <- import_server(
    "rawdata",
    return_class = c("data.frame", "data.table", "tbl_df")
  )
  
  # initialize reactive values ----
  data_rv <- reactiveValues(
    rawdata = data.frame(),
    data = data.frame(),
    preview = data.frame(),
    pivot_cols = "",
    pivot_names_to = "",
    pivot_names_prefix = "",
    pivot_values_to = "",
  )
  
  # observeEvent based on inputs ----
  observeEvent(imported$data(), {
    data_rv$rawdata <- imported$data()
  })
  
  observeEvent(input$btn_example, {
    data_rv$rawdata <- df_example
  })
  
  # renderUI: btn_rawdata ----
  output$ui_btn_rawdata <- renderUI({
    req(nrow(data_rv$rawdata) > 0)
    
    list(
      line_sep,
      actionBttn(
        inputId = "btn_rawdata",
        label = "Revert back to rawdata",
        style = btn_style, 
        color = "danger",
        size = "sm",
        block = TRUE
      )
    )
  })
  
  # update data_rv$data ----
  observeEvent(c(data_rv$rawdata, input$btn_rawdata), {
    data_rv$data <- data_rv$rawdata
  })
  
  
  # renderDT: dt ----
  output$dt <- renderDT({
    datatable(data_rv$data,
              filter = "top",
              extensions = c("ColReorder", "FixedColumns", "KeyTable"),
              options = list(
                colReorder = TRUE,
                scrollX = TRUE,
                fixedColumns = TRUE,
                keys = TRUE,
                dom = "Bfrtip",
                pageLength = input$pageLen,
                columnDefs = list(
                  list(
                    className = "dt-center", 
                    width = 120, 
                    targets = "_all"
                  )
                )
              ),
              selection = "none",
              rownames = FALSE,
              class = 'cell-border stripe'
    )
  })
  
  # renderUI: ui_parse_date ----
  output$ui_parse_date <- renderUI({
    req(nrow(data_rv$rawdata) > 0)
    
    # get column names of data_rv$rawdata
    colN_rawdata <- names(data_rv$rawdata)
    
    # get index of columns containing keyword "Date"
    strW_colN_exp <- str_which(colN_rawdata, keyword_colN_exp)
    
    if (length(strW_colN_exp) > 0) {
      list(
        line_sep,
        h5(HTML("<b>EXPERIMENT DATE</b>")),
        pickerInput(
          inputId = "colN_exp",
          label = "Column name",
          choices = colN_rawdata[strW_colN_exp]
        ),
        radioGroupButtons(
          inputId = "date_format",
          label = "Date format",
          choices = c("ymd", "mdy", "dmy"),
          justified = TRUE
        ),
        actionBttn(
          inputId = "btn_parse_date",
          label = "Parse dates",
          style = btn_style, 
          color = "primary",
          size = "sm",
          block = TRUE
        )
      )   
    }
  })
  
  # parse experiment dates ----
  observeEvent(input$btn_parse_date, {
    req(length(input$date_format) > 0)
    
    # create a new function to get parse date function
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
    if (!"Exp_Date" %in% names(data_rv$data)) {
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
  
  # renderUI: ui_download
  output$ui_download <- renderUI({
    req(nrow(data_rv$data) > 0)
    
    list(
      line_sep,
      h5(HTML("<b>EXPORT DATA</b>")),
      downloadBttn(
        outputId = "btn_download",
        style = btn_style,
        color = "success",
        size = "sm",
        block = TRUE    
      )
    )
  })
  
  # observeEvent: btn_download
  output$btn_download <- downloadHandler(
    filename = function() {
      paste(val_sysDate, " ", val_fileName, ".csv", sep = "")
    },
    content = function(con) {
      write.csv(data_rv$data, con)
    }
  )
  
}

# App ----
shinyApp(ui, server)