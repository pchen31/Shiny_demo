library(shiny)
library(shinyWidgets)
library(datamods)
library(DT)
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)
library(openxlsx)

# Create variables ----
val_app_title <- "My first Shiny app!"                 # title of the shiny app
  
val_sysDate <- Sys.Date()                              # get the current date         
val_fileName <- "Data and Plot"                        # file name for excel

val_p_title <- "Intrinsic Clearance (Hepatocytes)"     # plot title
val_p_xlab <- "Month"                                  # plot x-axis label
val_p_ylab <- "Clint (μL/min/million cells)"           # plot y-axis label
val_p_w <- 30                                          # plot width for export
val_p_h <- 15   

df_example <- read.csv("DataVis_Example.csv")          # example

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
      h5(HTML("<b>Data source</b>")),
      # *button: import data ----
      actionBttn(
        inputId = "launch_modal",
        label = "Import",
        style = "simple", 
        color = "primary",
        size = "sm",
        block = TRUE
      ),
      # *an empty line between the two buttons ----
      br(),
      # *button: load example ----
      actionBttn(
        inputId = "example",
        label = "Example",
        style = "simple", 
        color = "success",
        size = "sm",
        block = TRUE
      ),
      # *a solid line as separator ----
      tags$hr(),
      # *slider: page length ----
      sliderInput(
        inputId = "pageLen",
        label = "Number of entries per page",
        min = 5,
        max = 50, 
        value = 20,
        step = 1,
      )
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      width = 9,
      fluidPage(
        DTOutput("dt_rawdata")
      )
    )
  )
)

# Server section ----
server <- function(input, output, session) {
  
  # import using datamods ----
  observeEvent(input$launch_modal, {
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
    rawdata = data.frame()
  )
  
  # observeEvent based on inputs ----
  observeEvent(imported$data(), {
    data_rv$rawdata <- imported$data()
  })
  
  observeEvent(input$example, {
    data_rv$rawdata <- df_example
  })
  
  # render rawdata in a table
  output$dt_rawdata <- renderDT({
    datatable(data_rv$rawdata,
              extensions = c("ColReorder", "FixedColumns", "KeyTable"),
              options = list(
                colReorder = TRUE,
                scrollX = TRUE,
                fixedColumns = TRUE,
                keys = TRUE,
                dom = "ftp",
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
}

# App ----
shinyApp(ui, server)
