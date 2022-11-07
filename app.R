library(shiny)
library(shinyWidgets)
library(datamods)
library(DT)
library(tidyverse)
library(lubridate, warn.conflicts = FALSE)

# import modules
source("./components/variables.R")
source("./components/functions.R")

source("./components/mod_importData.R")
source("./components/mod_dataTable.R")
source("./components/mod_parseDate.R")


# UI section ----
ui <- fluidPage(
  # App title ----
  titlePanel(val_app_title),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
      importData_UI("import_data"),
      parseDate_UI("parse_date")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          title = "Table",
          dataTable_UI("dt")
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
  
  # Initialized data_rv ----
  data_rv <- reactiveValues(
    data = data.frame()
  )
  
  # Import data ----
  rawdata <- importData_Server("import_data")

  # update data_rv$data
  observeEvent(rawdata$data, {
    data_rv$data <- rawdata$data
  })
  
  # Parse date ----
  parsed_data <- parseDate_Server("parse_date", data_rv$data)
  
  # update data_rv$data
  observeEvent(parsed_data, {
    data_rv$data <- parsed_data
  })
  
  # Data table ----
  dataTable_Server("dt", dt_style_1, data_rv$data, rawdata$pageLen)
}

# App ----
shinyApp(ui, server)