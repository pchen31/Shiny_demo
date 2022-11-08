# Import component(s) ----
source("./components/functions.R")

# UI ----
dataTable_UI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("dt"))
  )
}

# Server ----
dataTable_Server <- function(id, fun_dt, data, pageLen) {
  moduleServer(
    id, 
    function(input, output, session) {
    ns <- session$ns
    
    output$dt <- renderDT({
      fun_dt(data(), pageLen())
    })

  })
}