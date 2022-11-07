source("./components/functions.R")

dataTable_UI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("dt"))
  )
}

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