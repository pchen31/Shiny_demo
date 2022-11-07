source("./components/variables.R")

# UI ----
importData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h5(HTML("<b>DATA SOURCE</b>")),
    # *button: import data ----
    actionBttn(
      inputId = ns("btn_launch_modal"),
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
      inputId = ns("btn_example"),
      label = "Example",
      style = btn_style, 
      color = "default",
      size = "sm",
      block = TRUE
    ),
    br(),
    # *slider: page length ----
    sliderInput(
      inputId = ns("pageLen"),
      label = "Number of entries per page",
      min = 5,
      max = 50, 
      value = 20,
      step = 1
    )
  )
}

# Server ----
importData_Server <- function(id, data) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      # import using datamods ----
      observeEvent(input$btn_launch_modal, {
        import_modal(
          id = ns("rawdata"),
          from = c("file", "copypaste"),
          title = "Import"
        )
      })
      
      imported <- import_server(
        "rawdata",
        return_class = c("data.frame", "data.table", "tbl_df")
      )
      
      # Initialize reactiveVal 'rawdata' ----
      rawdata <- reactiveVal(data.frame())
      
      # ObserveEvent based on input ----
      observeEvent(imported$data(), {
        rawdata(imported$data())
      })
      
      observeEvent(input$btn_example, {
        rawdata(df_example)
      })
      
      # return reactive value
      return(
        list(
          data = rawdata,
          pageLen = reactive({ input$pageLen })
        )
      )
    })
}