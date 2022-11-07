# app ----
val_app_title <- "My first Shiny app!"                 # title of the shiny app
line_sep <- tags$hr(style = "border-color: grey")      # a solid line as separator
btn_style <- "gradient"                                # button style

# mod_importData ----
df_example <- read.csv("DataVis_Example.csv")          # import csv file

# mod_parseDate ----
colN_keyword <- "(D|d)ate"                             # Keyword in experiment date column

# mod_download ----
val_fileName <- "Data"                                 # file name for excel
val_sysDate <- Sys.Date()                              # get the current date 