library(shiny)
library(dplyr)
library(stringr)
library(RSQLite)

localFolder <- file.path("..", "data")
tempFolder <- file.path("shinyData", "tempDB")

ui <- fluidPage()

server <- function(input, output, session) {
  mod_dbConnect_ui("getDB")
  test <- mod_dbConnect_server("getDB", localFolder, tempFolder)

  observe(print(test()))
}

shinyApp(ui, server)
