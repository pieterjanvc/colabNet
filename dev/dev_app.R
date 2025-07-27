library(shiny)
library(dplyr)
library(stringr)
library(RSQLite)

localFolder <- file.path("..", "data")
tempFolder <- file.path("shinyData", "tempDB")

ui <- fluidPage(
  actionButton("btn", "Test")
)

server <- function(input, output, session) {
  # observe({
  #   if (!"dbmode" %in% names(isolate(getQueryString()))) {
  #     mod_dbConnect_ui("getDB")
  #   } else {
  #     print(getQueryString())
  #   }
  # })

  test <- mod_dbConnect_server("getDB", localFolder, tempFolder)
}

shinyApp(ui, server)
