library(shiny)
library(dplyr)
library(stringr)
library(RSQLite)

localFolder <- file.path("..", "data")
tempFolder <- file.path("shinyData", "tempDB")
schema <- system.file("create_colabNetDB.sql", package = "colabNet")

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

  dbPath <- "../local/dev.db"
  dir(normalizePath(dbPath))

  test <- mod_dbSetup_server("getDB", localFolder, tempFolder, schema, dbPath)
}

shinyApp(ui, server)
