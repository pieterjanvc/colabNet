# library(shiny)
# library(DT)
# library(pool)
# library(RSQLite)

library(sqlife)

ui <- fluidPage(
  sqlife::mod_dbSetup_ui("cnDB")
)

server <- function(input, output, session) {
  pool <- reactive(dbPool(SQLite(), dbname = "../data/PGG_dev.db"))

  onSessionEnded(function() {
    isolate({
      poolClose(pool())
    })
  })

  connInfo <- sqlife::mod_dbSetup_server(
    id = "cnDB",
    localFolder = "../data/",
    tempFolder = "../temp/",
    schema = system.file("create_colabNetDB.sql", package = "colabNet"),
    useDB = NULL
  )

  observe({
    connInfo() |> print()
  })
}

shinyApp(ui, server)
