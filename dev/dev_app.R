# library(shiny)
# library(DT)
# library(pool)
# library(RSQLite)

mod_test_ui <- function(id) {
  tagList(
    mod_meshFilter_ui(NS(id, "mf"))
  )
}

mod_test_server <- function(id, pool) {
  moduleServer(
    id,
    function(input, output, session) {
      filter <- mod_meshFilter_server("mf", pool = pool)

      return(filter)
    }
  )
}

ui <- fluidPage(
  mod_test_ui("test")
)

server <- function(input, output, session) {
  pool <- reactive(dbPool(SQLite(), dbname = "../data/PGG_dev.db"))

  onSessionEnded(function() {
    isolate({
      poolClose(pool())
    })
  })

  mod <- mod_test_server("test", pool)
  observe({
    print(mod())
  })
}

shinyApp(ui, server)
