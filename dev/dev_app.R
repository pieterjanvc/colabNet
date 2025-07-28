library(shiny)

nameUI <- function(id) {
  tagList(
    downloadButton(NS(id, "down"), "Download")
  )
}

nameServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$down <- downloadHandler(
        filename = function() {
          x <- paste0("test", ".db")
          print(x)
          x
        },
        content = function(file) {
          file.copy("C:/Users/pj/Desktop/test.db", file)
        }
      )
    }
  )
}

ui <- fluidPage(
  nameUI("test")
)

server <- function(input, output, session) {
  nameServer("test")
}

shinyApp(ui, server)
