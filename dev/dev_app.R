# library(shiny)
# library(DT)
# library(RSQLite)

library(sqlife)

ui <- fluidPage(
  actionButton("btn", "Click")
)

server <- function(input, output, session) {
  observeEvent(input$btn, {
    print(environment())
  })
}

shinyApp(ui, server)
