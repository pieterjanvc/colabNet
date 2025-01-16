#' Run the colabNet Shiny App
#'
#' @import shiny dplyr stringr tidyr purrr visNetwork DT
#'
#' @return Start the Shiny app
#' @export
#'
colabNet_v2 <- function() {

  # ///////////////
  # ---- DATA ----
  # //////////////
  
  # //////////////
  # ---- UI ----
  # /////////////

  ui <- fluidPage(
    fluidRow(      
    ),
    br(),
    tags$footer(
      p(
        "This app was cretaed to support the Harvard Medical School BBS",
        tags$a("Program in Genetics and Genomics",
          href = "https://projects.iq.harvard.edu/pgg", target = "_blank"
        )
      ),
      p(
        "Content manager: Lorenzo Gesuita -",
        tags$a("lorenzo_gesuita@hms.harvard.edu", href = "mailto:lorenzo_gesuita@hms.harvard.edu"),
        "| App creator: PJ van Camp -",
        tags$a("pjvancamp@hms.harvard.edu", href = "mailto:pjvancamp@hms.harvard.edu")
      ),
      style = "width: 100%;margin: auto;text-align: center;background-color: #f6f6f6;
    color:#787878;border-top: 0.2rem solid;"
    )
  )


  # //////////////////
  # ---- SERVER ----
  # /////////////////

  server <- function(input, output, session) {

  }

  shinyApp(ui, server)
}
