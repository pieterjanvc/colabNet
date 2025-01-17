#' Run the colabNet Shiny App
#'
#' @import shiny dplyr stringr tidyr purrr visNetwork pool plotly
#' @importFrom RSQLite SQLite
#' @importFrom DT DTOutput renderDT datatable
#'
#' @return Start the Shiny app
#' @export
#'
colabNet_v2 <- function() {

  # ///////////////
  # ---- DATA ----
  # //////////////
  
  # ColabNet Database
  colabNetDB = "dev/colabNet.db"

  # Setup for functions in the package
  dbSetup(colabNetDB)

  # Pool for the Shiny app
  pool <- dbPool(SQLite(), dbname = colabNetDB)
  onStop(function() {
    poolClose(pool)
  })

  # Get the full diffTree
  print("Building difftree ...")
  auIDs <- tbl(pool, "author") |> 
    # TODO remove 165 exclusion once bug fixed!!
    filter(authorOfInterest == 1, auID != 163) |> 
    pull(auID)
  difftree <- diffTree(auIDs, pruneDuplicates = T)
  print("... finished building difftree")
  
  # //////////////
  # ---- UI ----
  # /////////////

  ui <- fluidPage(
    fluidRow(    
      fluidRow(
        column(12,p("Add filters ..."))
      ),
      fluidRow(column(12,
        tabsetPanel(
          tabPanel("Collaboration Network",
            visNetworkOutput("networkPlot", height = "60vh"),
            value="networkTab"),
            tabPanel("Collaboration MeSH Tree",
            plotlyOutput("meshTreePlot", height = "60vh"),
            value="networkTab")
        )
      )),
      fluidRow(column(12,
        DTOutput("articleTable")
      ))
    ),
    fluidRow(column(12,
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
    ))
  )


  # //////////////////
  # ---- SERVER ----
  # /////////////////

  server <- function(input, output, session) {

    # ---- Colab Network ----
    # ///////////////////////
    output$networkPlot <- renderVisNetwork({
      nodes <- data.frame(id = 1:5, 
        label = c("Node 1", "Node 2", "Node 3", "Node 4", "Node 5"))
      edges <- data.frame(from = c(1, 1, 2, 3, 4), 
        to = c(2, 3, 4, 5, 5))

      # Create a simple visNetwork graph
      visNetwork(nodes, edges) %>%
      visOptions(manipulation = TRUE) %>%
      visNodes(size = 30, color = list(background = "lightblue", border = "blue")) %>%
      visEdges(smooth = FALSE)
    })

    # ---- Colab MeSH Tree ----
    # //////////////////////////
    output$meshTreePlot <- renderPlotly({
      plotDiffTree(difftree)
    })
   
    # ---- Articles Table ----
    # ////////////////////////
    output$articleTable <- renderDT({
      tbl(pool, "coAuthor") |> filter(auID %in% local(auIDs)) |> distinct() |> 
        left_join(tbl(pool, "article"), by = "arID") |> 
        left_join(tbl(pool, "authorName") |> 
          group_by(auID) |> filter(default) |> ungroup(), by = "auID") |> 
          collect() |> select(PMID, lastName, month, year, title, journal) |>
          arrange(desc(PMID))
    },rownames = FALSE)
  }

  shinyApp(ui, server)
}
