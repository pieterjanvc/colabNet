# library(shiny)
# library(DT)
# library(pool)
# library(RSQLite)

# nameUI <- function(id) {
#   tagList(
#     downloadButton(NS(id, "down"), "Download")
#   )
# }
#
# nameServer <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       output$down <- downloadHandler(
#         filename = function() {
#           x <- paste0("test", ".db")
#           print(x)
#           x
#         },
#         content = function(file) {
#           file.copy("C:/Users/pj/Desktop/test.db", file)
#         }
#       )
#     }
#   )
# }

ui <- fluidPage(
  # nameUI("test")
  textInput("query", "Enter key word"),
  DTOutput("matches")
)

server <- function(input, output, session) {
  # nameServer("test")
  pool <- dbPool(SQLite(), dbname = "../data/PGG_dev.db")

  onSessionEnded(function() {
    poolClose(pool)
  })

  # Get tree root info
  treeRoots <- tbl(pool, "meshTree") |>
    filter(treenum %in% LETTERS) |>
    left_join(tbl(pool, "meshLink"), by = "uid") |>
    left_join(tbl(pool, "meshTerm"), by = "meshui") |>
    collect() |>
    rename(root = treenum)

  # Table that shows search matches
  output$matches <- renderDT(
    {
      tibble(
        `MeSH Term` = character(),
        Tree = character(),
        Level = integer()
      )
    },
    rownames = F
  )

  matchesProxy <- dataTableProxy("matches")

  # Query the database based on key words entered
  searchMatches <- reactive({
    if (str_trim(input$query) == "") {
      return(tibble(
        meshterm = character(),
        root = character(),
        level = integer()
      ))
    }

    # Get the matching terms and their unique tree numbers ffrom the DB
    q <- dbGetQuery(
      pool,
      paste(
        "SELECT tr.treenum, mt.meshterm",
        "FROM meshTerm as mt, meshLink as ml, meshTree as tr",
        "WHERE mt.meshui = ml.meshui AND ml.uid = tr.uid AND mt.meshterm LIKE ?",
        "GROUP BY mt.meshui HAVING MIN(mt.ROWID) ORDER BY tr.treenum"
      ),
      params = list(sprintf("%%%s%%", input$query))
    )

    # Order by level (closer to root on top)
    q |>
      mutate(
        root = str_extract(treenum, "^."),
        level = str_count(treenum, "\\."),
        level = ifelse(nchar(treenum) == 1, level, level + 1)
      ) |>
      arrange(level, meshterm)
  })

  # Update the matches table using the proxy
  observeEvent(searchMatches(), {
    replaceData(
      matchesProxy,
      searchMatches() |>
        # left_join(
        #   treeRoots |> select(root, Tree = root),
        #   by = "root"
        # ) |>
        select(`MeSH Term` = meshterm, Tree = root, Level = level),
      rownames = F
    )
  })
}

shinyApp(ui, server)
