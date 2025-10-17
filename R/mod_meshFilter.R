#' MeSH filtering module - UI
#' This module allows the user to search and select specific MeSH terms
#' from the current database
#'
#' @param id Module id, needs to be the same as the server function
#'
#' @returns UI for the module
#' @export
mod_meshFilter_ui <- function(id) {
  tagList(
    actionButton(NS(id, "create"), "Create MeSH filter"),
    actionButton(NS(id, "reset"), "Reset filter"),
    uiOutput(NS(id, "meshList"))
  )
}

#' MeSH filtering module - Server
#' This module allows the user to search and select specific MeSH terms
#' from the current database
#'
#' @param id Module id, needs to be the same as the UI function
#' @param pool A reactive pool object as DB connection
#'
#' @returns Server of the module that returns a reactive dataframe of MeSH terms
#' the user has selected
#' @export
mod_meshFilter_server <- function(id, pool) {
  moduleServer(
    id,
    function(input, output, session) {
      # show a modal pop-up window when the create MeSh filter button is clicked
      observeEvent(input$create, {
        # We need to get the session namespace given we're inside a module server
        ns <- session$ns
        # Modal UI
        showModal(modalDialog(
          fluidRow(
            column(
              8,
              tags$h3("1. Search MeSH terms"),
              textInput(ns("query"), "Enter key word"),
              tags$i(
                "Note that for each term selected all of its children",
                "(not shown here if no exact match) will also be included"
              ),
              DTOutput(ns("matches")),
              br(),
              actionButton(ns("add"), "Add selected rows")
            ),
            column(
              4,
              tags$h3("2. Selected terms"),
              tags$i(
                "Each selected term and its children will be shown in the tree"
              ),
              br(),
              DTOutput(ns("selection")),
              actionButton(ns("remove"), "Remove selected rows")
            )
          ),
          footer = list(
            actionButton(
              ns("confirm"),
              "CONFIRM SELECTION",
              style = "background:#4AD391;color:white;"
            ),
            modalButton("Cancel")
          ),
          size = "l"
        ))
      })

      # Get tree root info
      # treeRoots <- tbl(isolate(pool()), "meshTree") |>
      #   filter(treenum %in% LETTERS) |>
      #   left_join(tbl(pool(), "meshLink"), by = "uid") |>
      #   left_join(tbl(pool(), "meshTerm"), by = "meshui") |>
      #   collect() |>
      #   rename(root = treenum)

      # Table that shows search matches
      output$matches <- renderDT(
        {
          tibble(
            `MeSH Term` = character(),
            Tree = character(),
            Level = integer()
          )
        },
        rownames = F,
        options = list(dom = "tpi")
      )

      matchesProxy <- dataTableProxy("matches")

      # Query the database based on key words entered
      searchMatches <- reactive({
        # Empty table when start or no query words
        if (is.null(input$query) || str_trim(input$query) == "") {
          return(tibble(
            meshterm = character(),
            treenum = character(),
            level = integer()
          ))
        }

        # Get the matching terms and their unique tree numbers ffrom the DB
        q <- dbGetQuery(
          pool(),
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
            select(`MeSH Term` = meshterm, Tree = treenum, Level = level),
          rownames = F
        )
      })

      # Keep track of all selected terms
      selectedTerms <- reactiveVal()

      # Table that shows search matches
      output$selection <- renderDT(
        {
          tibble(
            `MeSH Term` = character()
          )
        },
        rownames = F,
        options = list(dom = "tpi")
      )

      selectionProxy <- dataTableProxy("selection")

      # When terms are added from the search by clicking the add button
      observeEvent(input$add, {
        req(length(input$matches_rows_selected) > 0)
        sel <- searchMatches()[input$matches_rows_selected, ]
        allSel <- rbind(
          selectedTerms(),
          sel |> filter(!treenum %in% selectedTerms()$treenum)
        ) |>
          arrange(meshterm)
        # Update the table with all selected MeSH terms
        replaceData(
          selectionProxy,
          allSel |> select(`MeSH Term` = meshterm),
          rownames = F
        )

        selectedTerms(allSel)
      })

      # When terms are removed from the selection by clicking the remove button
      observeEvent(input$remove, {
        req(length(input$selection_rows_selected) > 0)
        toKeep <- selectedTerms()[-input$selection_rows_selected, ]
        # Update the table with all selected MeSH terms
        replaceData(
          selectionProxy,
          toKeep |> select(`MeSH Term` = meshterm),
          rownames = F
        )

        selectedTerms(toKeep)
      })

      meshlist <- reactiveVal()
      # When the confirm button is clicked
      observeEvent(input$confirm, {
        input$query
        removeModal()
        meshlist(selectedTerms())
      })

      # When the reset button is clicked
      observeEvent(input$reset, {
        removeModal()
        meshlist(NULL)
      })

      output$meshList <- renderUI({
        if (length(meshlist()$meshterm) > 0) {
          tags$i(sprintf(
            "MeSH Filters: %s",
            paste(meshlist()$meshterm, collapse = "; ")
          ))
        } else {
          tags$i("No active MeSH filter")
        }
      })

      # Return the selected mesh values
      return(meshlist)
    }
  )
}
