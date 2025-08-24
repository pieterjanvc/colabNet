#' MeSH Tree Shiny Module - UI
#'
#' @param id ID for the module
#' @param limits Default = c(12, 10, 8, 6, 4). Values in the limits dropdown
#' menu. A default value for all will always be added
#' @param plotHeight Default = "60vh". CSS unit defining the height of the treemap
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#'
#' @returns UI for MeSH Tree
#' @export
#'
mod_meshTree_ui <- function(
  id,
  limits = c(12, 10, 8, 6, 4),
  plotHeight = "60vh"
) {
  tagList(
    fluidRow(
      column(
        6,
        selectInput(
          NS(id, "mtLimit"),
          "Limit to n-top scoring categories",
          choices = setNames(
            c(0, limits),
            c("All", as.character(limits))
          )
        ),
        mod_meshFilter_ui(NS(id, "meshfilter"))
      ),
      column(
        6,
        checkboxInput(NS(id, "mtBalance"), "Set equal rectangle size")
      )
    ),
    plotlyOutput(NS(id, "mtPlot"), height = plotHeight)
  )
}

#' MeSH Tree Shiny Module - Server
#'
#' @param id ID for the module
#' @param plotData A MeshTree plot data data frame
#' @param pool A reactive pool object as DB connection
#'
#' @returns Server function for MeSH Tree. The output will return a reactive list:
#' - mtrIDs: the mtrIDs currently in the treemap (based on filtering),
#' - selected: the currently selected branch,
#' - auIDs: the current author IDs
#'
#' @import shiny dplyr plotly
#'
#' @export
#'
mod_meshTree_server <- function(id, papermeshtree, pool) {
  # Filter treemap plot data based on limits
  plotDataFilter <- function(plotData, n) {
    n = as.integer(n)

    if (n > 0) {
      # Only get the top scoring n branches per level
      plotData <- plotData |>
        group_by(parentBranchID) |>
        slice_max(meshSum, n = n, na_rm = F) |>
        ungroup()

      # Trim dead branches (parent removed)
      n <- 0
      while (nrow(plotData) != n) {
        n <- nrow(plotData)
        plotData <- plotData |>
          filter(parentBranchID %in% branchID | is.na(parentBranchID))
      }
    }

    return(plotData)
  }

  moduleServer(id, function(input, output, session) {
    # Keep track of which branch is being viewed (also returned)
    selectedBranch <- reactiveVal()

    meshfilter <- mod_meshFilter_server("meshfilter", pool)

    data <- reactive({
      # Apply mesh filter if needed
      if (length(meshfilter()) > 0) {
        # Annotate all MeSH terms and children in the selection
        # filterlvl 2: MeSH term or child
        # filterlvl 1: Branches above selected needed to go to root
        # filterlvl 0: not in selection
        pmt <- papermeshtree() |>
          select(-c(branchID, parentBranchID, treemapVal)) |>
          mutate(
            filterlvl = str_detect(
              treenum,
              sprintf("^(%s)", paste(meshfilter()$treenum, collapse = "|"))
            ),
            filterlvl = ifelse(filterlvl == 1, 2, 0),
            filterlvl = ifelse(
              treenum %in%
                missingTreeNums(
                  meshfilter()$treenum,
                  includeRoots = T,
                  includeOriginal = T
                ) &
                filterlvl == 0,
              1,
              filterlvl
            )
          ) |>
          filter(filterlvl > 0)

        pmt <- pmt |>
          mutate(
            auID = ifelse(filterlvl == 1, 0, auID),
            nPapers = ifelse(filterlvl == 1, 0, nPapers),
            name = ifelse(filterlvl == 1, "", name)
          ) |>
          distinct() |>
          select(-c(filterlvl))

        # Recalculate the branch and parent branch IDs(to collapse later)
        # TODO Consider parsing this function out later as it's used in treemap generator too
        newBranches <- branchID(
          pmt$mtrID[pmt$level > 1],
          pmt$parent[pmt$level > 1]
        )

        pmt <- pmt |>
          left_join(
            data.frame(
              mtrID = as.integer(names(newBranches$branchID)),
              branchID = as.integer(unname(newBranches$branchID)),
              treemapVal = unname(newBranches$treemapVal)
            ),
            by = "mtrID"
          )

        pmt <- pmt |>
          arrange(treenum) |>
          group_by(branchID) |>
          mutate(link = parent[1]) |>
          ungroup() |>
          left_join(
            pmt |>
              select(link = mtrID, parentBranchID = branchID) |>
              distinct(),
            by = "link"
          ) |>
          select(-link) |>
          mutate(
            parentBranchID = ifelse(
              branchID == parentBranchID,
              NA,
              parentBranchID
            )
          )
      } else {
        pmt <- papermeshtree()
      }

      # Get the author IDs
      auIDs <- pmt$auID |> unique()
      auIDs <- auIDs[auIDs != 0]

      list(
        auIDs = auIDs,
        plotData = treemapData(pmt),
        branchInfo = pmt |> select(branchID, mtrID) |> distinct()
      )
    })

    # Filter data as needed
    mtPlotData <- reactive({
      plotData <- data()$plotData

      # Filter mtrIDs by limits if needed
      if (input$mtLimit > 0) {
        plotData <- plotData |> plotDataFilter(input$mtLimit)
        branchIDs <- plotData$branchID
        mtrIDs <- data()$branchInfo |>
          filter(branchID %in% plotData$branchID) |>
          pull(mtrID)
      } else {
        # If no limit all mtrIDs are being used and NULL is returned
        mtrIDs <- NULL
      }

      # Calculate the balancing values
      if (input$mtBalance) {
        plotData <- plotData |>
          left_join(
            treemapBalance(plotData$branchID, plotData$parentBranchID),
            by = c("branchID" = "id")
          )
      } else {
        plotData$balanceVal = 1
      }

      #Plot is being reset so the selected branch is the root
      # we're returning the auIDs also because otherwise lazy eval will not
      # trigger when other pair has same plot
      selectedBranch(list(
        mtrIDs = mtrIDs,
        selected = NULL,
        auIDs = data()$auIDs
      ))

      plotData
    })

    # Plotly Treemap
    output$mtPlot <- renderPlotly({
      req(mtPlotData())

      # Set the text
      boxText <- str_wrap(
        paste(mtPlotData()$meshSum, mtPlotData()$meshterm, sep = " | "),
        12
      )

      boxText <- ifelse(
        mtPlotData()$hasChildren,
        paste(boxText, "<b>+</b>"),
        boxText
      )

      #Plotly treemap
      plot <- plot_ly(
        type = "treemap",
        ids = mtPlotData()$branchID,
        parents = mtPlotData()$parentBranchID,
        labels = ifelse(
          is.na(mtPlotData()$meshSum),
          mtPlotData()$meshterm,
          paste(mtPlotData()$meshSum, mtPlotData()$meshterm, sep = " | ")
        ),
        text = boxText,
        values = mtPlotData()$balanceVal,
        marker = list(colors = mtPlotData()$colour),
        textinfo = "text",
        hovertext = mtPlotData()$authors,
        hoverinfo = "text",
        maxdepth = 2,
        source = "mtPlot"
      )

      plot
    })

    # Keep track of which branch is being viewed
    observeEvent(
      event_data("plotly_click", "mtPlot"),
      {
        branchIDs <- mtPlotData() |> pull(branchID)
        selected <- branchIDs[
          event_data("plotly_click", "mtPlot")$pointNumber + 1
        ]

        # Filter mtrIDs by limits if needed
        if (input$mtLimit > 0) {
          mtrIDs <- data()$branchInfo |>
            filter(branchID %in% branchIDs) |>
            pull(mtrID)
        } else {
          mtrIDs <- NULL
        }

        selectedBranch(list(
          mtrIDs = mtrIDs,
          selected = selected,
          auIDs = data()$auIDs
        ))
      }
    )

    # Return the currently selected branchID
    return(selectedBranch)
  })
}
