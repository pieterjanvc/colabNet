#' Filter treemap plot data based on limits
#'
#' @param plotData A MeSH Tree plot data dataframe
#' @param n Number of top-scoring categories to show
#'
#' @import dplyr
#'
#' @returns Data frame of plotData filtered by top-n categories per level
#' @export
#'
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
        )
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
#'
#' @returns Server function for MeSH Tree. The output will retrun the currently
#' selected branchID
#'
#' @import shiny dplyr plotly
#'
#' @export
#'
mod_meshTree_server <- function(id, papermeshtree) {
  moduleServer(id, function(input, output, session) {
    # Keep track of which branch is being viewed (also returned)
    selectedBranch <- reactiveVal()

    data <- reactive({
      list(
        plotData = treemapData(papermeshtree()),
        branchInfo = papermeshtree() |> select(branchID, mtrID) |> distinct()
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

      #Plot is being reset so the selected branch is the root (0)
      selectedBranch(list(mtrIDs = mtrIDs, selected = NULL))

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
      plot_ly(
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

        selectedBranch(list(mtrIDs = mtrIDs, selected = selected))
      }
    )

    # Return the currently selected branchID
    return(selectedBranch)
  })
}
