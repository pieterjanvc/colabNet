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

#' Module UI to connect to an SQLite database in various ways
#'
#' @param id ID name for the module
#'
#' @returns When inserted it will pop-up a modal window. There is no permanent UI
#' @export
mod_dbConnect_ui <- function(id) {
  tagList(
    showModal(modalDialog(
      titlePanel("Select a database"),
      wellPanel(fluidRow(
        column(
          4,
          tags$b("Option 1"),
          br(),
          actionButton(NS(id, "goLocal"), "Explore selected")
        ),
        column(
          8,
          selectInput(
            NS(id, "db_local"),
            "Choose an existing database",
            choices = NULL
          )
        )
      )),
      wellPanel(fluidRow(
        column(
          4,
          tags$b("Option 2"),
          br(),
          actionButton(NS(id, "goUpload"), "Explore uploaded")
        ),
        column(
          8,
          fileInput(
            NS(id, "db_upload"),
            "Upload a database from your computer",
            accept = ".db"
          ),
          div(id = sprintf("%s-%s", id, "db_upload_msg"))
        )
      )),
      wellPanel(fluidRow(
        column(
          4,
          tags$b("Option 3"),
          br(),
          actionButton(NS(id, "goTemp"), "Continue with temp")
        ),
        column(
          8,
          textInput(NS(id, "db_tempCode"), "Provide temporary database code")
        )
      )),
      wellPanel(fluidRow(
        column(
          4,
          tags$b("Option 4"),
          br(),
          actionButton(NS(id, "goNew"), "Start new")
        ),
        column(
          8,
          textInput(NS(id, "db_new"), "Database name")
        )
      )),
      size = "xl",
      footer = NULL
    ))
  )
}

#' Module server to connect to an SQLite database in various ways
#'
#' @param id ID name for the server
#' @param localFolder Folder with existing, permanent databases to provide
#' @param tempFolder Folder where temp (new and uploaded) databases live
#'
#' @import shiny
#' @importFrom stringr str_remove str_detect
#'
#' @returns Reactive list variable with 4 items
#' - dbPath: path to the database
#' - dbName: (file) name of the database
#' - dbCode: temporary code name of the database
#' - dbType: method chosen from the menu
#'
#' @export
#'
mod_dbConnect_server <- function(id, localFolder, tempFolder) {
  if (!dir.exists(localFolder) | !dir.exists(tempFolder)) {
    stop("Invalid path to local or temp folder")
  }

  tempDBname <- function() {
    paste0(
      Sys.time() |> as.integer(),
      "_",
      paste(sample(c(LETTERS, letters, 0:9), 8), collapse = "")
    )
  }

  moduleServer(id, function(input, output, session) {
    # List existing databases
    localDBs <- list.files(localFolder, pattern = ".db") |> str_remove(".db$")
    updateSelectInput(session, "db_local", choices = localDBs)

    connInfo <- reactiveVal()

    # --- OPTION 1 - Connect to a permanent local (demo) database
    observeEvent(input$goLocal, {
      # Check if there are any options available
      if (input$db_local == "") {
        elementMsg(
          sprintf("%s-%s", id, "db_local"),
          "There are no databases available"
        )
      }
      req(input$db_local)
      # Check if the selected DB is valid
      dbPath <- file.path(localFolder, paste0(input$db_local, ".db"))
      check <- dbSetup(dbInfo = dbPath, checkSchema = T)

      if (!check$success) {
        elementMsg(
          sprintf("%s-%s", id, "db_local"),
          "There is an issue with the selected database. Please use another option"
        )
      }
      # Update info
      req(check$success)
      connInfo(list(
        dbPath = dbPath,
        dbName = input$db_local,
        dbCode = input$db_local,
        dbType = 1
      ))
      removeModal()
    })

    # --- OPTION 2 - Upload database
    observeEvent(input$goUpload, {
      if (is.null(input$db_upload$datapath)) {
        elementMsg(
          sprintf("%s-%s", id, "db_upload_msg"),
          "You must upload a valid Colabnet database first"
        )
      }

      req(input$db_upload$datapath)
      # Check if the selected DB is valid
      check <- dbSetup(dbInfo = input$db_upload$datapath, checkSchema = T)

      if (!check$success) {
        elementMsg(
          sprintf("%s-%s", id, "db_upload_msg"),
          "The provided database is not a valid Colabnet database"
        )
      }
      req(check$success)

      # Move the DB to the designated temp folder with temp name
      nameCode <- tempDBname()
      tempPath <- file.path(tempFolder, paste0(nameCode, ".db"))
      moveDB <- file.copy(input$db_upload$datapath, tempPath)

      # Update info
      connInfo(list(
        dbPath = tempPath,
        dbName = input$db_upload$name |> str_remove(".db$"),
        dbCode = nameCode,
        dbType = 2
      ))
      removeModal()
    })

    # --- OPTION 3 - Connect to a temporary database with code
    observeEvent(input$goTemp, {
      # Check if the database is (still) available
      dbPath <- file.path(tempFolder, paste0(input$db_tempCode, ".db"))
      check <- file.exists(dbPath)

      if (!check) {
        elementMsg(
          sprintf("%s-%s", id, "db_tempCode"),
          "There is no temporary databases with this code"
        )
      }
      req(check)
      # Check if the selected DB is still valid
      check <- dbSetup(dbInfo = dbPath, checkSchema = T)

      if (!check$success) {
        elementMsg(
          sprintf("%s-%s", id, "db_local"),
          "There is an issue with the selected database. Please use another option"
        )
      }
      # Update info
      req(check$success)
      connInfo(list(
        dbPath = dbPath,
        dbName = input$db_tempCode,
        dbCode = input$db_tempCode,
        dbType = 3
      ))
      removeModal()
    })

    # --- OPTION 4 - Create a new, temporary database
    observeEvent(input$goNew, {
      # Check th provided name
      check <- input$db_new |> str_detect("^[\\w-]+$")
      if (!check) {
        elementMsg(
          sprintf("%s-%s", id, "db_new"),
          paste(
            "The name of the new database can only contain",
            "letters, numbers, underscores or dashes.",
            "No spaces or other special characters"
          )
        )
      }
      req(check)
      nameCode <- tempDBname()
      tempPath <- file.path(tempFolder, paste0(nameCode, ".db"))
      check <- dbSetup(tempPath)

      req(check$success)
      # Update info
      connInfo(list(
        dbPath = tempPath,
        dbName = input$db_new,
        dbCode = nameCode,
        dbType = 4
      ))
      removeModal()
    })

    return(connInfo)
  })
}
