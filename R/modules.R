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
#' @returns Server function for MeSH Tree. The output will return a reactive list:
#' - mtrIDs: the mtrIDs currently in the treemap (based on filtering),
#' - selected: the currently selected branch,
#' - auIDs: the current author IDs
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
      auIDs <- papermeshtree()$auID |> unique()
      auIDs <- auIDs[auIDs != 0]

      list(
        auIDs = auIDs,
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

#' Module UI to setup an SQLite database in various ways
#'
#' Note that the server function will automatically generate the modal pop-up
#' when initialised based on the settings / URL.
#'
#' @param id ID name for the module
#'
#' @returns When inserted it will pop-up a modal window. There is no permanent UI
#'
#' @export
mod_dbSetup_ui <- function(id) {
  tagList()
}

#' Module server to setup an SQLite database in various ways
#'
#' @param id ID name for the server
#' @param localFolder Folder with existing, permanent databases to provide
#' @param tempFolder Folder where temp (new and uploaded) databases live
#' @param schema The schema of the SQLite database (.sql file)
#' @param useDB Default = NULL. If set, the provided database is used and the
#' rest is skipped. This is especially useful for dev when you don't want the
#' modal pop-up. If there is not DB at the specified path, a new one is created
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
mod_dbSetup_server <- function(
  id,
  localFolder,
  tempFolder,
  schema,
  useDB = NULL
) {
  localFolder <- normalizePath(localFolder, mustWork = T)
  tempFolder <- normalizePath(tempFolder, mustWork = T)
  schema <- normalizePath(schema, mustWork = T)

  # Modal to show
  dbSelectionModal <- function(localDBs) {
    modalDialog(
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
            choices = localDBs
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
    )
  }

  # Generate a temp name for a new / uploaded database
  tempDBname <- function() {
    paste0(
      Sys.time() |> as.integer(),
      "_",
      paste(sample(c(LETTERS, letters, 0:9), 8), collapse = "")
    )
  }

  # Connect to a permanent local database
  dbLocal <- function(dbName, localFolder, localDBs, schema) {
    # Check if there are any options available
    if (!dbName %in% localDBs) {
      return(list(
        success = F,
        msg = "The selected database was not found",
        info = NULL
      ))
    }

    # Check if the selected DB is valid
    dbPath <- file.path(localFolder, paste0(dbName, ".db"))
    check <- dbSetup(path = dbPath, checkSchema = T, schema = schema)

    if (!check$success) {
      return(list(
        success = F,
        msg = "There is an issue with the selected database. Please use another option",
        info = NULL
      ))
    }
    # Update info
    updateQueryString(
      sprintf("?dbmode=local&dbcode=%s", URLencode(dbName, reserved = T)),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "Local database found",
      info = list(
        dbPath = dbPath,
        dbName = dbName,
        dbCode = dbName,
        dbType = 1
      )
    ))
  }

  # --- Upload a database
  dbUpload <- function(uploadInfo, tempFolder, schema) {
    if (is.null(uploadInfo$datapath)) {
      return(list(
        success = F,
        msg = "You must upload a valid Colabnet database first",
        info = NULL
      ))
    }

    # Check if the selected DB is valid
    check <- dbSetup(
      path = uploadInfo$datapath,
      checkSchema = T,
      schema = schema
    )

    if (!check$success) {
      return(list(
        success = F,
        msg = "The provided database is not a valid Colabnet database",
        info = NULL
      ))
    }

    # Move the DB to the designated temp folder with temp name
    nameCode <- tempDBname()
    tempPath <- file.path(tempFolder, paste0(nameCode, ".db"))
    moveDB <- file.copy(uploadInfo$datapath, tempPath)
    dbName <- uploadInfo$name |> str_remove(".db$")

    # Update info
    updateQueryString(
      sprintf("?dbmode=temp&dbcode=%s", nameCode),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "Database upload successful",
      info = list(
        dbPath = tempPath,
        dbName = dbName,
        dbCode = nameCode,
        dbType = 2
      )
    ))
  }

  # --- Connect to a temporary database with a code
  dbTemp <- function(dbCode, tempFolder, schema) {
    # Check if the database is (still) available
    dbPath <- file.path(tempFolder, paste0(dbCode, ".db"))
    check <- file.exists(dbPath)

    if (!check) {
      return(list(
        success = F,
        msg = "There is no temporary databases with this code",
        info = NULL
      ))
    }

    # Check if the selected DB is still valid
    check <- dbSetup(path = dbPath, checkSchema = T, schema = schema)

    if (!check$success) {
      return(list(
        success = F,
        msg = "There is an issue with the selected database. Please use another option",
        info = NULL
      ))
    }

    # Update info
    updateQueryString(
      sprintf("?dbmode=temp&dbcode=%s", dbCode),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "Temp database found",
      info = list(
        dbPath = dbPath,
        dbName = dbCode,
        dbCode = dbCode,
        dbType = 3
      )
    ))
  }

  # --- Create a new, temporary database
  dbNew <- function(dbName, tempFolder, schema) {
    # Check th provided name
    check <- dbName |> str_detect("^[\\w-]+$")

    if (!check) {
      return(list(
        success = F,
        msg = paste(
          "The name of the new database can only contain",
          "letters, numbers, underscores or dashes.",
          "No spaces or other special characters"
        ),
        info = NULL
      ))
    }

    nameCode <- tempDBname()
    tempPath <- file.path(tempFolder, paste0(nameCode, ".db"))
    check <- dbSetup(tempPath, schema = schema)

    updateQueryString(
      sprintf("?dbmode=temp&dbcode=%s", nameCode),
      mode = "push"
    )

    return(list(
      success = T,
      msg = "New database creation successful",
      info = list(
        dbPath = tempPath,
        dbName = dbName,
        dbCode = nameCode,
        dbType = 4
      )
    ))
  }

  moduleServer(id, function(input, output, session) {
    # List existing databases
    localDBs <- list.files(localFolder, pattern = ".db") |> str_remove(".db$")

    # This reactive will be returned by the module
    connInfo <- reactiveVal()

    # Runs when the module is intialised
    observe({
      if (!is.null(useDB)) {
        # Path has been directly provided
        useDB <- normalizePath(useDB, mustWork = T)
        dbName <- basename(useDB) |> str_remove("\\.db$")
        result <- dbLocal(dbName, dirname(useDB), dbName, schema)
        connInfo(result$info)
      } else if (!"dbmode" %in% names(isolate(getQueryString()))) {
        # No DB data provided, show modal
        showModal(dbSelectionModal(localDBs))
      } else {
        # Check the provided DB info in URL
        dbmode <- getQueryString()$dbmode

        if (dbmode == "local") {
          dbName <- URLdecode(getQueryString()$dbcode)
          result <- dbLocal(dbName, localFolder, localDBs, schema)
        } else if (dbmode == "temp") {
          result <- dbTemp(getQueryString()$dbcode, tempFolder, schema)
        } else {
          result(list(
            success = F,
            msg = "ERROR - The URL does not point to a valid database"
          ))
        }

        # Set connInfo if valid DB or show modal
        if (result$success) {
          connInfo(result$info)
        } else {
          mod_dbConnect_ui("getDB")
          showNotification(result$msg, type = "error")
        }
      }
    })

    # --- OPTION 1 - Connect to a permanent local database
    observeEvent(input$goLocal, {
      result <- dbLocal(input$db_local, localFolder, localDBs, schema)

      if (result$success) {
        connInfo(result$info)
        removeModal()
      } else {
        elementMsg(sprintf("%s-%s", id, "db_local"), result$msg)
      }
    })

    # --- OPTION 2 - Upload a database
    observeEvent(input$goUpload, {
      result <- dbUpload(input$db_upload, tempFolder, schema)

      if (result$success) {
        connInfo(result$info)
        removeModal()
      } else {
        elementMsg(sprintf("%s-%s", id, "db_upload_msg"), result$msg)
      }
    })

    # --- OPTION 3 - Connect to a temporary database with a code
    observeEvent(input$goTemp, {
      result <- dbTemp(input$db_tempCode, tempFolder, schema)

      if (result$success) {
        connInfo(result$info)
        removeModal()
      } else {
        elementMsg(sprintf("%s-%s", id, "db_tempCode"), result$msg)
      }
    })

    # --- OPTION 4 - Create a new, temporary database
    observeEvent(input$goNew, {
      result <- dbNew(input$db_new, tempFolder, schema)

      if (result$success) {
        connInfo(result$info)
        removeModal()
      } else {
        elementMsg(sprintf("%s-%s", id, "db_new"), result$msg)
      }
    })

    return(connInfo)
  })
}
