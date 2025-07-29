#' Module UI to setup an SQLite database in various ways
#'
#' Note that the server function will automatically generate the modal pop-up
#' when initialised based on the settings / URL.
#'
#' @param id ID name for the module
#' @param download Default = "button". Element to download the current database.
#' Choose from button, link or none
#'
#' @returns Module UI (with download element if not none)
#'
#' @export
mod_dbSetup_ui <- function(id, download = "button") {
  tagList(
    if (download == "button") {
      downloadButton(NS(id, "dbDownload"), "Download database")
    } else if (download == "link") {
      downloadLink(NS(id, "dbDownload"), "Download database")
    } else if (download == "none") {
      NULL
    }
  )
}

#' Module server to setup an SQLite database in various ways
#'
#' Option 1 - Explore a database on the server
#' Option 2 - Upload a database from your computer
#' Option 3 - Resume with a previously uploaded database
#' Option 4 - Start a new database
#'
#' @param id ID name for the server
#' @param localFolder Folder with existing, permanent databases to provide
#' @param tempFolder Folder where temp (new and uploaded) databases live
#' @param schema The schema of the SQLite database (.sql file)
#' @param options (Optional) If set, vector with integers indicating which DB
#' options will be available. By default all are
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
  options,
  useDB = NULL
) {
  localFolder <- normalizePath(localFolder, mustWork = T)
  tempFolder <- normalizePath(tempFolder, mustWork = T)
  schema <- normalizePath(schema, mustWork = T)

  # Modal to show
  dbSelectionModal <- function(localDBs, options) {
    choices = c(
      "Explore a database on the server" = 1,
      "Upload a database from your computer" = 2,
      "Resume with a previously uploaded database" = 3,
      "Start a new database" = 4
    )

    if (missing(options) || is.null(optoins)) {
      options = 1:length(choices)
    }

    if (!all(options) %in% 1:length(choices)) {
      stop("The DB selection options must be any of", 1:length(choices))
    }

    modalDialog(
      titlePanel("Select a database to get started"),
      radioButtons(
        NS(id, "option"),
        "How would you like to continue ...",
        choices = choices[options],
        width = "100%"
      ),
      if (1 %in% options) {
        conditionalPanel(
          condition = "input.option == '1'",
          selectInput(
            NS(id, "db_local"),
            "Choose an existing database",
            choices = localDBs
          ),
          ns = NS(id)
        )
      },
      if (2 %in% options) {
        conditionalPanel(
          condition = "input.option == '2'",
          fileInput(
            NS(id, "db_upload"),
            "Upload a database from your computer",
            accept = ".db"
          ),
          div(id = sprintf("%s-%s", id, "db_upload_msg")),
          ns = NS(id)
        )
      },
      if (3 %in% options) {
        conditionalPanel(
          condition = "input.option == '3'",
          textInput(NS(id, "db_tempCode"), "Provide temporary database code"),
          ns = NS(id)
        )
      },
      if (4 %in% options) {
        conditionalPanel(
          condition = "input.option == '4'",
          textInput(NS(id, "db_new"), "Database name"),
          ns = NS(id)
        )
      },
      actionButton(NS(id, "start"), "Continue"),
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

    # Update the modified time stamp to indicate the file is being used again
    Sys.setFileTime(dbPath, Sys.time())

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
        dir <- normalizePath(dirname(useDB), mustWork = T)
        dbName <- basename(normalizePath(useDB)) |> str_remove("\\.db$")
        result <- dbLocal(dbName, dir, dbName, schema)
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

    observeEvent(input$start, {
      if (input$option == "1") {
        # --- OPTION 1 - Connect to a permanent local database
        result <- dbLocal(input$db_local, localFolder, localDBs, schema)
        el <- "db_local"
      } else if (input$option == "2") {
        # --- OPTION 2 - Upload a database
        result <- dbUpload(input$db_upload, tempFolder, schema)
        el <- "db_upload_msg"
      } else if (input$option == "3") {
        # --- OPTION 3 - Connect to a temporary database with a code
        result <- dbTemp(input$db_tempCode, tempFolder, schema)
        el <- "db_tempCode"
      } else if (input$option == "4") {
        # --- OPTION 4 - Create a new, temporary database
        result <- dbNew(input$db_new, tempFolder, schema)
        el <- "db_new"
      } else {
        result <- list(info = NULL, success = F, msg = "start")
        el <- "start"
      }

      if (result$success) {
        connInfo(result$info)
        removeModal()
      } else {
        elementMsg(sprintf("%s-%s", id, el), result$msg)
      }
    })

    output$dbDownload <- downloadHandler(
      filename = function() {
        paste0(connInfo()$dbName, ".db")
      },
      content = function(file) {
        file.copy(connInfo()$dbPath, file)
      }
    )

    return(connInfo)
  })
}
