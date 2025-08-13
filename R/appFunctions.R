#' Run the colabNet Shiny App
#'
#' @param colabNetDB Path to the ColabNet database (will be created if needed)
#' @param localFolder (Optional) Path to a folder with database to explore
#' @param tempFolder (Optional) Path to a folder where temp databases are stored
#'
#' If local and temp folders are not set, they will be created in the default temp
#'
#' @import shiny dplyr stringr tidyr purrr visNetwork pool plotly
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom RSQLite SQLite
#' @importFrom DT DTOutput renderDT datatable dataTableProxy replaceData
#' @importFrom igraph graph_from_data_frame E degree components distances edge_density transitivity
#'
#' @return Start the Shiny app
#'
#' @export
#'
colabNet <- function(colabNetDB, localFolder, tempFolder) {
  envInfo = list(
    mode = "package",
    dbPath = if (missing(colabNetDB)) {
      NULL
    } else {
      check <- normalizePath(dirname(colabNetDB), mustWork = T)
      colabNetDB
    },
    localFolder = if (missing(localFolder)) {
      NULL
    } else {
      localFolder
    },
    tempFolder = if (missing(tempFolder)) {
      NULL
    } else {
      tempFolder
    },
    autoCleanTemp = NULL
  )

  sys.source(
    system.file("app.R", package = "colabNet"),
    envir = environment()
  )
  shinyApp(ui, server)
}

#' Show a screen overlay when busy to prevent other actions
#' By default an animation is played to indicate the server is busy.
#' removeModal has to be called separately
#'
#' @param session A Shiny session object
#' @param message (Optional) Text to add to the modal
#' @param title (Optional) Title of the modal
#' @param showAnimation Default = T. Show a busy animation
#'
#' @returns A modal pop-up window
#' @export
#'
busyModal <- function(
  session,
  message = NULL,
  title = NULL,
  showAnimation = T
) {
  showModal(modalDialog(
    if (showAnimation) {
      tags$img(src = "busy.gif", style = "width:100%;")
    },

    tags$div(message),
    footer = NULL,
    title = title
  ))
}

#' Decorate an HTML element with a message
#'
#' @param elementID The ID of the element as set in Shiny
#' @param message (Optional) If set, this message will be shown, if not any
#' existing message for this ID will be removed
#' @param type (Default = "error"). info, error or success (will define the colour)
#' @param where (Default = "afterEnd) Relative position of the message to the
#' ID. Any of "beforeStart", "afterStart", "beforeEnd" or "afterEnd"
#' @param session (Default = getDefaultReactiveDomain()). Shiny session object
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom shiny insertUI removeUI getDefaultReactiveDomain
#'
#' @return A dataset that can be used to create a (plotly) Treemap
#' @export
#'
elementMsg <- function(
  elementID,
  message,
  type = "error",
  where = "afterEnd",
  session = getDefaultReactiveDomain()
) {
  # Make sure the tag ID starts with # or .
  tagID <- ifelse(
    str_detect(elementID, "^[.#]"),
    elementID,
    paste0("#", elementID)
  )

  # Remove existing element
  removeUI(paste0(tagID, "_msg"), session = session)

  # Stop if no new message
  if (missing(message)) {
    return()
  }

  # Add / update message
  msg <- tags$div(
    tags$i(
      message,
      style = sprintf(
        "color:%s",
        case_when(
          type == "info" ~ "#2196f3",
          type == "error" ~ "#f44336",
          type == "success" ~ "#4caf50",
          TRUE ~ "#262626"
        )
      )
    ),
    id = paste0(elementID, "_msg")
  )

  insertUI(selector = tagID, where = where, msg, session = session)
}

#' Generate Shiny App Production Folder
#' This will extract all relevant files needed to publish the shiny app on a server
#'
#' @param folder Folder where to create the ColabNet publication folder
#' (this folder will be generated if needed)
#' @param localDBs (Optional) Vector of any local databases to include in the app
#' @param overwrite Default = T. If set to F, existing files will generate error
#'
#' @returns A ColabNet folder with all Shiny app files needed for production
#' @export
#'
generateProdFolder <- function(folder, localDBs, overwrite = T) {
  if (missing(folder) || !dir.exists(folder)) {
    stop("Folder not found")
  }

  folder <- file.path(folder, "ColabNet")

  if (!overwrite && length(list.files(folder)) > 0) {
    stop("ColabNet folder is not empty and overwrite = FALSE")
  }

  # Copy app and lock files
  x <- dir.create(folder, showWarnings = F)
  x <- file.copy(
    c(
      system.file("app.R", package = "colabNet"),
      system.file("renv.lock", package = "colabNet")
    ),
    folder,
    overwrite = T
  )

  # Change the app to production mode
  app <- readLines(file.path(folder, "app.R"))
  #Only check the first few lines (should be in there)
  modeFound <- any(grepl("mode <- \"dev\"", app[1:10]))
  if (!modeFound) {
    stop(
      "Make sure the line with `mode <- \"dev\"` is present in the first 10 lines"
    )
  }
  app[1:10] <- gsub("mode <- \"dev\"", "mode <- \"prod\"", app[1:10])
  writeLines(app, file.path(folder, "app.R"))

  # Copy www folder
  x <- dir.create(file.path(folder, "www"), showWarnings = F)
  x <- file.copy(
    list.files(system.file("www", package = "colabNet"), full.names = T),
    file.path(folder, "www"),
    overwrite = T
  )

  # Remove old databases and copy new ones
  if (file.exists(file.path(folder, "localDB"))) {
    x <- file.remove(list.files(file.path(folder, "localDB"), full.names = T))
  }

  if (!missing(localDBs)) {
    x <- dir.create(file.path(folder, "localDB"), showWarnings = F)
    x <- file.copy(localDBs, file.path(folder, "localDB"), overwrite = T)
  }

  # Check the environment
  check <- capture.output(renv::status(project = folder)$synchronized)[2]
  if (check != "[1] TRUE") {
    invisible(capture.output(renv::install(project = folder, prompt = F)))
    invisible(capture.output(renv::snapshot(project = folder, prompt = F)))
  }
}
