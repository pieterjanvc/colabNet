#' Wrapper function for profvis to work in Positron until issue resolved
#' https://github.com/posit-dev/positron/issues/3269
#'
#' @param expr R expression to evaluate with profvis
#' @param folder Default = "local". Folder where to store result files
#'
#' @import profvis
#' @importFrom htmlwidgets saveWidget
#' @importFrom rlang eval_tidy enquo
#'
#' @return Run profvis and open the results in the browser
#'
profvisRender <- function(expr, folder = "local") {
  folder = normalizePath(folder)
  temp_file <- tempfile(fileext = ".R")
  writeLines(deparse(substitute(expr)), con = temp_file)

  toTest <- profvis(
    source(temp_file, local = TRUE),
    prof_output = file.path(folder, "proftest.Rprofvis")
  )

  saveWidget(toTest, file.path(folder, "proftest.html"))
  # A URL always has forward slash
  browseURL(normalizePath(file.path(folder, "proftest.html"), winslash = "/"))
}
