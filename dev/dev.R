file.copy("data/dbmi.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"

devtools::install_github("pieterjanvc/sqlife", ref = "expandConnections")
devtools::install_github("pieterjanvc/sqlife", ref = "debug")


colabNetDB <- "D:/Desktop/newCN.db"
file.remove(colabNetDB)
dbSetup(colabNetDB, schema = "inst/create_colabNetDB.sql")


test <- ncbi_publicationDetails(
  PMIDs = c(32528441, 32333753),
  lastName = "Van Camp",
  firstName = "PJ",
  initials = "PJ"
)

conn <- dbGetConn(colabNetDB)

test2 <- dbAddAuthorPublications(test, dbInfo = conn)

dbFinish(conn)

tempFun <- function(
  conn,
  new = c("commit", "revert"),
  inherit = c("continue", "commit", "revert"),
  showWarnings = T,
  error
) {
  env = parent.frame()
  parFun = as.character(sys.call(sys.parent()))[1]
  parentID <- sub("^<environment: (.*)>$", "\\1", format(env))
  existing <- attributes(conn)$existing
  commit <- ifelse(existing, inherit[1] == "commit", new[1] == "commit")
  continue <- ifelse(existing, inherit[1] == "continue", F)
  closeExisting <- !existing
  closed <- F
  if (missing(error) && is.null(attr(conn, "sqlife")$environ[[parentID]])) {
    check <- parentID != names(attr(conn, "sqlife")$environ)[[1]]
    if (check) {
      orgEnv <- attr(conn, "sqlife")$environ[[1]]$parFun
      error <- paste(
        "dbFinish cannot be called inside",
        parFun,
        "as the connection was opened in",
        ifelse(orgEnv == "dbGetConn", "the global environment", orgEnv),
        "and should be closed there"
      )
    } else {
      error <- paste(
        "dbFinish was called inside",
        parFun,
        "without dbGetConn in the same environment"
      )
    }
  }
  if (!missing(error)) {
    commit <- F
    continue <- F
    closeExisting <- T
  }
  if (!dbIsValid(conn)) {
    if (!missing(error)) {
      stop(error)
    }
    if (showWarnings) {
      warning("The connection has already been closed or is not valid")
    }
    return(list(changed = F, transacting = F, closed = T))
  }
  transacting <- sqliteIsTransacting(conn)
  changed <- F
  if (transacting & commit & !continue) {
    changed <- dbGetQuery(conn, "SELECT total_changes();")[[1]] != 0
    dbCommit(conn)
    transacting <- F
  } else if (transacting & closeExisting & !continue) {
    changed <- F
    dbRollback(conn)
    transacting <- F
  }
  if (closeExisting || !attr(conn, "existing")) {
    closed <- T
    dbDisconnect(conn)
  }
  attr(conn, "sqlife")$environ[[parentID]][["finished"]] <- T
  if (!missing(error)) {
    stop("\n---- DETAILS ----\n", error, "\n-----------------\n")
  }
  invisible(list(changed = changed, transacting = transacting, closed = closed))
}
