#' Admin module for Colabnet  - UI
#'
#' @param id ID for the module (match with server)
#'
#' @returns UI object for database admin
#' @export
#'
mod_admin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      tags$h3("Articles in database"),
      selectInput(ns("auID"), "Author", choices = NULL),
      uiOutput(ns("alternativeNames")),
      fluidRow(DTOutput(ns("authorInDB"))),
      actionButton(ns("artDel"), "Remove selected articles")
    ),
    wellPanel(
      fluidRow(
        column(
          6,
          tags$h3("Find articles on Pubmed"),
          textInput(ns("lastName"), "Last name"),
          textInput(ns("firstName"), "First name + middle initials"),
          tags$i(
            HTML(
              "Use the first name and any middle name initials the author uses to publish.",
              "<ul><li>For a more specific search use full first name and then middle initials: ",
              "Joseph E Murray &rarr; Joseph E</li><li>For a broader search, use initals only",
              "Joseph E Murray &rarr; JE</li></ul>"
            )
          )
        ),
        column(
          6,
          HTML("<br><h4><i>Optional Filters</i></h4>"),
          textAreaInput(ns("PMIDs"), "Limit search by PMID (comma separated)"),
          textInput(ns("auAff"), "Filter based on affiliation (RegEx style)")
        ),
      ),

      actionButton(ns("pubmedByAuthor"), "Search Pubmed"),
      br(),
      br(),
      DTOutput(ns("authorSearch")),
      actionButton(ns("artAdd"), "Add selected articles")
    ),
    wellPanel(
      tags$h3("Bulk Import from CSV"),
      tags$i(
        HTML(
          "Upload a CSV file with the following columns<ul>",
          "<li>lastName: The last name of the author</li>",
          "<li>firstName: The first name + middle initials (see above for details)</li>",
          "<li>affiliation: (optional) A RegEx pattern to filter by author affiliation</li>",
          "</ul>"
        )
      ),
      fileInput(ns("bulkImportAuthor"), "CSV file", accept = ".csv"),
      tags$i(
        HTML(
          "Note that the nPubMed column below will show the number of articles ",
          "that match the author before filtering by affiliation (will be done when importing)"
        )
      ),
      div(id = "bulkImportAuthorMsg"),
      DTOutput(ns("bulkImportTable")),
      actionButton(ns("startBulkImport"), "Start Import")
    )
  )
}

#' Admin module for Colabnet  - Server
#'
#' @param id ID for the module (match with UI)
#' @param pool A reactive pool object connected to the database
#'
#' @returns Nothing
#'
#' @export
#'
mod_admin_server <- function(id, pool) {
  # Function to extract relevant authors (first, last and of interest) from list
  relevantAuthors <- function(authors, lastName) {
    lastName <- unique(lastName)

    if (length(lastName) > 1) {
      stop("Only a single last name can be used")
    }

    # Get first and last author, and author of interest if not either
    firstAuth <- str_extract(authors, "^[^,]+")
    lastAuth <- str_extract(authors, "\\s([^,]+)$", group = 1)
    middleAuth <- str_extract(
      authors,
      sprintf(",\\s(%s[^,]+),", lastName),
      group = 1
    )

    sprintf(
      "%s%s%s",
      firstAuth,
      ifelse(is.na(middleAuth), " ... ", sprintf(" ... %s ... ", middleAuth)),
      lastAuth
    )
  }

  moduleServer(id, function(input, output, session) {
    # ---- Existing authors ----
    authorList <- reactiveVal()

    observeEvent(pool(), {
      authors <- tbl(isolate(pool()), "author") |>
        filter(authorOfInterest == 1) |>
        left_join(
          tbl(pool(), "authorName") |>
            group_by(auID) |>
            filter(default) |>
            ungroup(),
          by = "auID"
        ) |>
        select(auID, lastName, firstName, initials) |>
        collect()
      authorList(authors)
    })

    # Update the list of authors user can select from
    observeEvent(authorList(), {
      newVals <- authorList()
      updateSelectInput(
        session,
        "auID",
        choices = setNames(
          c(newVals$auID, "0"),
          c(
            paste(newVals$lastName, newVals$firstName, sep = ", "),
            "Not in DB"
          )
        ),
        selected = newVals$auID[1]
      )
    })

    # Show alternative author names
    output$alternativeNames <- renderUI({
      auNames <- tbl(pool(), "authorName") |>
        filter(auID == local(input$auID)) |>
        collect()

      # Set the Pubmed Search to match selection
      default <- auNames |> filter(default == 1)
      altNames <- auNames |> filter(default == 0)

      if (nrow(altNames) == 0) {
        return(NULL)
      } else {
        return(tags$i(sprintf(
          "Also published under: %s",
          paste(
            altNames$lastName,
            altNames$firstName,
            sep = ", ",
            collapse = " | "
          )
        )))
      }
    })

    authorArticles <- reactive({
      # Reset UI
      elementMsg(sprintf("%s-%s", id, "pubmedByAuthor"))

      authors <- tbl(pool(), "coAuthor") |>
        group_by(arID) |>
        filter(any(local(input$auID) == auID)) |>
        ungroup() |>
        left_join(
          tbl(pool(), "authorName") |>
            filter(default) |>
            select(auID, lastName, initials),
          by = "auID"
        ) |>
        arrange(arID, authorOrder) |>
        mutate(name = paste(lastName, initials)) |>
        collect() |>
        group_by(arID) |>
        summarise(
          auID = local(input$auID),
          authors = paste(name, collapse = ", "),
          .groups = "drop"
        )

      articles <- tbl(pool(), "article") |>
        filter(arID %in% local(authors$arID)) |>
        collect() |>
        left_join(authors, by = "arID")
    })

    # ---- EXISTING ARTICLES BY AUTHOR ----
    emptyTable <- data.frame(
      PMID = character(),
      Title = character(),
      Info = character()
    )

    # Table that shows author articles
    output$authorInDB <- renderDT(
      {
        emptyTable
      },
      escape = F,
      rownames = F
    )

    authorInDB_proxy <- dataTableProxy("authorInDB")

    articlesInDB <- reactiveVal(NULL)

    observeEvent(
      input$auID,
      {
        authors <- tbl(pool(), "coAuthor") |>
          group_by(arID) |>
          filter(any(auID == local(as.integer(input$auID)))) |>
          ungroup() |>
          left_join(
            tbl(pool(), "authorName") |>
              filter(default) |>
              select(auID, lastName, initials),
            by = "auID"
          ) |>
          arrange(arID, authorOrder) |>
          mutate(name = paste(lastName, initials)) |>
          collect() |>
          group_by(arID) |>
          summarise(
            auID = local(input$auID),
            authors = paste(name, collapse = ", "),
            .groups = "drop"
          )

        df <- tbl(pool(), "article") |>
          filter(arID %in% local(authors$arID)) |>
          collect() |>
          left_join(authors, by = "arID")

        articlesInDB(df)
      },
      ignoreInit = T,
      ignoreNULL = F
    )

    observeEvent(
      articlesInDB(),
      {
        if (is.null(articlesInDB()) || nrow(articlesInDB()) == 0) {
          replaceData(authorInDB_proxy, emptyTable, rownames = F)
          return()
        }

        lastName <- tbl(pool(), "author") |>
          filter(authorOfInterest == 1, auID == local(input$auID)) |>
          left_join(
            tbl(pool(), "authorName") |> filter(default == 1),
            by = "auID"
          ) |>
          pull(lastName)

        replaceData(
          authorInDB_proxy,
          articlesInDB() |>
            mutate(
              Info = sprintf(
                "%s | <i>%s</i> (%s)",
                relevantAuthors(authors, lastName),
                journal,
                year
              ),
              PMID = sprintf(
                '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
                PMID,
                PMID
              )
            ) |>
            select(PMID, Title = title, Info),
          rownames = F
        )
      },
      ignoreNULL = F
    )

    # ----- NEW ARTICLE SEARCH ----

    # Table that shows author articles
    output$authorSearch <- renderDT(
      {
        emptyTable
      },
      escape = F,
      rownames = F
    )

    authorSearch_proxy <- dataTableProxy("authorSearch")

    searchResults <- reactiveVal(list(
      articles = NULL,
      author = NULL,
      history = NULL,
      pubDetails = NULL
    ))

    observeEvent(
      input$pubmedByAuthor,
      {
        # Check input
        if (
          str_trim(input$lastName) == "" |
            str_trim(input$firstName) == ""
        ) {
          elementMsg(
            sprintf("%s-%s", id, "pubmedByAuthor"),
            "You need to provide both last name and first name"
          )
          replaceData(authorSearch_proxy, emptyTable, rownames = F)
          return()
        }

        # Don't allow clicking button again while searching
        disable("pubmedByAuthor")

        # If the author is not in the database, set to unknown
        inDB <- tbl(pool(), "authorName") |>
          collect() |>
          filter(
            simpleText(lastName) %in% simpleText(input$lastName),
            simpleText(firstName) %in%
              simpleText(input$firstName) |
              simpleText(initials) %in% simpleText(input$firstName)
          )

        # Get info from Pubmed
        author <- ncbi_author(
          input$lastName,
          input$firstName,
          showWarnings = F
        ) |>
          filter(default)

        # If no author found
        if (length(author$lastName) == 0) {
          elementMsg(
            sprintf("%s-%s", id, "pubmedByAuthor"),
            sprintf(
              "No results for the author with last name '%s' and first name '%s'",
              input$lastName,
              input$firstName
            )
          )
          enable("pubmedByAuthor")
          updateSelectInput(session, "auID", selected = "0")
          replaceData(authorSearch_proxy, emptyTable, rownames = F)
          return()
        }

        # Look for articles on Pubmed
        msg = ""

        search <- ncbi_authorArticleList(
          lastName = input$lastName,
          firstName = input$firstName,
          PMIDs = str_split(input$PMIDs, ",")[[1]] |> str_trim(),
          returnHistory = T
        )

        if (!search$success) {
          msg = sprintf(
            "This search yielded too many (%i) results. Please use PMIDs instead",
            search$n
          )
          df <- emptyTable
        } else if (search$n == 0) {
          msg = sprintf(
            "No results for the author with last name '%s' and first name '%s'",
            input$lastName,
            input$firstName
          )
          df <- emptyTable
        } else {
          df <- search$articles |>
            transmute(
              PMID,
              Title = title,
              Info = sprintf(
                "%s | <i>%s</i> (%s)",
                relevantAuthors(authors, author$lastName),
                journal,
                as.Date(date) |> format("%Y")
              )
            )
        }

        # Check if affiliation filter is present (more calculation needed)
        auAff <- str_trim(input$auAff)

        if (auAff != "" & nrow(df) > 0) {
          pubDetails <- ncbi_publicationDetails(
            PMIDs = df$PMID,
            lastName = author$lastName,
            firstName = author$firstName,
            initials = author$initials,
            history = search$history
          ) |>
            filter_affiliation(auAff)

          df <- df |> filter(PMID %in% pubDetails$articles$PMID)

          if (nrow(df) == 0) {
            msg = "No articles found that match the affiliation filter"
            auAff <- NA # Set this so the message is not overwritten by next one
          }
        } else {
          pubDetails <- NULL
        }

        existing <- tbl(pool(), "article") |>
          filter(PMID %in% local(df$PMID)) |>
          pull(PMID)

        df <- df |> filter(!PMID %in% {{ existing }})

        if (nrow(df) == 0 & !is.na(auAff)) {
          msg = "No new articles found that aren't already in the database (see above)"
        } else if (
          nrow(df) > 0 &
            length(existing) > 0 &
            !is.na(auAff)
        ) {
          msg = sprintf(
            " %i articles for this author are already in the database (see above) and are not listed in the result",
            length(existing)
          )
        }

        if (length(existing) > 0) {
          toSelect = inDB |>
            filter(default == 1) |>
            pull(auID) |>
            as.character()
        } else {
          toSelect = "0"
        }

        updateSelectInput(session, "auID", selected = toSelect)

        if (msg != "") {
          elementMsg(sprintf("%s-%s", id, "pubmedByAuthor"), msg, "info")
        } else {
          elementMsg(sprintf("%s-%s", id, "pubmedByAuthor"))
        }

        enable("pubmedByAuthor")
        searchResults(
          list(
            articles = df,
            author = author,
            history = search$history,
            pubDetails = pubDetails
          )
        )
      },
      ignoreInit = T
    )

    observeEvent(searchResults(), {
      if (is.null(searchResults()$articles)) {
        replaceData(authorSearch_proxy, emptyTable, rownames = F)
        return()
      }

      replaceData(
        authorSearch_proxy,
        searchResults()$articles |>
          mutate(
            PMID = sprintf(
              '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
              PMID,
              PMID
            )
          ) |>
          select(PMID, Title, Info),
        rownames = F
      )
    })

    observe({
      PMIDs <- searchResults()$articles[input$authorSearch_rows_selected, ] |>
        pull(PMID)

      if (length(PMIDs) == 0) {
        showNotification("No articles to add", type = "message")
      }

      req(length(PMIDs) > 0)
      disable("artAdd")

      if (length(searchResults()$pubDetails) == 0) {
        # TODO Improve function so it will filter somehow history object or do it again
        new <- ncbi_publicationDetails(
          PMIDs = PMIDs,
          lastName = searchResults()$author$lastName,
          firstName = searchResults()$author$firstName,
          initials = searchResults()$author$initials,
          history = searchResults()$history
        ) |>
          filter_PMID(PMIDs)
      } else {
        new <- filter_PMID(searchResults()$pubDetails, PMIDs)
      }

      new <- dbAddAuthorPublications(new, dbInfo = localCheckout(pool()))

      # Remove added articles from search results
      searchResults(
        list(
          articles = searchResults()$articles |> filter(!PMID %in% new$PMID),
          author = searchResults()$author
        )
      )

      # Because of lazy eval we have to make a switch of inputs to update the table
      authorList({
        tbl(pool(), "author") |>
          filter(authorOfInterest == 1) |>
          left_join(tbl(pool(), "authorName"), by = "auID") |>
          collect() |>
          arrange(lastName, firstName)
      })

      if (input$auID == new$auID[1]) {
        updateSelectInput(session, "auID", selected = "0")
      }

      updateSelectInput(session, "auID", selected = new$auID[1])
      enable("artAdd")
    }) |>
      bindEvent(input$artAdd)

    observe({
      PMIDs <- auArtList()[input$authorArticleList_rows_selected, ] |>
        filter(InDB == "YES") |>
        pull(PMID)

      noHTML <- PMIDs |>
        str_extract(".*>(.+)<", group = 1)

      if (length(PMIDs) == 0) {
        showNotification("No articles to remove", type = "message")
      }

      req(length(PMIDs) > 0)
      disable("artDel")
      arIDs <- tbl(pool(), "article") |>
        filter(PMID %in% local(noHTML)) |>
        pull(arID)

      deleted <- dbDeleteArticle(arIDs, dbInfo = localCheckout(pool()))

      updatedTable <- auArtList() |>
        mutate(InDB = ifelse(PMID %in% PMIDs, "NO", InDB))
      replaceData(authorArticleList_proxy, updatedTable)
      enable("artDel")
    }) |>
      bindEvent(input$artDel)

    # ----- BULK IMPORT ----
    shinyjs::hide("startBulkImport")

    bulkImport <- eventReactive(
      input$bulkImportAuthor,
      {
        shinyjs::hide("startBulkImport")

        tryCatch(
          {
            data <- read.csv(input$bulkImportAuthor$datapath)
            missing <- setdiff(
              c("lastName", "firstName", "affiliation"),
              colnames(data)
            )

            if (length(missing) > 0) {
              stop(
                "The following columns are missing: ",
                paste(missing, collapse = ", ")
              )
            }

            if (
              !all(
                str_detect(data$firstName, "\\w+"),
                str_detect(data$lastName, "\\w+")
              )
            ) {
              stop("The firstName and lastName must be provided for everyone")
            }

            elementMsg(sprintf("%s-%s", id, "bulkImportAuthorMsg"))
            data <- data |> select(lastName, firstName, affiliation)
          },
          error = function(e) {
            elementMsg(
              sprintf("%s-%s", id, "bulkImportAuthorMsg"),
              HTML(
                sprintf(
                  "The upload failed with the following message:<br>%s",
                  e$message
                )
              )
            )
            shinyjs::hide("startBulkImport")

            return()
          }
        )

        importInfo <- data.frame()
        importData <- list()
        history <- list()

        withProgress(
          message = 'Verify Authors',
          value = 0,
          {
            n <- nrow(data)

            for (i in 1:n) {
              incProgress(
                1 / n,
                "Verify author:",
                detail = sprintf(
                  "%s, %s (%i/%i)",
                  data$lastName[i],
                  data$firstName[i],
                  i,
                  n
                )
              )

              status <- "ready to import author articles"

              # Get info from Pubmed
              author <- ncbi_author(
                data$lastName[i],
                data$firstName[i],
                showWarnings = F
              ) |>
                filter(default)

              # If no author found
              if (length(author$lastName) == 0) {
                status <- "No articles found on PubMed"
                df <- data.frame(
                  lastName = data$lastName[i],
                  firstName = data$firstName[i],
                  affiliation = data$affiliation[i],
                  nPubMed = 0,
                  status = status
                )
                importInfo <- bind_rows(importInfo, df)
                next()
              }

              # Look for articles in Pubmed
              search <- ncbi_authorArticleList(
                data$lastName[i],
                data$firstName[i],
                PMIDonly = T,
                returnHistory = T
              )

              if (!search$success) {
                status <- "> 1000 matches. Will be skipped"
                importData <- importData |>
                  append(list(
                    list(
                      author = NULL,
                      PMIDs = NULL,
                      affiliation = NULL,
                      statusCode = 1
                    )
                  ))
              } else if (search$n == 0) {
                status <- "No articles found on PudMed"
                importData <- importData |>
                  append(list(
                    list(
                      author = NULL,
                      PMIDs = NULL,
                      affiliation = NULL,
                      statusCode = 2
                    )
                  ))
              } else {
                importData <- importData |>
                  append(list(
                    list(
                      author = author,
                      PMIDs = search$PMID,
                      affiliation = data$affiliation[i],
                      statusCode = 0
                    )
                  ))
              }

              df <- data.frame(
                lastName = data$lastName[i],
                firstName = data$firstName[i],
                affiliation = data$affiliation[i],
                nPubMed = search$n,
                status = status
              )

              importInfo <- bind_rows(importInfo, df)

              history <- history |> append(list(search$history))
            }

            shinyjs::show("startBulkImport")
          }
        )

        list(
          importInfo = importInfo,
          importData = importData,
          history = history
        )
      }
    )

    observeEvent(input$startBulkImport, {
      shinyjs::hide("startBulkImport")
    })

    emptyImport <- data.frame(
      lastName = character(),
      firstName = character(),
      affiliation = character(),
      nPubMed = integer(),
      status = character()
    )

    output$bulkImportTable <- renderDT(
      {
        emptyImport
      },
      escape = F,
      rownames = F
    )

    bulkImportTable_proxy <- dataTableProxy("bulkImportTable")

    bulkImportResults <- reactiveVal(list(articles = NULL, author = NULL))

    observeEvent(bulkImport(), {
      if (is.null(bulkImport()$importInfo)) {
        replaceData(bulkImportTable_proxy, emptyImport, rownames = F)
        return()
      }

      replaceData(bulkImportTable_proxy, bulkImport()$importInfo, rownames = F)
    })

    observeEvent(input$startBulkImport, {
      nImported <- data.frame()

      withProgress(message = 'Gather author data from NCBI', value = 0, {
        n <- length(bulkImport()$importData)

        toImport <- which(
          sapply(bulkImport()$importData, "[[", c("statusCode")) == 0
        )

        for (i in toImport) {
          data <- bulkImport()$importData[[i]]

          incProgress(
            1 / n,
            "Collecting Data From NCBI:",
            detail = sprintf(
              "Processing %s, %s (%i/%i)",
              data$author$lastName,
              data$author$firstName,
              i,
              n
            )
          )

          new <- ncbi_publicationDetails(
            PMIDs = data$PMIDs,
            lastName = data$author$lastName,
            firstName = data$author$firstName,
            initials = data$author$initials,
            history = bulkImport()$history[[i]]
          ) |>
            filter_affiliation(data$affiliation)

          new <- dbAddAuthorPublications(
            new,
            dbInfo = localCheckout(pool()),
            flagUpdate = F
          )

          nImported <- bind_rows(
            nImported,
            data.frame(
              afterFilter = nrow(new),
              new = sum(new$status == "new"),
              existing = sum(new$status == "existing")
            )
          )
        }
      })

      dbFlagUpdate(1, dbInfo = localCheckout(pool()))

      nImported <- nImported |>
        mutate(
          status = case_when(
            afterFilter == 0 ~
              "WARNING - No articles found after affiliation filtering",
            afterFilter == existing ~
              "IGNORED - All articles for this author were already in the database",
            TRUE ~
              sprintf(
                "IMPORTED - %i matched affiliation, %i existing, %i new",
                afterFilter,
                existing,
                new
              )
          ),
          statusCode = case_when(
            afterFilter == 0 ~ 0,
            afterFilter == existing ~ 1,
            TRUE ~ 2
          )
        )
      importInfo <- bulkImport()$importInfo
      importInfo$status = "Skipped"
      importInfo$status[toImport] = nImported$status
      importInfo$statusCode = 3
      importInfo$statusCode[toImport] = nImported$statusCode

      replaceData(
        bulkImportTable_proxy,
        importInfo |>
          arrange(statusCode, lastName, firstName) |>
          select(-statusCode),
        rownames = F
      )

      shinyjs::show("startBulkImport")
    })

    return()
  })
}
