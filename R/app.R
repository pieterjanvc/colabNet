#' Run the colabNet Shiny App
#'
#' @import shiny dplyr stringr tidyr purrr visNetwork pool plotly
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom RSQLite SQLite
#' @importFrom DT DTOutput renderDT datatable dataTableProxy replaceData
#'
#' @return Start the Shiny app
#'
#' @export
#'
colabNet <- function(colabNetDB) {
  # ///////////////
  # ---- DATA ----
  # //////////////

  # Setup for functions in the package
  dbSetup(colabNetDB, checkSchema = F)

  # Pool for the Shiny app
  pool <- dbPool(SQLite(), dbname = colabNetDB)

  onStop(function() {
    poolClose(pool)
  })

  # Precompute data
  preCompData <- reactivePoll(
    5000,
    NULL,
    checkFunc = function() {
      if (!pool::dbIsValid(pool)) {
        return(NULL)
      }

      tbl(pool, "updateData") |>
        arrange(desc(uID)) |>
        head(1) |>
        pull(uID)
    },
    valueFunc = function() {
      print("Precomuting ...")

      auIDs <- tbl(pool, "author") |>
        filter(authorOfInterest == 1) |>
        pull(auID)

      if (length(auIDs) == 0) {
        return(list(
          auIDs = NULL,
          plotData = NULL,
          authorsimscore = NULL,
          allArticles = NULL
        ))
      }

      allArticles <- tbl(pool, "coAuthor") |>
        filter(auID %in% local(auIDs)) |>
        distinct() |>
        left_join(tbl(pool, "article"), by = "arID") |>
        left_join(
          tbl(pool, "authorName") |>
            group_by(auID) |>
            filter(default) |>
            ungroup(),
          by = "auID"
        ) |>
        select(arID, PMID, lastName, month, year, title, journal) |>
        arrange(desc(PMID)) |>
        collect() |>
        mutate(
          PMID = sprintf(
            '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
            PMID,
            PMID
          )
        )

      papermesh <- dbPaperMesh(auIDs)
      meshtree <- dbMeshTree(papermesh)
      papermeshtree <- paperMeshTree(papermesh, meshtree)
      # Add author names
      au <- tbl(pool, "author") |>
        filter(auID %in% local(unique(papermeshtree$auID))) |>
        select(auID) |>
        left_join(
          tbl(pool, "authorName") |> filter(default == 1),
          by = "auID"
        ) |>
        collect() |>
        rowwise() |>
        mutate(name = paste(lastName, firstName, sep = ", ")) |>
        select(auID, name)

      papermeshtree <- papermeshtree |>
        left_join(
          au |> select(auID, name),
          by = "auID"
        ) |>
        mutate(
          name = ifelse(nPapers == 0, "", name)
        )

      plotData <- treemapData(papermeshtree)
      zooscore <- zooScore(papermeshtree)

      print("... finished")
      return(list(
        auIDs = auIDs,
        plotData = plotData,
        authorsimscore = zooscore,
        allArticles = allArticles
      ))
    }
  )

  # Function to extract relevant authors (first, last and of interest) from list
  relevantAuthors <- function(authors, lastName) {
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

  # //////////////
  # ---- UI ----
  # /////////////

  ui <- fluidPage(
    useShinyjs(),
    fluidRow(column(
      12,
      tabsetPanel(
        tabPanel(
          "Exploration",
          fluidRow(
            fluidRow(column(
              12,
              h3("Similarity between researchers based on article MeSH terms")
            )),
            fluidRow(column(
              12,
              tabsetPanel(
                tabPanel(
                  "Network",
                  visNetworkOutput("networkPlot", height = "60vh"),
                  value = "networkTab"
                ),
                tabPanel(
                  "MeSH Tree",
                  plotlyOutput("meshTreePlot", height = "60vh"),
                  value = "networkTab"
                )
              )
            )),
            fluidRow(column(12, DTOutput("articleTable")))
          ),
          value = "exploration"
        ),
        tabPanel(
          "Data",
          fluidRow(
            column(
              5,
              wellPanel(
                tags$h3("Authors in database"),
                selectInput("auID", "Author", choices = NULL),
                uiOutput("alternativeNames")
              ),
              wellPanel(
                tags$h3("Add / Remove articles from database"),
                actionButton("artAdd", "Add selected articles"),
                actionButton("artDel", "Remove selected articles")
              )
            ),
            column(
              7,
              wellPanel(
                tags$h3("Find articles on Pubmed"),
                textInput("lastName", "Last name"),
                textInput("firstName", "First name + middle initials"),
                tags$i(HTML(
                  "Use the first name and add any middle name initials the author uses to publish.",
                  "<br>- For a more specific search use full first name and then middle initials: ",
                  "Joseph E Murray &rarr; Joseph E<br>- For a broader search, use initals only",
                  "Joseph E Murray &rarr; JE"
                )),
                br(),
                br(),
                textAreaInput(
                  "PMIDs",
                  "(optional) Limit search by PMID (comma separated)"
                ),
                actionButton("pubmedByAuthor", "Search Pubmed")
              ),
            )
          ),
          fluidRow(
            DTOutput("authorArticleList")
          ),
          value = "exploration"
        )
      )
    )),
    fluidRow(column(
      12,
      tags$footer(
        p(
          "This app was created to support the Harvard Medical School BBS",
          tags$a(
            "Program in Genetics and Genomics",
            href = "https://projects.iq.harvard.edu/pgg",
            target = "_blank"
          )
        ),
        p(
          "Content manager: Lorenzo Gesuita -",
          tags$a(
            "lorenzo_gesuita@hms.harvard.edu",
            href = "mailto:lorenzo_gesuita@hms.harvard.edu"
          ),
          "| App creator: PJ van Camp -",
          tags$a(
            "pjvancamp@hms.harvard.edu",
            href = "mailto:pjvancamp@hms.harvard.edu"
          )
        ),
        style = "width: 100%;margin: auto;text-align: center;background-color: #f6f6f6;
    color:#787878;border-top: 0.2rem solid;"
      )
    ))
  )

  # //////////////////
  # ---- SERVER ----
  # /////////////////

  server <- function(input, output, session) {
    # ---- EXPLORATION TAB ----
    # /////////////////////////

    # ---- Colab Network ----

    #CHECK BOTH DF LATER TO SHARE BETTER
    coPub <- tbl(pool, "author") |>
      filter(authorOfInterest == 1) |>
      left_join(
        tbl(pool, "coAuthor"),
        by = "auID"
      ) |>
      group_by(arID) |>
      filter(n() > 1) |>
      ungroup() |>
      left_join(
        tbl(pool, "authorName") |> filter(default) |> select(-anID),
        by = "auID"
      ) |>
      collect() |>
      mutate(name = sprintf("%s %s", lastName, firstName))

    if (nrow(coPub) > 0) {
      pairInfo <- coPub |>
        group_by(arID) |>
        reframe(
          as.data.frame(combn(auID, 2) |> t())
        ) |>
        rename(from = V1, to = V2) |>
        group_by(from, to) |>
        mutate(id = cur_group_id()) |>
        ungroup()
    } else {
      pairInfo <- data.frame()
    }

    output$networkPlot <- renderVisNetwork({
      req(nrow(coPub) > 0)
      nodes <- coPub |> select(id = auID, label = name) |> distinct()

      edges <- pairInfo |>
        group_by(id, from, to) |>
        summarise(width = n(), label = as.character(n()), .groups = "drop") |>
        mutate(
          color = case_when(
            width == 1 ~ "#ffcc33",
            width == 2 ~ "#ee6600",
            width > 2 ~ "#990000",
          )
        )

      # Create a simple visNetwork graph
      visNetwork(nodes, edges) |>
        visNodes(
          size = 20,
          color = list(background = "lightblue", border = "blue"),
          font = list(background = rgb(1, 1, 1, 0.8))
        ) |>
        visEdges(smooth = T) |>
        visPhysics(
          barnesHut = list(
            # gravitationalConstant = -2000,  # Optional: adjust gravity
            # centralGravity = 0.3,           # Optional: adjust central gravity
            springLength = 200, # Optional: adjust spring length
            # springConstant = 0.01,          # Optional: adjust spring constant
            # damping = 0.4,                  # Optional: adjust damping
            repulsion = 1000 # Increased repulsion value
          )
        ) |>
        visEvents(
          select = "function(data) {
                  Shiny.onInputChange('coPub_selection', data);
                  ;}"
        )
    })

    observeEvent(
      input$coPub_selection,
      {
        print("HI")
        req(input$coPub_selection)
        req(!is.null(preCompData()$allArticles))
        clicked <- input$coPub_selection

        if (class(clicked) == "NULL") {
          replaceData(
            proxy,
            preCompData()$allArticles |> select(-arID),
            rownames = F
          )
          return()
        }

        toFilter <- pairInfo |>
          filter(id %in% unlist(clicked$edges)) |>
          pull(arID)

        if (length(toFilter) == 0) {
          replaceData(
            proxy,
            preCompData()$allArticles |> select(-arID),
            rownames = F
          )
          return()
        }

        replaceData(
          proxy,
          preCompData()$allArticles |>
            filter(arID %in% toFilter) |>
            select(-arID) |>
            group_by(PMID) |>
            mutate(lastName = paste(sort(lastName), collapse = " & ")) |>
            ungroup() |>
            distinct(),
          rownames = F
        )

        ## Keep to see what the clicked data list contains if needed in future
        # showModal(modalDialog(
        #   HTML(paste(
        #     capture.output(print(clicked)),
        #     collapse = "<br>"
        #   ))
        # ))
      },
      ignoreNULL = F
    )

    # ---- Colab MeSH Tree ----

    output$meshTreePlot <- renderPlotly({
      req(preCompData()$plotData)

      plotData <- preCompData()$plotData

      boxText <- str_wrap(
        paste(plotData$meshSum, plotData$meshterm, sep = " | "),
        12
      )
      boxText <- ifelse(
        plotData$hasChildren,
        paste(boxText, "<b>+</b>"),
        boxText
      )

      plot_ly(
        type = "treemap",
        ids = plotData$branchID,
        parents = plotData$parentBranchID,
        labels = ifelse(
          is.na(plotData$meshSum),
          plotData$meshterm,
          paste(plotData$meshSum, plotData$meshterm, sep = " | ")
        ),
        text = boxText,
        values = plotData$treemapVal,
        marker = list(colors = treemapColour(plotData$meshSum)),
        textinfo = "text",
        hovertext = plotData$authors,
        hoverinfo = "text",
        maxdepth = 3,
        source = "treemap"
      )
    })
    # htmlwidgets::saveWidget(fig, "D:/Desktop/PJ-Lorenzo.html")

    meshSel <- reactive({
      selected <- preCompData()$plotData[
        event_data("plotly_click", "treemap")$pointNumber + 1,
      ]$branchID
      req(length(selected) > 0)
      children <- tbl(pool, "meshTree") |>
        filter(mtrID == as.integer(selected)) |>
        pull(treenum)
      children <- paste0(children[1], "%")
      tbl(pool, "meshTree") |>
        filter(str_like(treenum, local({{ children }}))) |>
        left_join(tbl(pool, "meshLink"), by = "uid") |>
        left_join(tbl(pool, "meshTerm"), by = "meshui") |>
        left_join(tbl(pool, "mesh_article"), by = "meshui") |>
        collect()
    })

    # ---- Articles Table ----

    proxy <- dataTableProxy("articleTable")
    output$articleTable <- renderDT(
      {
        req(!is.null(preCompData()$allArticles))
        preCompData()$allArticles |> select(-arID)
      },
      rownames = F,
      escape = F
    )

    observeEvent(meshSel(), {
      toFilter <- unique(meshSel()$arID)

      if (length(toFilter) == length(unique(preCompData()$allArticles$artID))) {
        articles <- preCompData()$allArticles
      } else {
        articles <- preCompData()$allArticles |> filter(arID %in% toFilter)
      }

      replaceData(proxy, articles |> select(-arID), rownames = F)
    })

    # ---- DATA TAB ----
    # /////////////////////////

    # ---- Existing authors ----
    authorList <- reactiveVal({
      tbl(pool, "author") |>
        filter(authorOfInterest == 1) |>
        left_join(
          tbl(pool, "authorName"),
          by = "auID"
        ) |>
        collect() |>
        arrange(lastName, firstName)
    })

    observeEvent(authorList(), {
      newVals <- authorList() |> filter(default == 1)
      updateSelectInput(
        session,
        "auID",
        choices = setNames(
          c(0, newVals$auID),
          c("Unknown", paste(newVals$lastName, newVals$firstName, sep = ", "))
        ),
        selected = newVals$auID[1]
      )
    })

    output$alternativeNames <- renderUI({
      auNames <- tbl(pool, "authorName") |>
        filter(auID == local(input$auID)) |>
        collect()

      # Set the Pubmed Search to match selection
      default <- auNames |> filter(default == 1)
      updateTextInput(session, "lastName", value = default$lastName)
      updateTextInput(session, "firstName", value = default$firstName)
      updateTextInput(session, "PMIDs", value = "")

      altNames <- auNames |> filter(default == 0)

      if (nrow(altNames) == 0) {
        return(NULL)
      } else {
        return(tags$i(
          sprintf(
            "Also published under: %s",
            paste(
              altNames$lastName,
              altNames$firstName,
              sep = ", ",
              collapse = " | "
            )
          )
        ))
      }
    })

    authorArticles <- reactive({
      if (input$auID == "0") {
        return(data.frame())
      }
      # Reset UI
      # pubmedSearch$articles = NULL
      elementMsg("pubmedByAuthor")

      authors <- tbl(pool, "coAuthor") |>
        group_by(arID) |>
        filter(any(local(input$auID) == auID)) |>
        ungroup() |>
        left_join(
          tbl(pool, "authorName") |>
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

      articles <- tbl(pool, "article") |>
        filter(arID %in% local(authors$arID)) |>
        collect() |>
        left_join(authors, by = "arID")
    })

    # ---- Search for new ----
    emptyTable <- data.frame(
      InDB = character(),
      PMID = character(),
      Title = character(),
      Info = character()
    )

    # Table that shows author articles
    output$authorArticleList <- renderDT(
      {
        emptyTable
      },
      escape = F, rownames = F
    )

    authorArticleList_proxy <- dataTableProxy("authorArticleList")

    nSearches <- reactiveVal(0)

    observeEvent(c(input$auID, input$pubmedByAuthor), {
      print("RUN ARTICLES")
      if (input$auID == "0") {
        replaceData(authorArticleList_proxy, emptyTable, rownames = F)
        return()
      }

      # New author search
      if (input$pubmedByAuthor > nSearches()) {
        if (str_trim(input$lastName) == "" | str_trim(input$firstName) == "") {
          elementMsg(
            "pubmedByAuthor",
            "You need to provide both last name and first name"
          )
          replaceData(authorArticleList_proxy, emptyTable, rownames = F)
          return()
        }

        # Don't allow clicking button again while searching
        disable("pubmedByAuthor")

        # If the author is not in the database, set to unknown

        inDB <- tbl(pool, "authorName") |>
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
        )[
          1,
        ]

        # If no author found
        if (length(author$lastName) == 0) {
          elementMsg(
            "pubmedByAuthor",
            sprintf(
              "No results for the author with last name '%s' and first name '%s'",
              input$lastName,
              input$firstName
            )
          )
          enable("pubmedByAuthor")
          updateSelectInput(session, "auID", selected = "0")
          replaceData(authorArticleList_proxy, emptyTable, rownames = F)
          return()
        }

        # Look for articles in Pubmed
        search <- ncbi_authorArticleList(
          input$lastName,
          input$firstName,
          str_split(input$PMIDs, ",")[[1]] |> str_trim(),
        )

        if (!search$success) {
          elementMsg(
            "pubmedByAuthor",
            sprintf(
              "This search yielded too many (%i) results. Please use PMIDs instead",
              search$n
            )
          )
          df <- emptyTable
        } else if (search$n == 0) {
          elementMsg(
            "pubmedByAuthor",
            sprintf(
              "No results for the author with last name '%s' and first name '%s'",
              input$lastName,
              input$firstName
            )
          )
          df <- emptyTable
        } else {
          elementMsg("pubmedByAuthor")
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

        existing <- tbl(pool, "article") |>
          filter(PMID %in% local(df$PMID)) |>
          pull(PMID)

        if (nrow(df) == length(existing)) {
          elementMsg(
            "pubmedByAuthor",
            "No new articles found that aren't already
            in the database",
            "info"
          )
        }

        df <- df |>
          mutate(
            InDB = ifelse(PMID %in% existing, "YES", "NO"),
            PMID = sprintf(
              '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
              PMID,
              PMID
            )
          )

        toSelect <- ifelse(
          length(existing) > 0,
          inDB |> filter(default == 1) |> pull(auID) |> as.character(),
          "0"
        )

        updateSelectInput(session, "auID", selected = toSelect)

        enable("pubmedByAuthor")

        replaceData(authorArticleList_proxy, df %>% select(InDB, PMID, Title, Info), rownames = F)
        nSearches(nSearches() + 1)
      } else {
        elementMsg("pubmedByAuthor")

        authors <- tbl(pool, "coAuthor") |>
          group_by(arID) |>
          filter(any(local(as.integer(input$auID)) == auID)) |>
          ungroup() |>
          left_join(
            tbl(pool, "authorName") |>
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

        df <- tbl(pool, "article") |>
          filter(arID %in% local(authors$arID)) |>
          collect() |>
          left_join(authors, by = "arID") |>
          transmute(
            InDB = "YES",
            PMID = sprintf(
              '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
              PMID,
              PMID
            ),
            Title = title,
            Info = relevantAuthors(
              authors,
              tbl(pool, "authorName") |>
                filter(default == 1, auID == as.integer(input$auID)) |>
                pull(lastName)
            )
          )
        print("RUN ARTICLES2")
        replaceData(authorArticleList_proxy, df %>% select(InDB, PMID, Title, Info), rownames = F)
      }
    }, ignoreInit = T)


    observe({
      PMIDs <- auArtList()[input$authorArticleList_rows_selected, ] |>
        filter(InDB == "NO") |>
        pull(PMID)

      noHTML <- PMIDs |>
        str_extract(".*>(.+)<", group = 1)

      if (length(PMIDs) == 0) {
        showNotification("No articles to add", type = "message")
      }

      req(length(PMIDs) > 0)
      disable("artAdd")
      new <- ncbi_publicationDetails(
        noHTML,
        pubmedSearch$author$lastName,
        pubmedSearch$author$firstName,
        pubmedSearch$author$initials
      )

      new <- dbAddAuthorPublications(new, dbInfo = localCheckout(pool))

      updatedTable <- auArtList() |>
        mutate(
          InDB = ifelse(PMID %in% PMIDs, "YES", InDB)
        )
      replaceData(authorArticleList_proxy, updatedTable)
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
      arIDs <- tbl(pool, "article") |>
        filter(PMID %in% local(noHTML)) |>
        pull(arID)

      deleted <- dbDeleteArticle(arIDs, dbInfo = localCheckout(pool))

      updatedTable <- auArtList() |>
        mutate(
          InDB = ifelse(PMID %in% PMIDs, "NO", InDB)
        )
      replaceData(authorArticleList_proxy, updatedTable)
      enable("artDel")
    }) |>
      bindEvent(input$artDel)
  }

  shinyApp(ui, server)

}
