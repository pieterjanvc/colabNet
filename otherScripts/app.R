# ///////////////
# ---- DATA ----
# //////////////

if (!exists("colabNetDB")) {
  print("DEV TEST")
  colabNetDB <- "../local/test.db"
  colabNetDB <- "D:/Desktop/PGG.db"
}

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
        wellPanel(
          tags$h3("Articles in database"),
          selectInput("auID", "Author", choices = NULL),
          uiOutput("alternativeNames"),
          fluidRow(
            DTOutput("authorInDB")
          ),
          actionButton("artDel", "Remove selected articles")
        ),
        wellPanel(
          tags$h3("Find articles on Pubmed"),
          fluidRow(column(6,textInput("lastName", "Last name")),
          column(6,textInput("firstName", "First name + middle initials"))),
          fluidRow(
            column(6,
                   textAreaInput(
                     "PMIDs",
                     "(optional) Limit search by PMID (comma separated)"
                   )
                   ),
            column(6,tags$i(HTML(
            "Use the first name and any middle name initials the author uses to publish.",
            "<ul><li>For a more specific search use full first name and then middle initials: ",
            "Joseph E Murray &rarr; Joseph E</li><li>For a broader search, use initals only",
            "Joseph E Murray &rarr; JE</li></ul>"
          )))),

         actionButton("pubmedByAuthor", "Search Pubmed"), br(), br(),
          DTOutput("authorSearch"),
          actionButton("artAdd", "Add selected articles")
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
        c(newVals$auID, "0"),
        c(paste(newVals$lastName, newVals$firstName, sep = ", "), "Not in DB")
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
    # updateTextInput(session, "lastName", value = default$lastName)
    # updateTextInput(session, "firstName", value = default$firstName)
    # updateTextInput(session, "PMIDs", value = "")

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

  # ---- EXISTING ARTICLES BY AUTHOR ---
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

  observeEvent(input$auID, {
    print(input$auID)
    elementMsg("pubmedByAuthor")

    authors <- tbl(pool, "coAuthor") |>
      group_by(arID) |>
      filter(any(auID == local(as.integer(input$auID)))) |>
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
      left_join(authors, by = "arID")

    articlesInDB(df)
  }, ignoreInit = T, ignoreNULL = F)

  observeEvent(articlesInDB(), {
    if(is.null(articlesInDB()) || nrow(articlesInDB()) == 0){
      replaceData(authorInDB_proxy, emptyTable, rownames = F)
      return()
    }

    lastName <- tbl(pool, "author") |>
      filter(authorOfInterest == 1, auID == local(input$auID)) |>
      left_join(
        tbl(pool, "authorName") |> filter(default == 1),
        by = "auID"
      ) |> pull(lastName)

    print(sprintf("UPDATE FOR %s", lastName))

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
  }, ignoreNULL = F)

  # ---- NEW ARTICLE SEARCH ---

  # Table that shows author articles
  output$authorSearch <- renderDT(
    {
      emptyTable
    },
    escape = F,
    rownames = F
  )

  authorSearch_proxy <- dataTableProxy("authorSearch")

  searchResults <- reactiveVal(list(articles = NULL, author = NULL))

  observeEvent(
    input$pubmedByAuthor,
    {
      # Check input
      if (str_trim(input$lastName) == "" | str_trim(input$firstName) == "") {
        elementMsg(
          "pubmedByAuthor",
          "You need to provide both last name and first name"
        )
        replaceData(authorSearch_proxy, emptyTable, rownames = F)
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
      ) |> filter(group == 1) |> slice(1)

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
        replaceData(authorSearch_proxy, emptyTable, rownames = F)
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

      df <- df |> filter(!PMID %in% {{ existing }})

      if (nrow(df) == 0) {
        elementMsg(
          "pubmedByAuthor",
          "No new articles found that aren't already in the database (see above)",
          "info"
        )
      } else if (nrow(df) > 0 & length(existing) > 0) {
        elementMsg(
          "pubmedByAuthor",
          sprintf(" %i articles for this author are already in the database (see above) and are not listed in the result",
                  length(existing)),
          "info"
        )
      }

      if(length(existing) > 0){
        toSelect = inDB |> filter(default == 1) |> pull(auID) |> as.character()
      } else {
        toSelect = "0"
      }

      updateSelectInput(session, "auID", selected = toSelect)

      enable("pubmedByAuthor")
      searchResults(list(articles = df, author = author))
    },
    ignoreInit = T
  )

  observeEvent(searchResults(), {
    if(is.null(searchResults()$articles)){
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
    new <- ncbi_publicationDetails(
      PMIDs,
      searchResults()$author$lastName,
      searchResults()$author$firstName,
      searchResults()$author$initials
    )

    new <- dbAddAuthorPublications(new, dbInfo = localCheckout(pool))

    # Remove added articels from search results
    searchResults(list(
      articles = searchResults()$articles |> filter(!PMID %in% new$PMID),
      author = searchResults()$author)
      )

    # Because of lazy eval we have to make a switch of inputs to update the table
    if(input$auID == new$auID[1]) {
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
