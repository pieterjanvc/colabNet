#' Run the colabNet Shiny App
#'
#' @import shiny dplyr stringr tidyr purrr visNetwork pool plotly
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom RSQLite SQLite
#' @importFrom DT DTOutput renderDT datatable dataTableProxy replaceData
#'
#' @return Start the Shiny app
#' @export
#'
colabNet_v2 <- function(colabNetDB) {
  # ///////////////
  # ---- DATA ----
  # //////////////

  # Setup for functions in the package
  dbSetup(colabNetDB, checkSchema = T)

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
      file.info(colabNetDB)$mtime
    },
    valueFunc = function() {
      print("Precompute shared data ...")
      auIDs <- tbl(pool, "author") |>
        filter(authorOfInterest == 1) |>
        pull(auID)
      difftree <- diffTree(auIDs, pruneDuplicates = T)
      plotData <- plotDiffTree(difftree)

      authorsimscore <- authorSimScore(difftree)
      print("... finished")
      return(list(
        auIDs = auIDs,
        plotData = plotData,
        authorsimscore = authorsimscore
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
                textInput("firstName", "First name"),
                checkboxInput(
                  "includeInitials",
                  "Extend search with initials (broader)"
                ),
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
          "This app was cretaed to support the Harvard Medical School BBS",
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

    output$networkPlot <- renderVisNetwork({
      nodes <- tbl(pool, "author") |>
        filter(authorOfInterest == 1, auID != 163) |>
        left_join(tbl(pool, "authorName") |> filter(default), by = "auID") |>
        select(id = auID, lastName, firstName, collectiveName) |>
        collect() |>
        mutate(
          label = case_when(
            !is.na(collectiveName) ~ collectiveName,
            is.na(firstName) ~ lastName,
            TRUE ~ sprintf("%s\n%s", lastName, firstName)
          )
        ) |>
        select(id, label)

      edges <- isolate(preCompData()$authorsimscore) |>
        transmute(
          from = auID1,
          to = auID2,
          width = simScore / max(simScore),
          color = colorRamp(c("#feffb3", "#12b725"))(width) |>
            rgb(maxColorValue = 255),
          width = width * 8
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
        )
    })

    # ---- Colab MeSH Tree ----

    # Shared data will only refresh when user refreshes app
    plotData <- isolate(preCompData()$plotData)

    output$meshTreePlot <- renderPlotly({
      plot_ly(
        type = "treemap",
        labels = plotData$meshterm,
        parents = plotData$parentMeshterm,
        marker = list(colors = plotData$colour),
        hovertext = sprintf(
          "%s<br><br>%s",
          plotData$meshterm,
          plotData$auNames
        ),
        hoverinfo = "text",
        textfont = list(
          color = textBW(plotData$colour)
        ),
        maxdepth = -1,
        source = "mtPlot"
      )
    })
    # htmlwidgets::saveWidget(fig, "D:/Desktop/PJ-Lorenzo.html")

    meshSel <- reactive({
      children <- plotData[
        event_data("plotly_click", "mtPlot")$pointNumber + 1,
      ]$leafNodeTreenum
      children <- paste0(children, "%")
      tbl(pool, "meshTree") |>
        filter(str_like(treenum, local({{ children }}))) |>
        left_join(tbl(pool, "meshLink"), by = "uid") |>
        left_join(tbl(pool, "meshTerm"), by = "meshui") |>
        left_join(tbl(pool, "mesh_article"), by = "meshui") |>
        collect()
    })

    # ---- Articles Table ----

    allArticles <- tbl(pool, "coAuthor") |>
      filter(auID %in% local(isolate(preCompData()$auIDs))) |>
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
    nArticles <- length(unique(allArticles$artID))

    proxy <- dataTableProxy("articleTable")
    output$articleTable <- renderDT(
      {
        allArticles |> select(-arID)
      },
      rownames = F,
      escape = F
    )

    observeEvent(meshSel(), {
      toFilter <- unique(meshSel()$arID)

      if (length(toFilter) == nArticles) {
        articles <- allArticles
      } else {
        articles <- allArticles |> filter(arID %in% toFilter)
      }

      replaceData(proxy, articles |> select(-arID), rownames = F)
    })

    # ---- DATA TAB ----
    # /////////////////////////

    # ---- Existing authors ----
    authorList <- reactive({
      tbl(pool, "author") |>
        filter(authorOfInterest == 1) |>
        left_join(
          tbl(pool, "authorName"),
          by = "auID"
        ) |>
        collect() |>
        arrange(lastName)
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
      if (input$auID == "0") return(data.frame())
      # Reset UI
      pubmedSearch(NULL)
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
    pubmedSearch <- reactiveVal()

    # Search by author name
    observe({
      if (str_trim(input$lastName) == "" | str_trim(input$firstName) == "") {
        elementMsg(
          "pubmedByAuthor",
          "You need to provide both last name and first name"
        )
        return(NULL)
      }

      # Don't allow clicking button again while searching
      disable("pubmedByAuthor")

      # If the author is not in the database, set to unknown
      inDB <- authorList() |>
        filter(
          simpleText(lastName) %in% simpleText(input$lastName),
          simpleText(firstName) %in%
            simpleText(input$firstName) |
            simpleText(initials) %in% simpleText(input$firstName)
        )

      if (nrow(inDB) == 0) {
        updateSelectInput(session, "auID", selected = "0")
      } else {
        updateSelectInput(
          session,
          "auID",
          selected = as.character(inDB$auID[1])
        )
      }

      # Get info from Pubmed
      author <- ncbi_author(input$lastName, input$firstName, showWarnings = F)

      # If no author found
      if (length(author$lastName) == 0) {
        elementMsg(
          "pubmedByAuthor",
          "No results for and author with provided first and last name"
        )
        enable("pubmedByAuthor")
        pubmedSearch(NULL)
        return(NULL)
      }

      # Look for articles in Pubmed
      search <- ncbi_authorArticleList(
        author$lastName,
        author$firstName,
        author$initials,
        str_split(input$PMIDs, ",")[[1]] |> str_trim(),
        input$includeInitials
      )

      if (search$statusCode == 0) {
        elementMsg(
          "pubmedByAuthor",
          sprintf(
            "This search yielded too many (%i) results. Please add PMIDs",
            search$n
          )
        )
        df <- NULL
      } else if (search$statusCode == 1 & input$includeInitials) {
        elementMsg(
          "pubmedByAuthor",
          "It's possible that this name is ambiguous as many results were found. 
          Consider limiting by PMID to only return relevant articles."
        )
        df <- search$articles
      } else {
        elementMsg("pubmedByAuthor")
        df <- search$articles
      }

      df <- df |> filter(!PMID %in% authorArticles()$PMID)

      if (nrow(df) == 0) {
        elementMsg(
          "pubmedByAuthor",
          "No new articles found that aren't already 
          in the database",
          "info"
        )
      }

      enable("pubmedByAuthor")
      pubmedSearch(df)
    }) |>
      bindEvent(input$pubmedByAuthor)

    auArtList <- reactiveVal()

    # Table that shows author articles
    output$authorArticleList <- renderDT(
      {
        data.frame(
          PMID = character(),
          InDB = character(),
          Title = character(),
          Info = character()
        )
      },
      escape = F
    )

    authorArticleList_proxy <- dataTableProxy("authorArticleList")

    observe({
      # Existing articles in DB for this author
      if (nrow(authorArticles()) > 0) {
        existing <- authorArticles() |>
          mutate(inDB = "YES") |>
          select(PMID, inDB, year, title, journal, authors)
      } else {
        existing <- data.frame()
      }

      # New articles from Pubmed Search if button was clicked
      if (is.null(pubmedSearch())) {
        new <- data.frame()
      } else {
        new <- pubmedSearch() |>
          mutate(
            inDB = "NO",
            year = str_extract(date, "^[^/]+") |> as.integer()
          ) |>
          select(PMID, inDB, year, title, journal, authors)
      }

      df <- bind_rows(existing, new)

      req(nrow(df) > 0)

      # Resulting tables combining new and existing articles
      df <- df |>
        transmute(
          PMID = sprintf(
            '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
            PMID,
            PMID
          ),
          InDB = inDB,
          Title = title,
          Info = sprintf(
            "%s | <i>%s</i> (%s)",
            relevantAuthors(authors, input$lastName),
            journal,
            year
          )
        )

      replaceData(authorArticleList_proxy, df)
      auArtList(df)
    })

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
      new <- ncbi_publicationDetails(noHTML, input$lastName)
      new <- dbAddAuthorPublications(new)
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
      deleted <- dbDeleteArticle(arIDs)
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
