# ///////////////
# ---- DATA ----
# //////////////

if (!exists("colabNetDB")) {
  print("DEV TEST")
  file.copy("../data/PGG_full.db", "../local/dev.db", overwrite = T)
  colabNetDB <- "../local/dev.db"

  # colabNetDB <- "C:/Users/pj/Desktop/dev.db"
  # file.remove(colabNetDB)
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

    authors <- tbl(pool, "author") |>
      filter(authorOfInterest == 1) |>
      left_join(
        tbl(pool, "authorName") |>
          group_by(auID) |>
          filter(default) |>
          ungroup(),
        by = "auID"
      ) |>
      select(auID, lastName, firstName, initials) |>
      collect()

    if (length(nrow(authors)) == 0) {
      return(list(
        authors = NULL,
        plotData = NULL,
        overlapscore = NULL,
        allArticles = NULL
      ))
    }

    allArticles <- tbl(pool, "coAuthor") |>
      filter(auID %in% local(authors$auID)) |>
      distinct() |>
      left_join(tbl(pool, "article"), by = "arID") |>
      collect() |>
      left_join(authors, by = "auID") |>
      select(
        auID,
        arID,
        PMID,
        lastName,
        firstName,
        month,
        year,
        title,
        journal
      ) |>
      arrange(desc(PMID)) |>
      collect() |>
      mutate(
        PMID = sprintf(
          '<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>',
          PMID,
          PMID
        ),
        name = paste(lastName, firstName, sep = ", "),
        month = as.integer(month)
      )

    papermesh <- dbPaperMesh(authors$auID)
    meshtree <- dbMeshTree(papermesh)
    papermeshtree <- paperMeshTree(papermesh, meshtree)

    # Add author names
    au <- tbl(pool, "author") |>
      filter(auID %in% local(unique(papermeshtree$auID))) |>
      select(auID) |>
      left_join(tbl(pool, "authorName") |> filter(default == 1), by = "auID") |>
      collect() |>
      rowwise() |>
      mutate(name = paste(lastName, firstName, sep = ", ")) |>
      select(auID, name)

    papermeshtree <- papermeshtree |>
      left_join(au |> select(auID, name), by = "auID") |>
      mutate(name = ifelse(nPapers == 0, "", name))

    overlapscore <- zooScore(papermeshtree)

    print("... finished")
    return(
      list(
        authors = authors,
        papermeshtree = papermeshtree,
        overlapscore = overlapscore,
        allArticles = allArticles
      )
    )
  }
)

# ///////////////////////////
# ---- SHARED FUNCTIONS ----
# //////////////////////////

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

# Function to filter MeSH tree plot by top-n scoring categories
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

# Calculate the MeSH Tree overlap between author pairs
calcOverlap <- function(precompOverlap, treeFilter) {
  if (!is.null(treeFilter)) {
    precompOverlap <- precompOverlap |> filter(tree %in% {{ treeFilter }})
  }

  overlapscore <- precompOverlap |>
    group_by(au1, au2) |>
    summarise(score = sum(score), .groups = "drop") |>
    arrange(desc(score))

  names <- preCompData()$authors |>
    transmute(auID, name = sprintf("%s, %s", lastName, firstName))

  overlapscoreTable <- overlapscore |>
    left_join(
      names |>
        select(au1 = auID, author1 = name),
      by = "au1"
    ) |>
    left_join(names |> select(au2 = auID, author2 = name), by = "au2") |>
    select(author1, author2, overlapScore = score)

  return(list(score = overlapscore, table = overlapscoreTable))
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
                "Co-authors",
                visNetworkOutput("networkPlot", height = "60vh"),
                value = "tab_coAuth"
              ),
              tabPanel(
                "MeSH Tree",
                mod_meshTree_ui("meshTree_overview"),
                value = "tab_meshTree"
              ),
              tabPanel(
                "Research Overlap",
                fluidRow(
                  column(
                    4,
                    selectInput(
                      "overlapCat",
                      "Filter Scoring Category",
                      multiple = T,
                      choices = NULL,
                      selected = NULL
                    ),
                    actionButton("applyFilterCat", "Apply Filter"),
                    actionButton("removeFilterCat", "Remove Filter")
                  ),
                  column(8, DTOutput("overlapscoreTable"))
                ),
                mod_meshTree_ui("meshTree_comparison"),
                value = "tab_comparison"
              ),
              id = "tabs_exploration"
            )
          )),
          fluidRow(column(
            12,
            DTOutput("articleTable")
          ))
        ),
        value = "exploration"
      ),
      tabPanel(
        "Analysis",
        tabsetPanel(
          tabPanel(
            "Overview",
            visNetworkOutput("netAnalisisPlot", height = "60vh"),
            sliderInput(
              "analysisRange",
              "Analysis Range",
              min = 0,
              max = 1,
              value = c(0, 1),
              step = 1,
              sep = ""
            ),
            uiOutput("summaryStats")
          ),
          tabPanel(
            "Trends",
            selectInput(
              "slidingWindow",
              "Time window",
              choices = c(10, 8, 5, 3, 2, 1),
              selected = 5
            ),
            plotOutput("copubTrendsPlot", height = "80vh")
          )
        )
      ),
      tabPanel(
        "Admin",
        wellPanel(
          tags$h3("Articles in database"),
          selectInput("auID", "Author", choices = NULL),
          uiOutput("alternativeNames"),
          fluidRow(DTOutput("authorInDB")),
          actionButton("artDel", "Remove selected articles")
        ),
        wellPanel(
          fluidRow(
            column(
              6,
              tags$h3("Find articles on Pubmed"),
              textInput("lastName", "Last name"),
              textInput("firstName", "First name + middle initials"),
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
              textAreaInput("PMIDs", "Limit search by PMID (comma separated)"),
              textInput("auAff", "Filter based on affiliation (RegEx style)")
            ),
          ),

          actionButton("pubmedByAuthor", "Search Pubmed"),
          br(),
          br(),
          DTOutput("authorSearch"),
          actionButton("artAdd", "Add selected articles")
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
          fileInput("bulkImportAuthor", "CSV file", accept = ".csv"),
          tags$i(
            HTML(
              "Note that the nPubMed column below will show the number of articles ",
              "that match the author before filtering by affiliation (will be done when importing)"
            )
          ),
          div(id = "bulkImportAuthorMsg"),
          DTOutput("bulkImportTable"),
          actionButton("startBulkImport", "Start Import")
        ),
        value = "exploration"
      ),
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
  #Updates when the pre-calculated data changes
  observeEvent(preCompData(), {
    # Update the tree root categories as filter options for the comparison tree
    roots <- tbl(pool, "meshTree") |>
      filter(treenum %in% local(unique(preCompData()$overlapscore$tree))) |>
      left_join(tbl(pool, "meshLink"), by = "uid") |>
      left_join(tbl(pool, "meshTerm"), by = "meshui") |>
      collect() |>
      arrange(meshterm)

    updateSelectInput(
      session,
      "overlapCat",
      choices = setNames(roots$treenum, roots$meshterm),
      selected = NULL
    )

    # Set analysis year range slider
    updateSliderInput(
      session,
      "analysisRange",
      min = min(preCompData()$allArticles$year),
      max = max(preCompData()$allArticles$year),
      value = c(
        min(preCompData()$allArticles$year),
        max(preCompData()$allArticles$year)
      )
    )
  })

  # ---- EXPLORATION TAB ----
  # /////////////////////////

  # ---- Articles Table ----

  # When switching tabs, the shared article table has to be updated according
  #  to the data on the active tab
  observeEvent(input$tabs_exploration, {
    if (input$tabs_exploration == "tab_coAuth") {
      arIDs <- coPub()$edgeArticles |>
        filter(edgeID %in% unlist(input$coPub_selection$edges)) |>
        pull(arID)

      setArticleTable(arIDs, merged = T)
    } else if (input$tabs_exploration == "tab_meshTree") {
      setArticleTable(arIDByMesh(
        mtOverviewSelected()$selected,
        mtOverviewSelected()$mtrIDs
      ))
    } else if (input$tabs_exploration == "tab_comparison") {
      # We're only looking at two authors
      auIDs <- overlapData()[input$overlapscoreTable_rows_selected, ] |>
        select(au1, au2) |>
        unlist()

      setArticleTable(
        arIDByMesh(
          mtComparisonSelected()$selected,
          mtComparisonSelected()$mtrIDs
        ),
        auIDs = auIDs
      )
    } else {
      stop("Tab issue:", input$tabs_exploration)
    }
  })

  output$articleTable <- renderDT(
    {
      req(!is.null(preCompData()$allArticles))
      preCompData()$allArticles |>
        select(PMID, name, month, year, title, journal)
    },
    rownames = F,
    escape = F
  )

  artTableProxy <- dataTableProxy("articleTable")

  # Function to update the content of the articles table using the proxy
  #  Note that this needs a reactive environment as it contains reactive vals inside
  setArticleTable <- function(arIDs, auIDs = c(), merged = F) {
    articles <- preCompData()$allArticles

    if (length(arIDs) > 0) {
      articles <- articles |> filter(arID %in% {{ arIDs }})
    }

    if (length(auIDs) > 0) {
      articles <- articles |> filter(auID %in% {{ auIDs }})
    }

    if (merged) {
      articles <- articles |>
        group_by(arID) |>
        mutate(name = paste(lastName, collapse = " & ")) |>
        ungroup()
    }

    replaceData(
      artTableProxy,
      articles |>
        select(PMID, name, month, year, title, journal) |>
        distinct(),
      rownames = F
    )
  }

  # ---- Colab Network ----

  # Nodes and Edges for the co-publication network
  coPub <- reactive({
    req(nrow(preCompData()$allArticles) > 0)
    copubGraphElements(preCompData()$allArticles)
  })

  # co-publication network
  output$networkPlot <- renderVisNetwork({
    req(nrow(coPub()$nodes) > 0)

    # Ignore authors who are not connected to anyone here
    nodes <- coPub()$nodes |>
      filter(id %in% c(coPub()$edges$from, coPub()$edges$to))

    edges <- coPub()$edges |>
      # group_by(id, from, to) |>
      # summarise(width = n(), label = as.character(n()), .groups = "drop") |>
      mutate(
        width = weight,
        label = as.character(weight),
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
          springLength = 200,
          # Optional: adjust spring length
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

  # When a node or edge is selected in the co publication network
  observeEvent(
    input$coPub_selection,
    {
      arIDs <- coPub()$edgeArticles |>
        filter(edgeID %in% unlist(input$coPub_selection$edges)) |>
        pull(arID)

      setArticleTable(arIDs, merged = T)
    },
    ignoreNULL = F
  )

  # ---- Colab MeSH Tree ----

  mtOverviewSelected <- mod_meshTree_server(
    "meshTree_overview",
    papermeshtree = reactive(preCompData()$papermeshtree)
  )

  observeEvent(mtOverviewSelected(), {
    setArticleTable(arIDByMesh(
      branchID = mtOverviewSelected()$selected,
      mtrIDs = mtOverviewSelected()$mtrIDs
    ))
  })

  # Get article IDs based on a branch in the MeSH tree that is selected
  #  provode a list of mtrIDs if the tree is being filtered
  arIDByMesh <- function(branchID, mtrIDs = NULL) {
    if (length(branchID) > 1) {
      stop("You can only select one tree branch at the same time")
    }

    # This will retrun the full table when supplied to setArticleTable
    if (length(branchID) == 0 || (branchID == 0 & length(mtrIDs) == 0)) {
      return(NULL)
    }

    # Only filter the full table for limited tree but at root (branchID = NULL)
    if (branchID == 0 & length(mtrIDs) > 0) {
      result <- tbl(pool, "meshTree") |>
        filter(mtrID %in% local(mtrIDs)) |>
        left_join(tbl(pool, "meshLink"), by = "uid") |>
        left_join(tbl(pool, "meshTerm"), by = "meshui") |>
        left_join(tbl(pool, "mesh_article"), by = "meshui") |>
        pull(arID) |>
        unique()
      return(result)
    }

    # Case where a brac has been selected
    children <- tbl(pool, "meshTree") |>
      filter(mtrID == as.integer(branchID)) |>
      pull(treenum)

    children <- paste0(children[1], "%")
    result <- tbl(pool, "meshTree") |>
      filter(str_like(treenum, local({{ children }})))
    #Filter in case the tree is limited by mtPlotLimit input
    if (length(mtrIDs) > 0) {
      result <- result |> filter(mtrID %in% local(mtrIDs))
    }

    result |>
      left_join(tbl(pool, "meshLink"), by = "uid") |>
      left_join(tbl(pool, "meshTerm"), by = "meshui") |>
      left_join(tbl(pool, "mesh_article"), by = "meshui") |>
      pull(arID) |>
      unique()
  }

  # observeEvent(event_data("plotly_click", "treemap_overview"), {
  #   branchIDs <- preCompData()$papermeshtree |>
  #     plotDataFilter(input$mtPlotLimit) |>
  #     pull(branchID)
  #   selected <- branchIDs[
  #     event_data("plotly_click", "treemap_overview")$pointNumber + 1
  #   ]
  #   setArticleTable(arIDByMesh(selected, branchIDs))
  # })

  # ---- Research Comparison ----

  # The table that shows the overlap score for pairs of authors
  output$overlapscoreTable <- renderDT(
    {
      calcOverlap(preCompData()$overlapscore, NULL)$table
    },
    rownames = F,
    selection = list(mode = "single", selected = 1),
    options = list(lengthMenu = list(c(5, 10, 15), c("5", "10", "15")))
  )

  overlapscoreTable_proxy <- dataTableProxy("overlapscoreTable")

  overlapData <- reactiveVal()

  # Update table that show the overlap score and return the raw table data
  observeEvent(
    input$applyFilterCat,
    {
      req(nrow(preCompData()$overlapscore) > 0)

      overlap <- calcOverlap(preCompData()$overlapscore, input$overlapCat)

      req(
        is.null(overlapData()) ||
          !identical(overlap$score, overlapData())
      )

      replaceData(overlapscoreTable_proxy, overlap$table, rownames = F)

      if (nrow(overlap$table) > 0) {
        DT::selectRows(overlapscoreTable_proxy, 1)
      }

      overlapData(overlap$score)
    },
    ignoreNULL = FALSE
  )

  # Remove any tree filters from the scoring categories
  observeEvent(input$removeFilterCat, {
    req(length(input$overlapCat) > 0)

    updateSelectInput(session, "overlapCat", selected = character(0))
    overlap <- calcOverlap(preCompData()$overlapscore, NULL)

    replaceData(overlapscoreTable_proxy, overlap$table, rownames = F)

    if (nrow(overlap$table) > 0) {
      DT::selectRows(overlapscoreTable_proxy, 1)
    }

    overlapData(overlap$score)
  })

  #  When a new author combination is chosen from the table
  treemapcomp <- eventReactive(input$overlapscoreTable_rows_selected, {
    req(nrow(overlapData()) > 0)
    # Get the author IDs
    auIDs <- overlapData()[input$overlapscoreTable_rows_selected, ] |>
      select(au1, au2) |>
      unlist()
    req(length(auIDs) == 2)

    # Get the treemap for the two authors
    if (length(input$overlapCat) == 0) {
      tmComp <- papermeshtreeFromAuIDs(c(auIDs[1], auIDs[2]))
    } else {
      tmComp <- papermeshtreeFromAuIDs(
        c(auIDs[1], auIDs[2]),
        roots = input$overlapCat
      )
      #TODO make sure the table filters with arIDs only found in the selected subtrees
    }

    # Update the article table with all articles for either author
    # arIDs <- preCompData()$allArticles |>
    #   filter(auID %in% {{ auIDs }}) |>
    #   pull(arID)
    # setArticleTable(arIDs = arIDs, auIDs = auIDs)

    # Return the treemap
    tmComp$papermeshtree
  })

  # MeSH tree for two author comparison
  mtComparisonSelected <- mod_meshTree_server(
    "meshTree_comparison",
    papermeshtree = treemapcomp
  )

  # Get articles in part of the author comparison MeSH tree branch selected
  observeEvent(
    mtComparisonSelected(),
    {
      req(nrow(overlapData()) > 0)

      # We're only looking at two authors
      auIDs <- overlapData()[input$overlapscoreTable_rows_selected, ] |>
        select(au1, au2) |>
        unlist()

      setArticleTable(
        arIDByMesh(
          mtComparisonSelected()$selected,
          mtComparisonSelected()$mtrIDs
        ),
        auIDs = auIDs
      )
    },
    ignoreNULL = F
  )

  # ---- ANALYSIS TAB ----
  # //////////////////////

  networkanalysis <- reactive({
    range <- input$analysisRange
    req(range[1] > 0) # Ignore the init stage
    rangeData <<- preCompData()$allArticles |>
      filter(between(year, range[1], range[2]))

    graphElements <- copubGraphElements(rangeData)
    graphStats <- copubGraphStats(graphElements)

    return(list(graphElements = graphElements, graphStats = graphStats))
  })

  output$netAnalisisPlot <- renderVisNetwork({
    edges <- networkanalysis()$graphElements$edges |>
      mutate(
        width = weight,
        label = as.character(weight),
        color = case_when(
          width == 1 ~ "#ffcc33",
          width == 2 ~ "#ee6600",
          width > 2 ~ "#990000",
        )
      )

    # Create a simple visNetwork graph
    visNetwork(networkanalysis()$graphElements$nodes, edges) |>
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
          springLength = 200,
          # Optional: adjust spring length
          # springConstant = 0.01,          # Optional: adjust spring constant
          # damping = 0.4,                  # Optional: adjust damping
          repulsion = 1000 # Increased repulsion value
        )
      )
  })

  output$summaryStats <- renderUI({
    x <- networkanalysis()$graphStats
    # average distance, ignoring unconnected
    # dis_avg <- x$distances[upper.tri(x$dis)]
    # dis_avg <- dis_avg[!is.infinite(dis_avg)]
    # dis_avg <- ifelse(length(dis_avg) == 0, 0, mean(dis_avg))

    # addSpaces <- function(int, max) {
    #   sapply(int, function(x) {
    #     if (x == 0) {
    #       return(" ")
    #     }
    #     paste(
    #       rep("&nbsp;", floor(log10(max)) - floor(log10(x)) + 1),
    #       collapse = ""
    #     )
    #   })
    # }

    authorStats <- x$authorStats |>
      left_join(
        preCompData()$authors |>
          transmute(auID, name = paste(lastName, firstName, sep = ", ")),
        by = "auID"
      ) |>
      mutate(
        papers = sprintf(
          "%i (%.2f%%)",
          nCopubs,
          colabPerc
        )
      ) |>
      arrange(desc(degree), desc(nCopubs)) |>
      select(
        name,
        connections = degree,
        `papers (% total)` = papers,
        nCopubs,
        group = membership,
        betweenness,
        closeness
      )

    tagList(
      h3("Author co-publication stats"),
      # Table with stats by author
      datatable(
        authorStats,
        rownames = F,
        options = list(
          pageLength = 5,
          # Order papers column by hidden nCopubs (0-index!)
          columnDefs = list(
            list(targets = 2, orderData = 3),
            list(targets = 3, visible = F)
          )
        ),
        escape = F
      ),
      h3("Network Summary Stats"),
      tags$ul(
        tags$li(sprintf(
          "Total number of authors in network: %i",
          x$nAuthors
        )),
        tags$li(sprintf(
          "Median number of co-publications: %i",
          median(ifelse(
            length(x$authorStats$nCopubs) == 0,
            0,
            x$authorStats$nCopubs
          ))
        )),
        tags$li(sprintf(
          "Maximum number of co-publications: %i",
          max(x$authorStats$nCopubs, 0)
        )),
        tags$li(
          sprintf(
            "Number of authors without any co-publications: %i",
            sum(x$authorStats$unconnected)
          )
        ),
        tags$li(sprintf(
          "Average distance between co-publishing authors: %.1f",
          x$distance_avg
        )),
        tags$li(sprintf(
          "Maximum distance between authors (network diameter): %i",
          x$diameter
        )),
        tags$li(sprintf(
          "How close to perfect collaboration (everyone collaborates with everyone else): %.1f%%",
          x$density * 100
        )),
        tags$li(sprintf(
          "Probability that authors who share a collborator also collaborated together (transitivity): %.1f%%",
          x$transitivity * 100
        )),
        tags$li(sprintf(
          "How easy would it be to work with anyone in the network (global efficiency): %.1f%%",
          x$globalEffiency * 100
        ))
      )
    )
  })

  # ---- Co-publication Network trends over time

  output$copubTrendsPlot <- renderPlot({
    copubTrendInfo(
      preCompData()$allArticles,
      windowSize = as.integer(input$slidingWindow)
    )$plot
  })

  # ---- ADMIN TAB ----
  # ///////////////////

  # ---- Existing authors ----
  authorList <- reactiveVal({
    tbl(pool, "author") |>
      filter(authorOfInterest == 1) |>
      left_join(tbl(pool, "authorName"), by = "auID") |>
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
        c(
          paste(newVals$lastName, newVals$firstName, sep = ", "),
          "Not in DB"
        )
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
      # elementMsg("pubmedByAuthor")

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

      lastName <- tbl(pool, "author") |>
        filter(authorOfInterest == 1, auID == local(input$auID)) |>
        left_join(
          tbl(pool, "authorName") |> filter(default == 1),
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
      ) |>
        filter(default)

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

      existing <- tbl(pool, "article") |>
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
        toSelect = inDB |> filter(default == 1) |> pull(auID) |> as.character()
      } else {
        toSelect = "0"
      }

      updateSelectInput(session, "auID", selected = toSelect)

      if (msg != "") {
        elementMsg("pubmedByAuthor", msg, "info")
      } else {
        elementMsg("pubmedByAuthor")
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

    new <- dbAddAuthorPublications(new, dbInfo = localCheckout(pool))

    # Remove added articles from search results
    searchResults(
      list(
        articles = searchResults()$articles |> filter(!PMID %in% new$PMID),
        author = searchResults()$author
      )
    )

    # Because of lazy eval we have to make a switch of inputs to update the table
    authorList({
      tbl(pool, "author") |>
        filter(authorOfInterest == 1) |>
        left_join(tbl(pool, "authorName"), by = "auID") |>
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
    arIDs <- tbl(pool, "article") |>
      filter(PMID %in% local(noHTML)) |>
      pull(arID)

    deleted <- dbDeleteArticle(arIDs, dbInfo = localCheckout(pool))

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

          elementMsg("bulkImportAuthorMsg")
          data <- data |> select(lastName, firstName, affiliation)
        },
        error = function(e) {
          elementMsg(
            "bulkImportAuthorMsg",
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

      list(importInfo = importInfo, importData = importData, history = history)
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

      # test <<- bulkImport()
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
          dbInfo = localCheckout(pool),
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

    dbFlagUpdate(1, dbInfo = localCheckout(pool))

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
}

shinyApp(ui, server)
