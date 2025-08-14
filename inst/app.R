# ///////////////
# ---- DATA ----
# //////////////

# ---- SETUP ----
if (!exists("envInfo")) {
  mode <- "dev" # Set to dev or prod

  if (mode == "dev") {
    # Add any code here to use for dev (running the file in IDE)
    # devtools::load_all()
    message("DEV TEST")

    file.copy("../data/dbmi.db", "../local/dev.db", overwrite = T)
    testDB <- "../local/dev.db"
    # testDB <- "C:/Users/pj/Desktop/sz.db"
    # testDB <- "../data/dbmi.db"
    # testDB <- NULL

    envInfo = list(
      mode = mode,
      dbPath = testDB,
      localFolder = file.path("..", "data"),
      tempFolder = file.path("..", "temp"),
      autoCleanTemp = NULL
    )
  } else if (mode == "prod") {
    # This is used when the app is published to the web
    library(dplyr)
    library(dbplyr)
    library(DT)
    library(RSQLite)
    library(pool)
    library(shiny)
    library(shinyjs)
    library(stringr)
    library(visNetwork)
    library(colabNet)

    message("ColabNet Production Mode")
    dir.create("localDB", showWarnings = F)
    dir.create("tempDB", showWarnings = F)

    envInfo = list(
      mode = mode,
      dbPath = NULL,
      localFolder = "localDB",
      tempFolder = "tempDB",
      autoCleanTemp = list(
        checkTime = 1, # How often to check (in hours)
        maxTime = 24, # Hours before deleting temp DB
        maxSize = 50 # Max size of temp folder
      )
    )
  }
} else {
  # Generate temp dirs
  if (is.null(envInfo$localFolder)) {
    envInfo$localFolder <- tempdir()
  }

  if (is.null(envInfo$tempFolder)) {
    envInfo$tempFolder <- tempdir()
  }
}

# ///////////////////////////
# ---- SHARED FUNCTIONS ----
# //////////////////////////

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
calcOverlap <- function(precompOverlap, treeFilter, authors, permutation = F) {
  if (!is.null(treeFilter)) {
    precompOverlap <- precompOverlap |> filter(tree %in% {{ treeFilter }})
  }

  overlapscore <- precompOverlap |>
    group_by(au1, au2) |>
    summarise(score = sum(score), .groups = "drop") |>
    arrange(desc(score))

  names <- authors |>
    transmute(auID, name = sprintf("%s, %s", lastName, firstName))

  overlapscoreTable <- overlapscore |>
    left_join(
      names |>
        select(au1 = auID, author1 = name),
      by = "au1"
    ) |>
    left_join(names |> select(au2 = auID, author2 = name), by = "au2") |>
    select(author1, author2, overlapScore = score)

  if (permutation) {
    overlapscore <- bind_rows(
      overlapscore,
      overlapscore |> select(au1 = au2, au2 = au1, score)
    )
    overlapscoreTable <- bind_rows(
      overlapscoreTable,
      overlapscoreTable |>
        select(author1 = author2, author2 = author1, overlapScore)
    ) |>
      arrange(desc(overlapScore), author1, author2)
  }

  return(list(score = overlapscore, table = overlapscoreTable))
}

# Clean up the temp folder regularly
if (!is.null(envInfo$autoCleanTemp)) {
  tempClean <- reactivePoll(
    envInfo$autoCleanTemp$checkTime * 3600000, #check every hour
    NULL,
    checkFunc = function() {
      toDelete <- tempFileCheck(
        envInfo$tempFolder,
        envInfo$autoCleanTemp$maxTime,
        envInfo$autoCleanTemp$maxSize,
        ".db"
      )

      if (nrow(toDelete$fileInfo) > 0) {
        Sys.time()
      } else {
        NULL
      }
    },
    valueFunc = function() {
      # Delete flagged temp files
      toDelete <- tempFileCheck(
        envInfo$tempFolder,
        envInfo$autoCleanTemp$maxTime,
        envInfo$autoCleanTemp$maxSize,
        ".db"
      )
      files <- toDelete$fileInfo |> filter(toRemove) |> pull(path)

      if (length(files) > 0) {
        # file.remove(files)
      }

      toDelete$summary
    }
  )

  observe({
    message(Sys.time(), " - Auto clean up temp files")
    print(tempClean())
  })
}

# //////////////
# ---- UI ----
# /////////////

ui <- fluidPage(
  useShinyjs(),
  # Download button style
  tags$head(tags$style(HTML(paste(
    "#cnDB-dbDownload {",
    "position: fixed; top: 0; right: 0; margin: 10px; z-index:1000;",
    "background-color:#40928A75; color: white;}"
  )))),
  div(
    mod_dbSetup_ui("cnDB")
  ),
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

                tags$i(
                  "The connections in this graph represent the weighed sum of co-authored papers.",
                  "The more recent the paper, the higher its weight"
                ),
                tags$hr(),
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
        ),
        value = "analysis"
      ),
      tabPanel("Admin", uiOutput("adminTab"), value = "admin")
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
  # ---- SETUP ----
  # ///////////////

  # Setup the DB
  connInfo <- mod_dbSetup_server(
    id = "cnDB",
    localFolder = envInfo$localFolder,
    tempFolder = envInfo$tempFolder,
    schema = system.file("create_colabNetDB.sql", package = "colabNet"),
    useDB = envInfo$dbPath
  )

  #Only show that admin / download modules for databases that are not local
  output$adminTab <- renderUI({
    adminPass <- isolate({
      ifelse(is.null(input$adminPass), "", input$adminPass)
    })
    tries <- input$login

    if (
      connInfo()$dbType != 1 |
        ifelse(exists("adminPassword"), adminPass == adminPassword, T)
    ) {
      # Admin module UI
      newUI <- mod_admin_ui("admin")
      # Admin module server
      admin <- mod_admin_server("admin", pool)
    } else {
      newUI <- wellPanel(
        "This database is locked. Please provide the password",
        passwordInput("adminPass", "Admin Password"),
        actionButton("login", "Login")
      )
    }
    return(newUI)
  })

  # pool for the current DB
  pool <- eventReactive(connInfo(), {
    dbPool(SQLite(), dbname = connInfo()$dbPath)
  })

  onSessionEnded(function() {
    isolate(poolClose(pool()))
  })

  # Precompute data
  preCompData <- reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      if (is.null(connInfo()$dbPath)) {
        return(NULL)
      }
      # Check the database for new updates
      tbl(pool(), "updateData") |>
        arrange(desc(uID)) |>
        head(1) |>
        pull(uID)
    },
    valueFunc = function() {
      req(connInfo()$dbPath)
      showNotification(
        "Gathering data ... be patient",
        type = "message",
        duration = NULL,
        id = "precalc"
      )

      authors <- tbl(pool(), "author") |>
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

      if (length(nrow(authors)) == 0) {
        return(list(
          authors = NULL,
          plotData = NULL,
          overlapscore = NULL,
          allArticles = NULL
        ))
      }

      allArticles <- tbl(pool(), "coAuthor") |>
        filter(auID %in% local(authors$auID)) |>
        distinct() |>
        left_join(tbl(pool(), "article"), by = "arID") |>
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

      papermesh <- dbPaperMesh(authors$auID, dbInfo = pool())
      meshtree <- dbMeshTree(papermesh, dbInfo = pool())
      papermeshtree <- paperMeshTree(papermesh, meshtree)

      # Add author names
      au <- tbl(pool(), "author") |>
        filter(auID %in% local(unique(papermeshtree$auID))) |>
        select(auID) |>
        left_join(
          tbl(pool(), "authorName") |> filter(default == 1),
          by = "auID"
        ) |>
        collect() |>
        rowwise() |>
        mutate(name = paste(lastName, firstName, sep = ", ")) |>
        select(auID, name)

      papermeshtree <- papermeshtree |>
        left_join(au |> select(auID, name), by = "auID") |>
        mutate(name = ifelse(nPapers == 0, "", name))

      overlapscore <- zooScore(papermeshtree)

      removeNotification("precalc")

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

  #Updates when the pre-calculated data changes
  observeEvent(preCompData(), {
    req(nrow(preCompData()$authors) > 0)

    # Update the tree root categories as filter options for the comparison tree
    roots <- tbl(pool(), "meshTree") |>
      filter(treenum %in% local(unique(preCompData()$overlapscore$tree))) |>
      left_join(tbl(pool(), "meshLink"), by = "uid") |>
      left_join(tbl(pool(), "meshTerm"), by = "meshui") |>
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
      # We're only looking at two authors.
      req(overlapData())
      if (length(input$overlapscoreTable_rows_selected) == 0) {
        auIDs <- overlapData()
      } else {
        auIDs <- overlapData()[input$overlapscoreTable_rows_selected, ]
      }

      auIDs <- auIDs |>
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
      mutate(
        width = weight,
        label = as.character(round(weight, 1)),
        color = case_when(
          width >= 2 ~ "#990000",
          width >= 1 ~ "#ee6600",
          width >= 0.5 ~ "#ffcc33",
          TRUE ~ "#0000ff"
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
          repulsion = 500 # Increased repulsion value
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
      result <- tbl(pool(), "meshTree") |>
        filter(mtrID %in% local(mtrIDs)) |>
        left_join(tbl(pool(), "meshLink"), by = "uid") |>
        left_join(tbl(pool(), "meshTerm"), by = "meshui") |>
        left_join(tbl(pool(), "mesh_article"), by = "meshui") |>
        pull(arID) |>
        unique()
      return(result)
    }

    # Case where a brac has been selected
    children <- tbl(pool(), "meshTree") |>
      filter(mtrID == as.integer(branchID)) |>
      pull(treenum)

    children <- paste0(children[1], "%")
    result <- tbl(pool(), "meshTree") |>
      filter(str_like(treenum, local({{ children }})))
    #Filter in case the tree is limited by mtPlotLimit input
    if (length(mtrIDs) > 0) {
      result <- result |> filter(mtrID %in% local(mtrIDs))
    }

    result |>
      left_join(tbl(pool(), "meshLink"), by = "uid") |>
      left_join(tbl(pool(), "meshTerm"), by = "meshui") |>
      left_join(tbl(pool(), "mesh_article"), by = "meshui") |>
      pull(arID) |>
      unique()
  }

  # ---- Research Comparison ----

  # The table that shows the overlap score for pairs of authors
  output$overlapscoreTable <- renderDT(
    {
      req(nrow(preCompData()$overlapscore) > 0)
      calcOverlap(preCompData()$overlapscore, NULL, preCompData()$authors)$table
    },
    rownames = F,
    selection = list(mode = "single", selected = 1),
    options = list(lengthMenu = list(c(5, 10, 15), c("5", "10", "15")))
  )

  overlapscoreTable_proxy <- dataTableProxy("overlapscoreTable")

  overlapData <- reactiveVal()

  # Update table that shows the overlap score and return the raw table data
  observeEvent(
    c(input$applyFilterCat, preCompData()$overlapscore),
    {
      req(nrow(preCompData()$overlapscore) > 0)
      overlap <- calcOverlap(
        preCompData()$overlapscore,
        input$overlapCat,
        preCompData()$authors
      )

      req(
        is.null(overlapData()) ||
          !identical(overlap$score, overlapData())
      )

      replaceData(overlapscoreTable_proxy, overlap$table, rownames = F)

      if (nrow(overlap$table) > 0) {
        DT::selectRows(overlapscoreTable_proxy, 1)
      }

      overlapData(overlap$score)
    }
  )

  # Remove any tree filters from the scoring categories
  observeEvent(input$removeFilterCat, {
    req(length(input$overlapCat) > 0)

    updateSelectInput(session, "overlapCat", selected = character(0))
    overlap <- calcOverlap(
      preCompData()$overlapscore,
      NULL,
      preCompData()$authors
    )

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
      tmComp <- papermeshtreeFromAuIDs(c(auIDs[1], auIDs[2]), dbInfo = pool())
    } else {
      tmComp <- papermeshtreeFromAuIDs(
        c(auIDs[1], auIDs[2]),
        roots = input$overlapCat,
        dbInfo = pool()
      )
      #TODO make sure the table filters with arIDs only found in the selected subtrees
    }

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
    rangeData <- preCompData()$allArticles |>
      filter(between(year, range[1], range[2]))

    graphElements <- copubGraphElements(rangeData)
    graphStats <- copubGraphStats(graphElements)

    return(list(graphElements = graphElements, graphStats = graphStats))
  })

  output$netAnalisisPlot <- renderVisNetwork({
    edges <- networkanalysis()$graphElements$edges |>
      mutate(
        width = weight,
        label = as.character(round(weight, 1)),
        color = case_when(
          width >= 2 ~ "#990000",
          width >= 1 ~ "#ee6600",
          width >= 0.5 ~ "#ffcc33",
          TRUE ~ "#0000ff"
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
          repulsion = 500 # Increased repulsion value
        )
      )
  })

  output$summaryStats <- renderUI({
    x <- networkanalysis()$graphStats

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
        ),
        weightTotal = round(weightTotal, 1)
      ) |>
      arrange(desc(degree), desc(weightTotal)) |>
      select(
        name,
        connections = degree,
        `papers (% total)` = papers,
        `total weight` = weightTotal,
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
            list(targets = 2, orderData = 4),
            list(targets = 4, visible = F)
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

  # observe({
  #   print(tempClean())
  # })
}

shinyApp(ui, server)
