#' Run the colabNet Shiny App
#'
#' @import shiny dplyr stringr tidyr purrr visNetwork
#' @importFrom DT DTOutput renderDT
#'
#' @return Start the Shiny app
#' @export
#'
colabNet_v1 <- function() {
  # ////////////////
  # ---- DATA ----
  # ///////////////

  # Original data
  # All data in /data will be loaded at automatically

  # Add Techniques and Models column
  df <- df %>%
    rowwise() %>%
    mutate(
      Broad = paste(c(Broad, if (!is.na(Techniques)) "Techniques"), collapse = ", "),
      Broad = paste(c(Broad, if (!is.na(Model)) "Model"), collapse = ", ")
    )

  # Get the key words by level
  lvl1 <- df$MacroArea %>%
    str_split(", ") %>%
    unlist() %>%
    unique()
  lvl2 <- df %>%
    select(-c(MyID:Broad, Extra:Link)) %>%
    colnames() %>%
    str_replace("_", " ")
  keywords <- df %>% select(all_of(str_replace(lvl2, "\\s", "_")))
  lvl3 <- keywords %>%
    unlist() %>%
    str_split(", ") %>%
    unlist() %>%
    unique()
  lvl3 <- lvl3[!is.na(lvl3)]

  # Link lvl 3 to lvl 2 keywords (there is no 2 to 1 link)
  lvl2to3 <- keywords %>%
    pivot_longer(everything()) %>%
    filter(!is.na(value), value != "na") %>%
    mutate(name = str_replace(name, "_", " "))

  lvl2to3 <- map_df(unique(lvl2to3$name), function(x) {
    data.frame(
      lvl2 = x,
      lvl3 = lvl2to3 %>% filter(name == x) %>% pull(value) %>%
        str_split(", ") %>% unlist() %>% unique()
    )
  }) %>% distinct()

  # Generate key words table
  keywords <- bind_rows(
    data.frame(keyword = lvl1, lvl = 1),
    data.frame(keyword = lvl2, lvl = 2),
    data.frame(keyword = lvl3, lvl = 3)
  ) %>%
    left_join(
      lvl2to3 %>% select(keyword = lvl3, parent = lvl2),
      by = "keyword"
    ) %>%
    arrange(lvl, keyword)

  # Table with PI info
  PIs <- df_net %>%
    select(piId = MyID, lName = Last_Name, fName = First_Name) %>%
    distinct() %>%
    left_join(
      df %>% select(piId = MyID, labSite = Link),
      by = "piId"
    )

  # Link PI to keywords
  piKeywords <- apply(df, 1, function(pi) {
    bind_rows(
      data.frame(
        piId = pi[["MyID"]],
        keyword = str_split(pi[["MacroArea"]], ", ") %>% unlist()
      ) %>% left_join(keywords %>% filter(lvl == 1), by = "keyword"),
      data.frame(
        piId = pi[["MyID"]],
        keyword = str_split(pi[["Broad"]], ", ") %>% unlist()
      ) %>% left_join(keywords %>% filter(lvl == 2), by = "keyword"),
      data.frame(
        piId = pi[["MyID"]],
        keyword = pi[-c(1:5, (length(pi) - 1):length(pi))] %>% unlist() %>%
          str_split(", ") %>% unlist() %>% unique()
      ) %>% distinct() %>% filter(!is.na(keyword)) %>%
        left_join(keywords %>% filter(lvl == 3), by = "keyword")
    )
  }) %>% bind_rows()

  # List of papers
  papers <- df_net %>%
    select(piId = MyID, title = Title, PMID) %>%
    distinct()


  # //////////////
  # ---- UI ----
  # /////////////

  ui <- fluidPage(
    fluidRow(
      column(3, DTOutput("lvl1Key"), DTOutput("lvl2Key"), DTOutput("lvl3Key")),
      column(
        9,
        visNetworkOutput("visGraph", height = "100vh"),
        uiOutput("keyInfo"),
        DTOutput("piTable"),
        uiOutput("selInfo"),
        DTOutput("authorTable"),
        br(),
        DTOutput("paperTable")
      )
    ),
    br(),
    tags$footer(
      p(
        "This app was cretaed to support the Harvard Medical School BBS",
        tags$a("Program in Genetics and Genomics",
          href = "https://projects.iq.harvard.edu/pgg", target = "_blank"
        )
      ),
      p(
        "Content manager: Lorenzo Gesuita -",
        tags$a("lorenzo_gesuita@hms.harvard.edu", href = "mailto:lorenzo_gesuita@hms.harvard.edu"),
        "| App creator: PJ van Camp -",
        tags$a("pjvancamp@hms.harvard.edu", href = "mailto:pjvancamp@hms.harvard.edu")
      ),
      style = "width: 100%;margin: auto;text-align: center;background-color: #f6f6f6;
    color:#787878;border-top: 0.2rem solid;"
    )
  )


  # //////////////////
  # ---- SERVER ----
  # /////////////////

  server <- function(input, output, session) {
    lvl1Key <- keywords %>%
      filter(lvl == 1) %>%
      select(`Macro Area` = keyword)

    output$lvl1Key <- renderDT({
      datatable(lvl1Key,
        options = list(dom = "t"),
        rownames = F, selection = "single"
      ) %>%
        formatStyle(0,
          target = "row",
          backgroundColor = "#F9FAFF"
        )
    })


    lvl2Key <- reactive({
      req(input$lvl1Key_rows_selected)
      id <- piKeywords %>%
        filter(lvl == 1, keyword %in% c(lvl1Key[input$lvl1Key_rows_selected, ])) %>%
        pull(piId)
      piKeywords %>%
        filter(lvl == 2, piId %in% id) %>%
        select(keyword, parent) %>%
        distinct() %>%
        arrange(keyword)
    })

    output$lvl2Key <- renderDT({
      datatable(lvl2Key() %>% select(`Broad Area` = keyword),
        options = list(
          dom = "t", pageLength = nrow(keywords),
          language = list(emptyTable = "No matching keywords")
        ),
        rownames = F, selection = "single"
      ) %>%
        formatStyle(0,
          target = "row",
          backgroundColor = "#FAFFF9"
        )
    })

    lvl3Key <- reactive({
      req(input$lvl2Key_rows_selected)
      id <- piKeywords %>%
        filter(keyword %in% c(
          lvl1Key[input$lvl1Key_rows_selected, ],
          lvl2Key()[input$lvl2Key_rows_selected, ]
        )) %>%
        group_by(piId) %>%
        filter(n() == 2) %>%
        ungroup() %>%
        pull(piId)
      piKeywords %>%
        filter(
          lvl == 3, piId %in% id,
          parent %in% c(lvl2Key()[input$lvl2Key_rows_selected, ])
        ) %>%
        select(keyword, parent) %>%
        distinct() %>%
        arrange(keyword)
    })

    output$lvl3Key <- renderDT({
      req(input$lvl2Key_rows_selected)
      datatable(lvl3Key() %>% select(`Specific Area` = keyword),
        options = list(
          dom = "t", pageLength = nrow(keywords),
          language = list(emptyTable = "No matching keywords")
        ),
        rownames = F, selection = "single"
      ) %>%
        formatStyle(0,
          target = "row",
          backgroundColor = "#FFF9FA"
        )
    })

    PIsel <- reactive({
      x <- c(
        ifelse(!is.null(input$lvl1Key_rows_selected), lvl1Key[input$lvl1Key_rows_selected, ], NA),
        ifelse(!is.null(input$lvl2Key_rows_selected), lvl2Key()[input$lvl2Key_rows_selected, ], NA),
        ifelse(!is.null(input$lvl3Key_rows_selected), lvl3Key()[input$lvl3Key_rows_selected, ], NA)
      )
      x <- x[!is.na(x)]
      piKeywords %>%
        filter(keyword %in% x) %>%
        group_by(piId) %>%
        filter(n() == length(x)) %>%
        pull(piId)
    })

    output$keyInfo <- renderUI({
      x <- ""
      if (!is.null(input$lvl1Key_rows_selected)) {
        x <- lvl1Key[input$lvl1Key_rows_selected, "Macro Area"]
      }

      if (!is.null(input$lvl2Key_rows_selected)) {
        x <- paste(x, ">", (lvl2Key() %>% pull(keyword) %>%
          unique())[input$lvl2Key_rows_selected])
      }

      if (!is.null(input$lvl3Key_rows_selected)) {
        x <- paste(x, ">", (lvl3Key() %>% pull(keyword) %>%
          unique())[input$lvl3Key_rows_selected])
      }

      h2(x)
    })

    output$piTable <- renderDT({
      datatable(
        PIs %>% filter(piId %in% PIsel()) %>%
          select(`First Name` = fName, `Last Name` = lName),
        select = "single", rownames = F
      )
    })

    piTableProxy <- dataTableProxy("piTable")

    # ---- Network ----
    # //////////////////

    nodes <- df_net %>%
      select(MyID, First_Name, Last_Name) %>%
      distinct() %>%
      transmute(
        id = MyID, label = paste(Last_Name, First_Name),
        color.highlight.background = "#d97526"
      ) %>%
      left_join(df %>% select(id = MyID, Link), by = "id") %>%
      # Remove once real lab links
      mutate(Link = "https://projects.iq.harvard.edu/pgg/faculty")

    collab <- df_net %>% select(MyID, PMID, Title)
    collab <- collab %>%
      select(p1 = MyID, PMID, Title) %>%
      left_join(collab %>% select(p2 = MyID, PMID),
        by = "PMID", relationship = "many-to-many"
      ) %>%
      filter(p1 != p2) %>%
      group_by(PMID) %>%
      slice(1) %>%
      ungroup()

    edges <- collab %>%
      group_by(p1, p2) %>%
      summarise(
        nPapers = n(),
        Title = paste(n(), ifelse(n() == 1, "paper", "papers")),
        .groups = "drop"
      )

    edges <- edges %>%
      transmute(
        from = p1, to = p2, width = nPapers, label = as.character(nPapers),
        color = "#6b948b", font.color = "blue", title = Title
      ) %>%
      mutate(id = 1:n())

    output$visGraph <- renderVisNetwork(
      visNetwork(nodes, edges,
        height = "100vh",
        main = "Collaborations across PGG"
      ) %>%
        visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(gravitationalConstant = -15)
        ) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
        # Custom function to be able to select nodes AND edges
        visEvents(select = "function(data) {
                Shiny.onInputChange('nodes_selection', data.nodes);
                Shiny.onInputChange('edges_selection', data.edges);
                ;}") %>%
        visInteraction(zoomView = F)
    )

    observe({
      nodes <- nodes %>%
        mutate(color.background = ifelse(id %in% PIsel(), "#268ad9", "#d3e7f7"))
      visNetworkProxy("visGraph") %>%
        visUpdateNodes(nodes = nodes)
    })

    authors <- reactiveVal(c())

    observeEvent(input$piTable_rows_selected,
      {
        piId <- PIs %>%
          filter(piId %in% PIsel()) %>%
          slice(input$piTable_rows_selected) %>%
          pull(piId)
        authors(piId)
        visNetworkProxy("visGraph") %>%
          visSelectNodes(id = piId)
      },
      ignoreNULL = F
    )

    observeEvent(c(input$nodes_selection, input$edges_selection),
      {
        sel <- c()

        if (!is.null(input$edges_selection)) {
          p12 <- edges %>% filter(id %in% input$edges_selection)
          sel <- c(p12$from, p12$to)
        }

        if (!is.null(input$nodes_selection)) {
          sel <- input$nodes_selection
        }

        if (!any(sel %in% PIsel())) {
          selectRows(piTableProxy, NULL)
        } else if (length(sel) == 1) {
          x <- PIs %>%
            filter(piId %in% PIsel()) %>%
            pull(piId)
          selectRows(piTableProxy, which(sel == x))
        }

        authors(sel)
      },
      ignoreNULL = F
    )

    output$selInfo <- renderUI({
      if (length(authors()) == 1) {
        HTML("<h1>Author papers</h1>")
      } else if (length(authors()) == 2) {
        HTML("<h1>Collaboration</h1>")
      }
    })

    output$authorTable <- renderDT({
      req(authors())
      datatable(
        nodes %>% filter(id %in% authors()) %>%
          mutate(Link = sprintf(
            '<a href="%s" target="_blank">%s</a>',
            Link, Link
          )) %>%
          select(Name = label, Website = Link),
        rownames = F, escape = F, selection = "none",
        options = list(dom = "t", pageLength = 2)
      )
    })

    output$paperTable <- renderDT({
      req(authors())
      if (length(authors()) == 1) {
        x <- df_net %>%
          filter(MyID == authors()) %>%
          select(PMID, Title) %>%
          arrange(Title)
      } else if (length(authors()) == 2) {
        x <- collab %>%
          filter(p1 %in% authors() & p2 %in% authors()) %>%
          select(PMID, Title)
      }
      datatable(
        x %>%
          mutate(PMID = sprintf(
            '<a href="%s%s" target="_blank">%s</a>',
            "https://pubmed.ncbi.nlm.nih.gov/", PMID, PMID
          )),
        escape = F, selection = "none", rownames = F
      )
    })
  }

  shinyApp(ui, server)
}
