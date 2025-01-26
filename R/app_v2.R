#' Run the colabNet Shiny App
#'
#' @import shiny dplyr stringr tidyr purrr visNetwork pool plotly
#' @importFrom RSQLite SQLite
#' @importFrom DT DTOutput renderDT datatable dataTableProxy replaceData
#'
#' @return Start the Shiny app
#' @export
#'
colabNet_v2 <- function() {

  # ///////////////
  # ---- DATA ----
  # //////////////
  
  # ColabNet Database
  colabNetDB = "dev/colabNet.db"

  # Setup for functions in the package
  dbSetup(colabNetDB, checkSchema = T)

  # Pool for the Shiny app
  pool <- dbPool(SQLite(), dbname = colabNetDB)
  onStop(function() {
    poolClose(pool)
  })

  # Get the full diffTree
  print("Precompute shared data ...")
  auIDs <- tbl(pool, "author") |> 
    filter(authorOfInterest == 1) |> 
    pull(auID)
  difftree <- diffTree(auIDs, pruneDuplicates = T)
  plotData <- plotDiffTree(difftree)
  allArticles <- tbl(pool, "coAuthor") |> filter(auID %in% local(auIDs)) |> distinct() |> 
    left_join(tbl(pool, "article"), by = "arID") |> 
    left_join(tbl(pool, "authorName") |> 
      group_by(auID) |> filter(default) |> ungroup(), by = "auID") |> 
      select(arID, PMID, lastName, month, year, title, journal) |>
      arrange(desc(PMID)) |> collect() |> 
    mutate(PMID = sprintf('<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>', PMID, PMID))
  nArticles <- length(unique(allArticles$artID))
  authorsimscore <- authorSimScore(difftree)
  print("... finished")  
  
  # //////////////
  # ---- UI ----
  # /////////////

  ui <- fluidPage(
    fluidRow(column(12,
      tabsetPanel(
        tabPanel("Exploration",
        fluidRow(    
          fluidRow(column(12,
            h3("Similarity between researchers based on article MeSH terms")
          )),
          fluidRow(column(12,
            tabsetPanel(
              tabPanel("Network",
                visNetworkOutput("networkPlot", height = "60vh"),
                value="networkTab"),
                tabPanel("MeSH Tree",
                plotlyOutput("meshTreePlot", height = "60vh"),
                value="networkTab")
            )
          )),
          fluidRow(column(12,
            DTOutput("articleTable")
          ))
        ),value="exploration"),
        tabPanel("Data",
        fluidRow(column(6,          
          wellPanel(
            selectInput("auID", "Authors in Database", choices = NULL),
            uiOutput("alternativeNames")
          ),
          DTOutput("authorArticleList")        
        ), column(6,
          wellPanel(
            textInput("lastName", "Last name"),
            textInput("firstName", "First name"),
            textAreaInput("PMIDs", "(optional) Limit search by PMID (comma separated)"),
            actionButton("pubmedByAuthor", "Search Pubmed")
          ),
          uiOutput("pubmedSearch_msg"),
          DTOutput("pubmedArticles")
        )),
        value="exploration"
      )
      )
    ))    ,
    fluidRow(column(12,
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
        select(id = auID, lastName, firstName, collectiveName) |> collect() |> 
        mutate(
          label = case_when(
            !is.na(collectiveName) ~ collectiveName,
            is.na(firstName) ~ lastName,
            TRUE ~ sprintf("%s\n%s", lastName, firstName)
          )
        ) |> select(id, label)

      edges <- authorsimscore |> 
        transmute(from = auID1, to = auID2, 
          width = simScore / max(simScore),
          color = colorRamp(c("#feffb3", "#12b725"))(width) |> rgb(maxColorValue = 255),
          width = width  * 8)

      # Create a simple visNetwork graph
      visNetwork(nodes, edges) |>      
        visNodes(
          size = 20, 
          color = list(background = "lightblue", border = "blue"),
          font = list(background = rgb(1,1,1,0.8))
        ) |>
        visEdges(smooth = T) |> 
        visPhysics(
          barnesHut = list(
            # gravitationalConstant = -2000,  # Optional: adjust gravity
            # centralGravity = 0.3,           # Optional: adjust central gravity
            springLength = 200,             # Optional: adjust spring length
            # springConstant = 0.01,          # Optional: adjust spring constant
            # damping = 0.4,                  # Optional: adjust damping
            repulsion = 1000               # Increased repulsion value
          )
        )
    })

    # ---- Colab MeSH Tree ----

    output$meshTreePlot <- renderPlotly({      
      plot_ly(
        type = "treemap",
        labels = plotData$meshterm,
        parents = plotData$parentMeshterm,
        marker = list(colors = plotData$colour),
        hovertext = sprintf("%s<br><br>%s", plotData$meshterm, plotData$auNames),
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
      children <- plotData[event_data("plotly_click", "mtPlot")$pointNumber+1,]$leafNodeTreenum
      children <- paste0(children, "%")
      tbl(pool, "meshTree") |> filter(str_like(treenum, local({{children}}))) |> 
        left_join(tbl(pool, "meshLink"), by = "uid") |> 
        left_join(tbl(pool, "meshTerm"), by = "meshui") |> 
        left_join(tbl(pool, "mesh_article"), by = "meshui") |> 
        collect()
    })
   
    # ---- Articles Table ----

    proxy <- dataTableProxy("articleTable")
    output$articleTable <- renderDT({
      allArticles |> select(-arID)
    }, rownames = F, escape = F)    

    observeEvent(meshSel(), {

      toFilter <- unique(meshSel()$arID)
      
      if(length(toFilter) == nArticles){
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
        filter(authorOfInterest == 1) |> left_join(
          tbl(pool, "authorName"), by = "auID"
        ) |> collect() |> arrange(lastName)    
    })

    observeEvent(authorList(),{
      newVals <- authorList() |> filter(default == 1)
      updateSelectInput(session, "auID", choices = setNames(
        newVals$auID,
        paste(newVals$lastName, newVals$firstName, sep = ", ")        
      ))
    })

    output$alternativeNames <- renderUI({
      altNames <- authorList() |> filter(auID == local(input$auID)) |> 
        filter(default == 0)

      if(nrow(altNames) == 0) {
        return(NULL)
      } else {
        return(tags$i(
          sprintf("Also published under: %s", 
          paste(altNames$lastName, altNames$firstName, sep = ", ", collapse = " | "))))
      }
      
    })

    authorArticles <- reactive({      
      tbl(pool, "coAuthor") |> 
        filter(auID == local(input$auID)) |> select(auID, arID) |> left_join(
          tbl(pool, "article"), by = "arID"
        ) |> collect() |> arrange(desc(PMID))
    })

    output$authorArticleList <- renderDT({
      authorArticles() |> select(PMID, year, month, title, journal) |> 
        mutate(PMID = sprintf('<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>', PMID, PMID))
    }, rownames = F, escape = F)
  
    # ---- Search for new ----
    

    # Search by author name
    pubmedSearch <- reactive({

      if(str_trim(input$lastName) == "" | str_trim(input$firstName) == "" ){
        elementMsg("pubmedByAuthor", 
          "You need to provide both last name and first name")
        return(NULL)
      } 

      author <- ncbi_author(input$lastName, input$firstName)
        
        # If no author found
        if(length(author$lastName) == 0){
          elementMsg("pubmedByAuthor", 
          "No results for and author with provided first and last name")
          return(NULL)
        } 
        
        search <- ncbi_authorArticleList(author$lastName, author$firstName,
          author$initials, str_split(input$PMIDs, ",")[[1]] |> str_trim())        

        if(search$statusCode ==  0) {
          elementMsg("pubmedByAuthor",sprintf(
            "This search yielded too many (%i) results. Please add PMIDs",
            search$n))
          df <- NULL
        } else if (search$statusCode ==  1) {
          elementMsg("pubmedByAuthor",
          "It's possible that this name is ambiguous as many results were found. 
          Consider limiting by PMID to only return relevant articles.")
          df <- search$articles
        } else {
          elementMsg("pubmedByAuthor")
          df <- search$articles
        }      

        return(df)
      
    }) |> bindEvent(input$pubmedByAuthor)

    output$pubmedArticles <- renderDT({
      req(pubmedSearch())
      pubmedSearch() |> select(PMID, date, title, journal) |> 
        mutate(PMID = sprintf('<a href="https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>', PMID, PMID))
    }, rownames = F, escape = F)
  
  }  

  shinyApp(ui, server)
}
