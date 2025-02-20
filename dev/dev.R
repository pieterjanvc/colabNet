# Create a database to work with

# Install package for additional testing
# devtools::install()
# library("colabNet")

#devtools::load_all()
dbSetup("dev/colabNet.db", checkSchema = T)

# authors = data.frame(
#   firstName = c("PJ", "Lorenzo", "Cristina", "Irene", "Grey", "Kayla", "Lauren"),
#   lastName = c("Van Camp", "Gesuita","Deoliveira", "Wong", 'Kuling', "Nygaard", "Essler")
# )
# i = 2
# authorPublications <- lapply(1:nrow(authors), function(i){  
#   print("Next one")
#   firstName = authors$firstName[i]
#   lastName = authors$lastName[i]
#   authorinfo <- ncbi_author(lastName, firstName)
#   result <- ncbi_authorArticleList(authorinfo$lastName, authorinfo$firstName, 
#     authorinfo$initials, PMIDonly = T)

#   if(result$statusCode != 2){
#     warning("Too many results for ", firstName, " ", lastName)
#     return(NULL)
#   }
#   ncbi_publicationDetails(PMIDs = result$PMID, lastNameOfInterest = authorinfo$lastName)
# })

# authorPublications <- authorPublications[!sapply(authorPublications, is.null)]
# saveRDS(authorPublications, "data/ap.rds")
authorPublications <- readRDS("data/ap.rds")

# authorPublications <- readRDS("data/ap.rds")[[2]]
# x <- readRDS("data/ap.rds")[[1]]
result <- map_df(authorPublications, function(x){
  print("Next one")
  dbAddAuthorPublications(x)
})

result <- dbDeleteArticle(c(21, 23,24))

# auIDs = c(1,31,75)

# Find overlap between authors based on MeSH terms of their research papers

meshRoots <- data.frame(
  treenum = c(LETTERS[1:14], "V", "Z"),
  meshterm = c(
    "Anatomy",
    "Organisms",
    "Diseases",
    "Chemicals and Drugs",
    "Analytical, Diagnostic and Therapeutic Techniques, and Equipment",
    "Psychiatry and Psychology",
    "Phenomena and Processes",
    "Disciplines and Occupations",
    "Anthropology, Education, Sociology, and Social Phenomena",
    "Technology, Industry, and Agriculture",
    "Humanities",
    "Information Science",
    "Named Groups",
    "Health Care",
    "Publication Characteristics",
    "Geographicals"
  )
)


amt1 <- authorMeshTree(1) # PJ
amt2 <- authorMeshTree(31) # Lorenzo


## GENERATE TREEMAP PLOT
# auIDs = c(1,31,69,100,137,163)
# issue with Essler 163
# row 138  - E01.370.225.500.607 -> <br>E01.370.225.500.607.512
auIDs <- tbl(dbGetConn(checkSchema = F), "author") |> 
    # TODO remove 163 exclusion once bug fixed!!
    filter(authorOfInterest == 1, auID != 163) |> 
    pull(auID)
difftree <- diffTree(auIDs, pruneDuplicates = T)

test <- ncbi_authorArticleList("Van Camp", "Pieter-Jan", "PJ")

lastName = "Van Camp"

PMIDs <- "28202393"
lastNameOfInterest<- "DeOliveira"
authorPublications <- ncbi_publicationDetails("28202393", "DeOliveira")

new <- dbAddAuthorPublications(authorPublications)
