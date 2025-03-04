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

n_distinct(plotData$meshterm) == length(plotData$meshterm)

n = 2111
plot_ly(
  type = "treemap",
  labels = plotData$meshterm,
  parents = plotData$parentMeshterm,
  marker = list(colors = plotData$colour),
  hovertext = sprintf("%s<br><br>%s", plotData$meshterm, 
  plotData$auNames),
  hoverinfo = "text",
  textfont = list(
    color = textBW(plotData$colour)
  ),
  maxdepth = -1,
  source = "mtPlot"
)    

plotData$meshterm[2111]

# E05.591.560.500
# E05.591.560 is missing

dbSetup("dev/PGG.db", checkSchema = T)

auIDs <- tbl(dbGetConn(checkSchema = F), "author") |> 
        filter(authorOfInterest == 1) |> 
        pull(auID)
difftree <- diffTree(auIDs, pruneDuplicates = T)

# PRUNE ALGORITHM

# Step 1 - Find duplicated meshterms in different (parts of) tree
test <- difftree |>
    group_by(uid) |>
    mutate(duplicated = n() > 1, nDup = n() - 1) |>
    ungroup()

# Step 2 - Start at bottom and work way up the tree. For each treenum add:
# - number of unique children
# - number of duplicated children

test <- test |> mutate(uniqueChildren = 0, dupChildren = 0)

for(level in sort(unique(test$level), decreasing = T)){

  # THe number of unique / dup children is the previous plus current 
  currentNums <- test |> filter(level == {{level}}) |> 
    select(treenum, duplicated, parent, uniqueChildren, dupChildren) |> 
    mutate(
      addUnique = uniqueChildren + !duplicated,
      addDup = dupChildren + duplicated,
    ) |> select(treenum = parent, addUnique, addDup) |>    
    filter(treenum != "") |> 
    group_by(treenum) |> 
    summarise(addUnique = sum(addUnique), addDup = sum(addDup), .groups = "drop")

  
  test <- test |> 
    left_join(currentNums, by = "treenum") |> 
    mutate(
      uniqueChildren = ifelse(is.na(addUnique), uniqueChildren, 
        uniqueChildren + addUnique),
        dupChildren = ifelse(is.na(addDup), dupChildren, 
        dupChildren + addDup)
    ) |> select(-addUnique, -addDup)
}

# Step 3 - Pruning
# The highest level that is duplicated and has all duplicate children is removed 
# first
test <- backup

remainingDup <- data.frame(uid = c(), remaining = c())
pruned <- c()

toPrune <- test |> 
  filter(!treenum %in% pruned) |> 
  filter(duplicated, uniqueChildren == 0) |>
  filter(dupChildren == max(dupChildren)) |> 
  filter(level == min(level)) 
j = 1
while(nrow(toPrune) > 0){

  for(i in 1:nrow(toPrune)){

    # Get the node of interest and all children (all should be duplicated)
    x <- test |> filter(str_detect(treenum, paste(toPrune[i,]$treenum, collapse = "|")))
    
    # Check how many duplicates are left in the original dataset (don't prune last one)
    remainingDup <- bind_rows(
      remainingDup, 
      x |> select(uid, remaining = nDup) |> 
      group_by(uid) |> slice(1) |> ungroup() |> filter(!uid %in% remainingDup$uid)
    )
    
    toRemove <- x |> select(treenum, uid) |> left_join(remainingDup, by = "uid") |>
      filter(remaining > 0)

    if(nrow(toRemove) == 0) {
      next
    }

    # Check for rare case where child of to be removed has no remaining duplicates
    toKeep <- x |> filter(str_detect(treenum, paste(toRemove$treenum, collapse = "|")))
    if(nrow(toKeep) > nrow(toRemove)){
      toRemove <- toRemove |> filter(!treenum %in% toKeep$treenum)
      
      if(nrow(toRemove) == 0){
        next
      }
    }

    x <- toRemove

    remainingDup <- remainingDup |> left_join(
      x |> group_by(uid) |> summarise(n = n(), .groups = "drop"), by = "uid"
    ) |> mutate(remaining = ifelse(is.na(n), remaining, remaining - n)) |> 
      select(-n) 

    toRemove <- x$treenum
    # Rare case where duplicates are nested in the same part of the tree
    # and would all be pruned removing them completely (keep at least one)
    if(any(remainingDup$remaining < 0)){
      toFix <- remainingDup$uid[remainingDup$remaining < 0]
      for(y in toFix){
        # Select one to keep (least nested)
        toKeep <- x |> filter(uid == y) |> 
          filter(nchar(treenum) == min(nchar(treenum))) |> 
          slice(1)
        toKeep <- c(missingTreeNums(toKeep$treenum), toKeep$treenum)
        toRemove <- setdiff(toRemove, toKeep)
      }
      # Manually set the remaining to 0 to not trigger the same one next round
      remainingDup$remaining[remainingDup$remaining < 0] = 0
    }

  
    test <- test |> filter(!treenum %in% toRemove)
  }

  toPrune <- test |> 
    filter(!treenum %in% pruned) |> 
    filter(duplicated, uniqueChildren == 0) |>
    filter(dupChildren == max(dupChildren)) |> 
    filter(level == min(level)) 

  pruned <- c(pruned, toPrune$treenum)

  if(!"N05.715.360.300.715.500.530" %in% test$treenum) {
    print(j)
    break
  }
  j = j + 1
}

check <- difftree$uid[!difftree$uid %in% test$uid]
test1 <- difftree |> filter(uid %in% difftree$uid[!difftree$uid %in% test$uid])

missingTreeNums(test$treenum)

test$uid[test$uid %in% names(table(test$uid)[table(test$uid) > 1])]
# I01.409
# N03.540.348
x <- difftree |> filter(str_detect(treenum, "I01|N03.540"))
