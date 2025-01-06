# 1 - Create blank database
library(RSQLite)
colabDB <- "dev/colabNet.db"
if(!file.exists(colabDB)){
  sqlFile = readLines("database/create_colabNetDB.sql") %>% paste(collapse = "")
  sqlFile = strsplit(sqlFile,";") %>% unlist()


  myConn = dbConnect(SQLite(),colabDB)
  sapply(sqlFile, function(sql){
    q = dbExecute(myConn, sql)
  })
  dbDisconnect(myConn)
}

# Check if an author exists and is in DB
myConn = dbConnect(SQLite(),colabDB)
firstName = "PJ"
lastName = "Van Camp"
author = ncbi_author(firstName, lastName)
authors = tbl(myConn, "authorName") |> filter(paste(lastName, initials) == author) |> collect()

### IF NOT - Add author and papers
if(nrow(authors) > 0){
  stop("Author info already in DB")
}

# Get all publication details
pubInfo = ncbi_authorPublicationInfo(firstName, lastName)

# insert author data into database
# Create author ID
auID <- dbGetQuery(myConn, "INSERT INTO author(modified) VALUES (?) RETURNING auID",
           params = list(timeStamp()))
auID <- auID$auID
# Insert name(s)
q <- dbExecute(
  myConn,
  "INSERT INTO authorName(auID,lastName,firstName,initials)
  VALUES (?,?,?,?)",
  params = list(rep(auID, nrow(pubInfo$author)), pubInfo$author$lastName,
                pubInfo$author$firstName, pubInfo$author$initials))

# Get meshUI from papers
meshui = unique(pubInfo$meshDescriptors$DescriptorUI)

meshInfo = ncbi_meshInfo(meshui, type = "meshui")

# Check DB if the missing nodes are already known

missingNodes = missingTreeNums(meshInfo$meshTree$treenum)
knownNodes = tbl(myConn, "meshTree") %>% filter(treenum %in% local(missingNodes)) %>%
    pull(treenum)
  
missingNodes = setdiff(missingNodes, knownNodes)

while(length(missingNodes) > 0){ 
  
  # Only get new info on nodes not already in the database
  newNodes = ncbi_meshInfo(missingNodes, type = "treenum")  
  meshInfo$meshTerms = rbind(meshInfo$meshTerms, newNodes$meshTerms)
  meshInfo$meshTree = rbind(meshInfo$meshTree, newNodes$meshTree)

  missingNodes = missingTreeNums(meshInfo$meshTree$treenum)
  knownNodes = tbl(myConn, "meshTree") %>% filter(treenum %in% local(missingNodes)) %>%
  pull(treenum)

  missingNodes = setdiff(missingNodes, knownNodes)
}

knownNodes = tbl(myConn, "meshTree") %>% filter(treenum %in% local(meshInfo$meshTree$treenum)) %>%
    pull(treenum)
toAdd = setdiff(meshInfo$meshTree$treenum, knownNodes)

if(length(toAdd) > 0){

  # Only add new tree data
  meshInfo$meshTree = meshInfo$meshTree |> filter(treenum %in% toAdd)
  meshInfo$meshTerms = meshInfo$meshTerms |> filter(meshui %in% meshInfo$meshTree$meshui)

  # Add to database
  q = RSQLite::dbWriteTable(myConn, "meshLinks", 
  meshInfo$meshTree |> select(uid, meshui) |> distinct(), append = T)
  q = RSQLite::dbWriteTable(myConn, "meshTerms", meshInfo$meshTerms, append = T)
  q = RSQLite::dbWriteTable(myConn, "meshTree", 
  meshInfo$meshTree |> select(uid, treenum) |> distinct(), append = T)
}

### ADD PAPERS AND METADATA
pubInfo$coAuthors

RSQLite::dbDisconnect(myConn)

myConn = dbConnect(SQLite(),colabDB)

authors = pubInfo$coAuthors

dbAddAuthors(authors)

articles = pubInfo$articles
affiliations = pubInfo$affiliations
