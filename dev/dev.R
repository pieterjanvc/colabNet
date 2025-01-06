# 1 - Create blank database
library(RSQLite)
colabDB <- "dev/colabNet.db"
if(!file.exists(colabDB)){
  sqlFile = readLines("database/create_colabNetDB.sql") %>% paste(collapse = "")
  sqlFile = strsplit(sqlFile,";") %>% unlist()


  myConn = dbConnect(SQLite(),colabDB)
  sapply(sqlFile, function(sql){
    x = dbExecute(myConn, sql)
  })
  dbDisconnect(myConn)
}

# Check if an author exists and is in DB
ncbi_author("PJ", "Van Camp")
# insert author data into database
x = ncbi_authorPublicationInfo("PJ", "Van Camp")

myConn = dbConnect(SQLite(),colabDB)
# Create author ID
auID <- dbGetQuery(myConn, "INSERT INTO author(modified) VALUES (?) RETURNING auID",
           params = list(timeStamp()))
auID <- auID$auID
# Insert name(s)
q <- dbSendStatement(
  myConn,
  "INSERT INTO authorName(auID,lastName,firstName,initials)
  VALUES (?,?,?,?)",
  params = list(rep(auID, nrow(x$author)), x$author$lastName,
                x$author$firstName, x$author$initials))

dbDisconnect(myConn)

# Get meshUI from papers
x = ncbi_authorPublicationInfo("Laurent", "Van Camp")
meshui = unique(x$meshDescriptors$DescriptorUI)

meshInfo = ncbi_meshInfo(meshui, type = "meshui")

# Check DB if the missing nodes are already known
myConn = RSQLite::dbConnect(RSQLite::SQLite(),"dev/test.db")

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
  x = RSQLite::dbWriteTable(myConn, "meshLinks", 
  meshInfo$meshTree |> select(uid, meshui) |> distinct(), append = T)
  x = RSQLite::dbWriteTable(myConn, "meshTerms", meshInfo$meshTerms, append = T)
  x = RSQLite::dbWriteTable(myConn, "meshTree", 
  meshInfo$meshTree |> select(uid, treenum) |> distinct(), append = T)
}

RSQLite::dbDisconnect(myConn)
