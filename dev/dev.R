
# insert author data into database
x = ncbi_authorPublicationInfo("PJ", "Van Camp")



# Get meshUI from papers
x = ncbi_authorPublicationInfo("PJ", "Van Camp")
meshui = unique(x$meshDescriptors$DescriptorUI)

meshInfo = ncbi_meshInfo(terms = meshui)
missingNodes = missingTreeNums(meshInfo$meshTree$treenum)

# Check DB if the missing nodes are already known
myConn = RSQLite::dbConnect(RSQLite::SQLite(),"dev/test.db")

knownNodes = tbl(myConn, "meshTree") %>% filter(treenum %in% local(missingNodes)) %>%
  pull(treenum)

missingNodes = setdiff(missingNodes, knownNodes)

# Only get new info on nodes not already in the database
missingNodes = ncbi_meshInfo(missingNodes)

# Add to database
x = RSQLite::dbWriteTable(myConn, "meshTerms", missingNodes$meshterms, append = T)
x = RSQLite::dbWriteTable(myConn, "meshTree", missingNodes$meshTree, append = T)

RSQLite::dbDisconnect(myConn)

sqlFile = readLines("database/create_colabNetDB.sql") %>% paste(collapse = "")
sqlFile = strsplit(sqlFile,";") %>% unlist()


myConn = RSQLite::dbConnect(RSQLite::SQLite(),"dev/check.db")
sapply(sqlFile, function(sql){
  x = RSQLite::dbExecute(myConn, sql)
})

RSQLite::dbDisconnect(myConn)
