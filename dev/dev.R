# Create a database to work with

firstName = "PJ"
lastName = "Van Camp"

firstName = "Lorenzo"
lastName = "Gesuita"

conn <- dbGetConn("dev/colabNet.db", checkSchema = T)

authorPublications <- ncbi_authorPublications(firstName, lastName)
result <- dbAddAuthorPublications(conn, authorPublications)

# Find overlap between authors based on MeSH terms of their research papers

# Get all MeSH tree entries for the author
auID = 1 #PJ Van Camp
auMeshui <- tbl(conn, "author_affiliation") |> filter(auID == local(auID)) |> 
  select(arID, auID) |> distinct() |> left_join(
    tbl(conn, "mesh_article"), by = "arID" 
  ) |> left_join(
    tbl(conn, "meshLink"), by = "meshui"
  ) |> left_join(
    tbl(conn, "meshTree"), by = "uid"
  ) |> 
  # Ignore papers with no MeSH terms
  filter(!is.na(meshui)) |> 
  collect()

# Use the meshTree to get all intermediate meshuis
auTree <- auMeshui |> select(mtrID, uid, treenum) |> distinct() 

# Cut off one level of the treenum and find its parents
nextNums <- ifelse(str_detect(auTree$treenum, "\\."), 
  str_remove(auTree$treenum, "\\.\\d+$"), NA) |> unique(na.rm = T)

while(length(nextNums) > 0){
  new <- tbl(conn, "meshTree") |> filter(treenum %in% local(nextNums)) |> 
    collect() 
  newTreenums <- setdiff(new$treenum, auTree$treenum)
  auTree <- bind_rows(auTree, new |> filter(treenum %in% newTreenums)) |> distinct()
  nextNums <- ifelse(str_detect(newTreenums, "\\."), 
    str_remove(newTreenums, "\\.\\d+$"), NA) |> unique(na.rm = T)
}



