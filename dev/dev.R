# Create a database to work with

firstName = "PJ"
lastName = "Van Camp"

firstName = "Lorenzo"
lastName = "Gesuita"

conn <- dbGetConn("dev/colabNet.db", checkSchema = T)

authorPublications <- ncbi_authorPublications(firstName, lastName)
result <- dbAddAuthorPublications(conn, authorPublications)

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

auID = 1 #PJ Van Camp

amt1 <- authorMeshTree(conn, 1)
amt2 <- authorMeshTree(conn, 2)

# Get for each treenum whether there is overlap or not
diffTree <- bind_rows(amt1, amt2) |> group_by(treenum) |> 
  mutate(overlap = ifelse(n() == 1, F, T)) |> 
  filter(auID == auID[1]) |> 
  ungroup() |> 
  mutate(level = str_count(treenum, "\\."))

#Check the higest level overlaps (furthest from the root)
diffTree <- diffTree |> filter(overlap)

lvl <- max(diffTree$level)
toCheck <- diffTree |> mutate(root = str_extract(treenum, "^."))
result <- data.frame()
while(lvl > 0){
  toKeep <- toCheck |> filter(level == {{lvl}})
  toRemove <- missingTreeNums(toKeep$treenum)

  result <- bind_rows(result, toKeep)
  toCheck <- toCheck |> filter(!treenum %in% c(toKeep$treenum, toRemove))
  lvl <- lvl - 1
  
}

test <- result |> select(uid, level, treenum, root) |> 
  group_by(uid, root) |> filter(level == max(level)) |> ungroup()

meshTerm <- tbl(conn, "meshLink") |> filter(uid %in% local(test$uid)) |> 
  left_join(tbl(conn, "meshTerm"), by = "meshui") |> distinct() |> 
  group_by(uid) |> filter(mteID == min(mteID)) |> ungroup() |> collect()

# test <- test |> left_join(meshTerm, by = "uid")

plotData <- c(test$treenum, missingTreeNums(test$treenum))
parents <- str_remove(plotData, "\\.\\d+$")
parents <- ifelse(plotData == parents, NA, parents)

meshTerm <- tbl(conn, "meshTree") |> filter(treenum %in% local(c(plotData))) |> 
  left_join(tbl(conn, "meshLink"), by = "uid") |> 
  left_join(tbl(conn, "meshTerm"), by = "meshui") |> 
  collect() |> 
  group_by(treenum) |> slice(1) |> ungroup() |> 
  
  left_join(
    data.frame(
      treenum = plotData,
      parent = parents
    ), by = "treenum"
  )

meshTerm <- meshTerm |> mutate(root = str_extract(treenum, "^.")) |> group_by(meshterm) |> 
    mutate(flag = ifelse(n_distinct(root) > 1, T, F)) |> ungroup()

meshTerm <- bind_rows(
  meshTerm |> filter(flag) |> group_by(meshterm) |> 
    mutate(meshterm = sprintf("%s (%i)",meshterm, 1:n())) |> ungroup(), 
  meshTerm |> filter(!flag)
)

meshTerm <- meshTerm |> left_join(meshTerm |> select(parent = treenum, parentMesh = meshterm),
  by = "parent")

library(plotly)
fig <- plot_ly(
  data = meshTerm,
  type="treemap",
  labels= ~meshterm,
  parents= ~parentMesh,
  maxdepth = -1
)
fig
