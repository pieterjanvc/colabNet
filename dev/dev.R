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
diffTree <- bind_rows(amt1$auTree, amt2$auTree) |> group_by(treenum) |> 
  mutate(auID = ifelse(n() == 1, auID, 0) |> as.integer()) |> 
  ungroup() |> distinct() |> 
  mutate(level = as.integer(str_count(treenum, "\\.") + 1))

# #Check the higest level overlaps (furthest from the root)
# diffTree <- diffTree |> filter(overlap)

# lvl <- max(diffTree$level)
# toCheck <- diffTree |> mutate(root = str_extract(treenum, "^."))
# result <- data.frame()
# while(lvl > 0){
#   toKeep <- toCheck |> filter(level == {{lvl}})
#   toRemove <- missingTreeNums(toKeep$treenum)

#   result <- bind_rows(result, toKeep)
#   toCheck <- toCheck |> filter(!treenum %in% c(toKeep$treenum, toRemove))
#   lvl <- lvl - 1
  
# }

# test <- result |> select(uid, level, treenum, root) |> 
#   group_by(uid, root) |> filter(level == max(level)) |> ungroup()

meshTerm <- tbl(conn, "meshLink") |> filter(uid %in% local(diffTree$uid)) |> 
  left_join(tbl(conn, "meshTerm"), by = "meshui", na_matches = "never") |> distinct() |> 
  # If there are multiple term descriptions, only keep one
  group_by(uid) |> filter(mteID == min(mteID)) |> ungroup() |> collect()

# test <- test |> left_join(meshTerm, by = "uid")

# plotData <- c(test$treenum, missingTreeNums(test$treenum))
# parents <- str_remove(plotData, "\\.\\d+$")
# parents <- ifelse(plotData == parents, NA, parents)

# Get the parent for each treenum (NA =  root)
diffTree <- diffTree |> mutate(
  parent = str_remove(diffTree$treenum, "\\.\\d+$"),
  parent = ifelse(diffTree$treenum == parent, "", parents)
)

# Add the number of children for each treenum
diffTree <- diffTree |> left_join(
  diffTree |> group_by(treenum = parent) |> summarise(children = n(), .groups = "drop"),
  by = "treenum"
) |> mutate(children = as.integer(ifelse(is.na(children), 0, children))) |> 
  arrange(treenum)

# Check if treenums can be merged if they don't branch off
b = 1
bID = c(b, rep(NA, nrow(diffTree) - 1))
for(i in 2:nrow(diffTree)){
  
  if(diffTree$parent[i] != diffTree$treenum[i-1] | diffTree$treenum[i] == "" |
    diffTree$children[i] > 1 |diffTree$children[i-1] > 1){
    b = b + 1
  }
  # if(diffTree$children[i] != 1 | diffTree$children[i-1] > 1 | 
  #   diffTree$parent[i-1] == "" | diffTree$parent[i] != diffTree$parent[i-1]){
  #   b = b + 1
  # }  
  bID[i] = b
}

diffTree <- diffTree |> 
  mutate(
    branchID = as.integer({{bID}})
    # x = lag(branchID),
    # branchID = ifelse(children == 0, x, branchID)
  ) |>  select(treenum, branchID, children, parent, everything())

diffTree <- diffTree |> left_join(
  diffTree |> select(parent = treenum, parentBranchID = branchID),
  by = "parent"
)

plotData <- diffTree |> group_by(branchID) |> summarise(
  parentBranchID = min(parentBranchID),
  treenum = paste(treenum, collapse = " -> <br>"),
  .groups = "drop"
)

plotData <- plotData |> left_join(
  plotData |> select(parent = treenum, parentBranchID = branchID),
  by = "parentBranchID"
)

fig <- plotly::plot_ly(
  type="treemap",
  labels= plotData$treenum,
  parents= plotData$parent,
  maxdepth = -1
)
fig

#---
parents <- str_remove(diffTree$treenum, "\\.\\d+$")
parents <- ifelse(diffTree$treenum == parents, NA, parents)

fig <- plotly::plot_ly(
  type="treemap",
  labels= diffTree$treenum,
  parents= parents,
  maxdepth = -1
)
fig

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

  fig <- plotly::plot_ly(
    data = meshTerm,
    type="treemap",
    labels= ~meshterm,
    parents= ~parentMesh,
    maxdepth = -1
  )
  fig
