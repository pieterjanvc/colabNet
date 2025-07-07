file.copy("data/PGG_dev.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"


# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()


auIDs <- tbl(pool, "author") |>
  filter(authorOfInterest == 1) |>
  pull(auID)

papermesh <- dbPaperMesh(auIDs)
meshtree <- dbMeshTree(papermesh)
papermeshtree <- paperMeshTree(papermesh, meshtree)

# Add author names
au <- tbl(pool, "author") |>
  filter(auID %in% local(unique(papermeshtree$auID))) |>
  select(auID) |>
  left_join(tbl(pool, "authorName") |> filter(default == 1), by = "auID") |>
  collect() |>
  rowwise() |>
  mutate(name = paste(lastName, firstName, sep = ", ")) |>
  select(auID, name)

papermeshtree <- papermeshtree |>
  left_join(au |> select(auID, name), by = "auID") |>
  mutate(name = ifelse(nPapers == 0, "", name))

plotData <- treemapData(papermeshtree)
zooscore <- zooScore(papermeshtree)

test <- zooscore |> group_by(au1, au2) |> summarise(
  zooscore = sum(score), .groups = "drop"
) |>
  left_join(au |> select(au1 = auID, au1_name = name), by = "au1") |>
  left_join(au |> select(au2 = auID, au2_name = name), by = "au2") |>
  arrange(desc(zooscore))

# TREE MAP COMPARISON

auIDs = c(1053, 1673)
plotData <- treeMapComparison(auIDs[1], auIDs[2])

# Render the plot
boxText <- str_wrap(paste(plotData$meshSum, plotData$meshterm, sep = " | "), 12)
boxText <- ifelse(plotData$hasChildren, paste(boxText, "<b>+</b>"), boxText)

plot_ly(
  type = "treemap",
  ids = plotData$branchID,
  parents = plotData$parentBranchID,
  labels = ifelse(
    is.na(plotData$meshSum),
    plotData$meshterm,
    paste(plotData$meshSum, plotData$meshterm, sep = " | ")
  ),
  text = boxText,
  values = plotData$treemapVal,
  marker = list(colors = plotData$colour),
  textinfo = "text",
  hovertext = plotData$authors,
  hoverinfo = "text",
  maxdepth = 3,
  source = "treemap"
)


