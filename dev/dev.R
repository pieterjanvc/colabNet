file.copy("data/PGG_dev.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"


# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()

auIDs = c(1053, 1673)
treemapFromIDs <- function(auIDs){

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

  return(treemapData(papermeshtree))
}

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

dbDisconnect(pool)

test <- zooscore |> group_by(au1, au2) |> summarise(
  zooscore = sum(score), .groups = "drop"
) |>
  left_join(au |> select(au1 = auID, au1_name = name), by = "au1") |>
  left_join(au |> select(au2 = auID, au2_name = name), by = "au2") |>
  arrange(desc(zooscore))

plotData <- treemapFromIDs(c(1053, 1673))
boxText <- str_wrap(paste(plotData$meshSum, plotData$meshterm, sep = " | "), 12)
boxText <- ifelse(plotData$hasChildren, paste(boxText, "<b>+</b>"), boxText)

plotData <- plotData |>
  mutate(
    authors = str_remove(authors, "^\n"), # Because sometimes starts with \n
    colourCode = case_when(
    is.na(authors) ~ 1,
    authors == "Perrimon, Norbert" ~ 2,
    authors == "Church, George M" ~3,
    authors == "Church, George M\nPerrimon, Norbert" ~ 4,
    TRUE ~ 1
  ))

test <- plotData

plotData <- test

toUpdate <- plotData |> filter(!hasChildren) |>
  select(branchID = parentBranchID, childCol = colourCode) |>
  group_by(branchID) |>
  mutate(childCol = case_when(
    all(2:3 %in% childCol) ~ 4,
    TRUE ~ max(childCol)
  )) |>
  summarise(
    childCol = childCol[1], .groups = "drop"
  ) |>
  ungroup() |> distinct()

while(nrow(toUpdate) > 0){
  plotData <- plotData |> left_join(toUpdate, by = "branchID") |>
    mutate(colourCode = case_when(
      is.na(childCol) ~ colourCode,
      childCol > colourCode ~ childCol,
      TRUE ~ colourCode
    ))

  toUpdate <- plotData |> filter(!is.na(childCol)) |>
    select(branchID = parentBranchID, childCol = colourCode) |>
    group_by(branchID) |>
    mutate(childCol = case_when(
      all(2:3 %in% childCol) ~ 4,
      TRUE ~ max(childCol, 1)
    )) |>
    summarise(
      childCol = childCol[1], .groups = "drop"
    ) |>
    ungroup() |> distinct()

  plotData <- plotData |> select(-childCol)
}

colSel = c("#CCCCCC", "#3398DB", "#F1C40E", "#7aa64c")

plotData <- plotData |>
  group_by(colourCode) |>
  mutate(
    colour = treemapColour(meshSum,
                           minCol = lightenColour(colSel[colourCode[1]], 0.9),
                           maxCol = colSel[colourCode[1]])
  ) |>  ungroup(  )

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


