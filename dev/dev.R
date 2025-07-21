file.copy("data/PGG_dev.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"


# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()

topN = 20

# Original
# backup <- plotData
backup <- treemapData(papermeshtree)

plotData <- backup

# Only get the top scoring n branches per level
plotData <- backup |>
  group_by(parentBranchID) |>
  slice_max(meshSum, n = topN, na_rm = F) |>
  ungroup()

# Trim dead branches (parent removed)
n = 0
while (nrow(plotData) != n) {
  n = nrow(plotData)
  plotData <- plotData |>
    filter(parentBranchID %in% branchID | is.na(parentBranchID))
}

# Calculate the balancing values
plotData <- plotData |>
  left_join(
    treemapBalance(plotData$branchID, plotData$parentBranchID),
    by = c("branchID" = "id")
  ) |>
  arrange(meshterm)

# PLOT
boxText <- str_wrap(
  paste(plotData$meshSum, plotData$meshterm, sep = " | "),
  12
)
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
  values = plotData$balanceVal,
  marker = list(colors = treemapColour(plotData$meshSum)),
  textinfo = "text",
  hovertext = plotData$authors,
  hoverinfo = "text",
  maxdepth = 2,
  source = "treemap_overview"
)

lcm <- function(ints) {
  gcd <- function(x, y) {
    r <- x %% y
    return(ifelse(r, gcd(y, r), y))
  }

  lcm = 1
  for (x in unique(ints)) {
    lcm = (lcm * x) %/% gcd(lcm, x)
  }

  return(lcm)
}


test <- data.frame(
  id = c(1:9),
  parent = c(NA, 1, 1, 2, 3, 3, 4, 1, 1),
  value = c(0, 0, 0, 0, 1, 1, 2, 2, 2)
) |>
  mutate(
    hasChildren = id %in% parent
  )

balanceVal <- test |>
  filter(!hasChildren & parent > 1) |>
  group_by(parent) |>
  summarise(n = n()) |>
  pull(n) |>
  lcm()

test <- test |>
  group_by(parent) |>
  mutate(
    balanceVal = case_when(
      hasChildren ~ 0,
      parent == 1 ~ balanceVal,
      TRUE ~ balanceVal / sum(!hasChildren)
    )
  ) |>
  ungroup()

plot_ly(
  type = "treemap",
  ids = test$id,
  parents = test$parent,
  labels = test$id,
  values = test$balanceVal
)


balanceVal <- plotData |>
  filter(!hasChildren) |>
  group_by(parentBranchID) |>
  summarise(n = n()) |>
  pull(n) |>
  lcm()

plotData <- plotData |>
  group_by(parentBranchID) |>
  mutate(
    balanceVal = case_when(
      hasChildren ~ 0,
      parentBranchID == 0 ~ balanceVal,
      TRUE ~ balanceVal / sum(!hasChildren)
    )
  ) |>
  ungroup()

boxText <- str_wrap(
  paste(plotData$leafVal, plotData$meshterm, sep = " | "),
  12
)
boxText <- ifelse(plotData$hasChildren, paste(boxText, "<b>+</b>"), boxText)

plot_ly(
  type = "treemap",
  ids = plotData$branchID,
  parents = plotData$parentBranchID,
  labels = paste(plotData$leafVal, plotData$meshterm, sep = " | "),
  text = boxText,
  values = plotData$leafVal,
  marker = list(colors = treemapColour(plotData$meshSum)),
  textinfo = "text",
  hovertext = plotData$authors,
  hoverinfo = "text",
  maxdepth = 2,
  source = "treemap_overview"
)

unique(plotData$leafVal)

lcm(c(12, 6, 2))
30 / 5

# Find the leaf nodes
# Get all unique parents for leaf nodes
# Check how many siblings these parents have who's children are leaves
# Multiply the parent's sibling number by number of children
# take lcm
# For each parent, divide the lcm by the siblings number
# Now distribute this value among the children

leafVals <- function(id, parent) {
  # Create a df with ids and parents
  tree <- data.frame(
    id = id,
    parent = parent
  ) |>
    mutate(hasChildren = id %in% parent)

  family <- tree

  # Find the leaf nodes' parents
  leafParents = family |> filter(!hasChildren) |> pull(parent) |> unique()

  # Check how many siblings these parents have who's children are leaves
  leafGrandparents = family |> filter(id %in% leafParents) |> pull(parent)

  family <- family |>
    filter(parent %in% leafGrandparents) |>
    group_by(parent) |>
    mutate(siblings = sum(id %in% leafParents)) |>
    ungroup() |>
    left_join(
      family |>
        filter(!hasChildren) |>
        group_by(id = parent) |>
        summarise(children = n(), .groups = "drop"),
      by = "id"
    ) |>
    select(parent = id, siblings, children) |>
    filter(!is.na(children))

  # Multiply the parent's sibling number by number of children
  # take lcm
  # For each parent, divide the lcm by the siblings number
  # Now distribute this value among the children
  family <- family |>
    mutate(
      leafVal = lcm(siblings * children) / (siblings * children)
    )

  tree |>
    left_join(family |> select(parent, leafVal), by = "parent") |>
    mutate(leafVal = ifelse(is.na(leafVal), 0, leafVal)) |>
    select(id, leafVal)
}

plotData <- data.frame(
  id = c(1:18),
  parent = c(NA, 1, 1, 2, 2, 4, 4, 4, 5, 5, 3, 3, 3, 3, 3, 10, 10, 9)
)

split(plotData$id, plotData$parent)

plotData <- plotData |>
  left_join(leafVals(plotData$id, plotData$parent), by = "id")

boxText <- str_wrap(
  paste(plotData$leafVal, plotData$id, sep = " | "),
  12
)

plot_ly(
  type = "treemap",
  ids = plotData$id,
  parents = plotData$parent,
  labels = paste(plotData$leafVal, plotData$id, sep = " | "),
  text = boxText,
  values = plotData$leafVal,
  textinfo = "text",
  maxdepth = -1,
  source = "treemap_overview"
)

## NEW APPROACH

id = c(1:19)
parent = c(NA, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 9, 9, 6, 6, 6)

# Build tree as parent / child list
tree = split(id, parent)

# Get the level of each node
lvl = rep(NA, length(id))
current = id[!parent %in% id]

curLvl = 1
while (length(current) > 0) {
  lvl[current] = curLvl
  current = tree[as.character(current)] |> unlist()
  curLvl = curLvl + 1
}

# Update the values to balance the tree at each level

# Lowest level vals are set by default to 1
val = rep(0, length(id))
val[lvl == max(lvl)] = 1
curLvl = max(lvl) - 1
nodeTotal = val # Cumalitive sum of node and children

while (curLvl > 1) {
  # --- Merge in values from previous level
  newVals = id[curLvl == lvl]
  newVals <- tree[as.character(newVals)]
  newVals <- newVals[!is.na(names(newVals))]
  newVals <- sapply(newVals, function(x) sum(nodeTotal[x]))

  val[names(newVals) |> as.integer()] <- newVals
  nodeTotal[names(newVals) |> as.integer()] <- newVals

  # --- Balance all values per parent

  # Get the current level node IDs
  nodes <- id[curLvl == lvl + 1]
  nodes <- tree[as.character(nodes)]
  # Balance by parent
  toAdd <- sapply(nodes, function(x) {
    max(val[x]) - val[x]
  }) |>
    unlist(use.names = F)
  #Add balance values
  val[unlist(nodes, use.names = F)] <- toAdd
  nodeTotal[unlist(nodes, use.names = F)] <- nodeTotal[unlist(
    nodes,
    use.names = F
  )] +
    toAdd

  curLvl = curLvl - 1
}

setNames(val, id)
setNames(nodeTotal, id)


test <- val

test[c(4, 5)] = test[c(4, 5)] + 3
boxText <- str_wrap(
  sprintf("ID %i - val %i", id, test),
  12
)
plot_ly(
  type = "treemap",
  ids = id,
  values = c(
    0,
    0,
    0,
    0,
    0,
    0,
    30,
    30,
    0,
    20,
    20,
    20,
    30,
    30,
    15,
    15,
    10,
    10,
    10
  ),
  parents = parent,
  labels = sprintf("ID %i - val %i", id, test),
)

lcm(c(1:40))


### Simple appraoch

id = c(1:20)
parent = c(NA, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 9, 9, 6, 6, 6, 1)

# # Make sure ids are consecutive
# tempID <- id |> as.factor() |> as.integer()
# tempParent <- setNames(tempID, id)[as.character(parent)]

leaves = id[!id %in% parent]
val = rep(0, length(id))

# Build tree as parent / child list
tree = split(id, parent)

# Get the level of each node
lvl = rep(NA, length(id))
current = id[!parent %in% id]

curLvl = 1
while (length(current) > 0) {
  lvl[current] = curLvl
  current = tree[as.character(current)] |> unlist()
  curLvl = curLvl + 1
}

curVal = 1
for (curLvl in 2:max(lvl)) {
  nodes <- id[lvl == curLvl]
  toAssign <- leaves[leaves %in% nodes]

  if (length(toAssign) > 0) {
    val[toAssign] = curVal
  }
}

leafVal <- function(curID, curVal) {
  if (curID %in% leaves) {
    # print(sprintf("val for %i = %f", curID, curVal))
    return(c(curID, curVal))
  } else {
    ids <- id[parent == curID]
    ids <- ids[!is.na(ids)]
    results <- lapply(ids, leafVal, curVal = curVal / length(ids))
    return(do.call(cbind, results))
  }
}

leafVal(1, 48)


id <- test$branchID
parent <- test$parentBranchID

parent = c(NA, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 9, 9, 6, 6, 6)
id = c(1:length(parent))


id <- c(4, 6, 8, 9, 11, 13, 17, 23)
parent <- c(NA, 4, 4, 6, 6, 6, 8, 8)

rand <- sample(1:length(id), length(id))
id <- id[rand]
parent <- parent[rand]

# WITH INTERGERS (but overflow error when too big)
getVals <- function(id, parent) {
  # Remove any duplicated IDs
  remDupl <- !duplicated(id)
  id <- id[remDupl]
  parent <- parent[remDupl]
  # Get the root and leave IDs
  root <- id[is.na(parent)]
  if (length(root) != 1) {
    stop("The must be exactly one root (i.e parent = NA)")
  }
  leaves <- id[!id %in% parent]
  # Function to find the total sum of final leaves
  leafSum <- function(curID, curVal) {
    if (curID %in% leaves) {
      return(curVal)
    } else {
      ids <- id[parent == curID]
      ids <- ids[!is.na(ids)]
      results <- sapply(ids, leafSum, curVal = curVal * length(ids))
      return(unlist(results))
    }
  }
  #Function to find the leave values
  leafVal <- function(curID, curVal) {
    if (curID %in% leaves) {
      return(c(curID, curVal))
    } else {
      ids <- id[parent == curID]
      ids <- ids[!is.na(ids)]
      results <- lapply(ids, leafVal, curVal = curVal / length(ids))
      return(do.call(cbind, results))
    }
  }

  leafsum <- lcm(leafSum(root, 1))
  leafVal(root, leafsum)
}

# Floating point number (less exact but works for big trees)
getVals2 <- function(id, parent) {
  # Remove any duplicated IDs
  remDupl <- !duplicated(id)
  id <- id[remDupl]
  parent <- parent[remDupl]
  # Get the root and leave IDs
  root <- id[is.na(parent)]
  if (length(root) != 1) {
    stop("The must be exactly one root (i.e parent = NA)")
  }
  leaves <- id[!id %in% parent]
  #Function to find the leave values
  leafVal <- function(curID, curVal) {
    if (curID %in% leaves) {
      return(list(id = curID, val = curVal))
    } else {
      ids <- id[parent == curID]
      ids <- ids[!is.na(ids)]
      results <- lapply(ids, leafVal, curVal = curVal / length(ids))
      return(do.call(Map, c(f = c, results)))
    }
  }

  leafVal(root, 1)
}


# Get the ID mappings for the current
test <- papermeshtree |>
  # filter(root %in% LETTERS[1:6]) |>
  select(branchID, parentBranchID)
test$parentBranchID[is.na(test$parentBranchID)] <- 0
test <- rbind(c(0, NA), test)
id <- test$branchID
parent <- test$parentBranchID

result <- getVals(id, parent)
result <- getVals2(id, parent) |> as.data.frame()

max(result$val)

plotData <- backup

treemapBalance(plotData$branchID, plotData$parentBranchID)

result <- getVals2(plotData$branchID, plotData$parentBranchID) |>
  as.data.frame()

plotData <- plotData |>
  left_join(result, by = c("branchID" = "id")) |>
  mutate(val = ifelse(is.na(val), 0, val))


boxText <- str_wrap(
  paste(plotData$val, plotData$meshterm, sep = " | "),
  12
)

plot_ly(
  type = "treemap",
  ids = plotData$branchID,
  parents = plotData$parentBranchID,
  labels = paste(plotData$val, plotData$meshterm, sep = " | "),
  text = boxText,
  values = plotData$val,
  textinfo = "text",
  maxdepth = 2,
  source = "treemap_overview"
)
