library(tidyverse)
library(visNetwork)

# Dataset

# dummy <- tribble(
#   ~auID, ~nodeID, ~lvl, ~parent, ~n,
#   1,13,5,11,1,
#   2,14,5,11,3,
#   1,12,4,7,10,
#   1,11,4,7,1,
#   3,10,4,7,3,
#   1,8,4,5,3,
#   3,8,4,5,2,
#   1,9,4,5,2,
#   1,4,3,2,1,
#   1,5,3,2,0,
#   3,6,3,2,4,
#   1,7,3,3,4,
#   1,2,2,1,1,
#   1,3,2,1,2,
#   1,1,1,NA,0
# )


# Show tree
nodes <- dummy |>
  group_by(id = nodeID, level = lvl) |>
  summarise(
    label = paste(paste("A", auID, "/", n, sep = ""), collapse = " "),
    .groups = "drop"
  )

edges <- dummy |>
  select(from = nodeID, to = parent) |>
  distinct()

visNetwork(nodes, edges, width = "100%", height = "1000px") %>%
  visHierarchicalLayout()

# ---- Algorithm ----

# Function to calculate node score and remainder to be pushed up to parent
nodeEval <- function(auID, n, allID, carry) {
  # Fill in missing authors with 0 papers
  missing <- setdiff(allID, auID)
  auID <- c(auID, missing)
  n <- c(n, rep(0, length(missing)))
  # Order by ID for easier merge later
  o <- order(auID)
  auID <- auID[o]
  n <- n[o]

  # Create the scoring matrix
  # Cols 1-2 are author ID combinations
  m <- combn(auID, 2) |> t()
  # Col 3-4 are number of papers for each pair
  m <- cbind(
    m,
    n[match(m[, 1], auID)],
    n[match(m[, 2], auID)]
  )
  # Add any remainders from previous levels
  if (length(carry) > 0) {
    m[, 3:4] <- m[, 3:4] + carry[, c("rem1", "rem2")]
  }

  # Col 5 is score for the pair
  m <- cbind(m, apply(matrix(m[, 3:4], ncol = 2), 1, min))
  # Update col 3-4 with remainder
  m[, 3:4] <- m[, 3:4] - m[, 5]

  colnames(m) <- c("au1", "au2", "rem1", "rem2", "score")

  return(m)
}

# Setup the run
auIDs <- sort(unique(dummy$auID))
finalScore <- combn(sort(auIDs), 2) |> t()
finalScore <- cbind(finalScore, 0)
colnames(finalScore) <- c("au1", "au2", "score")

# Start at leaves
curLvl <- max(dummy$lvl)
result <- list()

# Process the tree level by level for all author pairs
while (curLvl > 0) {
  # Get data for all nodes at lowest current level
  lvlData <- dummy |>
    filter(lvl == curLvl)

  currentNodes <- lvlData |>
    select(nodeID, parent) |>
    distinct()

  # Run the node eval function for all nodes
  result <- map(currentNodes$nodeID, function(nodeID) {
    nodeEval(
      auID = lvlData$auID[lvlData$nodeID == nodeID],
      n = lvlData$n[lvlData$nodeID == nodeID], allID = auIDs,
      carry = result[[as.character(nodeID)]]
    )
  })

  # If there are multiple nodes feeding into the same parent, merge them
  #  this cannot happen when you are at the root
  if (curLvl > 1) {
    result <- map(currentNodes$parent |> unique(), function(parent) {
      matches <- which(currentNodes$parent == parent)
      matches <- result[matches]

      if (length(matches) == 1) {
        return(matches[[1]])
      }

      cbind(
        matches[[1]][, 1:2],
        Reduce(`+`, lapply(matches, function(x) {
          x[, 3:5]
        }))
      )
    })
  }

  names(result) <- currentNodes$parent |> unique()

  # Add the level scores to the final score
  finalScore[, "score"] <- finalScore[, "score"] +
    rowSums(sapply(result, function(x) x[, "score"])) *
      curLvl

  # Next level
  curLvl <- curLvl - 1
}

finalScore

### TEST WITH ACTUAL DATA

dbSetup("data/PGG.db", checkSchema = T)
conn <- dbGetConn()
auIDs <- tbl(conn, "author") |>
  filter(authorOfInterest == 1) |>
  pull(auID)

# Get the full diftree
difftree <- diffTree(auIDs, pruneDuplicates = T)

# Only consider tree D (Chemicals and Drugs) for test
simpleTree <- difftree |> filter(str_detect(treenum, "^D"))

# Transform into table compatible with algorithm
dummy <- simpleTree |> select(x = parent, nodeID = mtrID, auID = auIDs, lvl = level)
dummy <- dummy |>
  left_join(
    simpleTree |> select(x = treenum, parent = mtrID),
    by = "x"
  ) |>
  select(-x) |>
  mutate(
    parent = ifelse(is.na(parent), 0, parent),
    lvl = lvl + 1
  ) |>
  separate_rows(auID, sep = ",") |>
  mutate(
    n = 1,
    across(everything(), as.integer)
  )

# Add the root
dummy <- bind_rows(data.frame(
  nodeID = as.integer(0), auID = 1, lvl = 1, parent = NA, n = as.integer(0)
), dummy)

test <- as.data.frame(finalScore)

au <- tbl(conn, "author") |>
  filter(auID %in% c(test$au1, test$au2)) |>
  select(auID) |>
  left_join(
    tbl(conn, "authorName") |> filter(default == 1)
  ) |>
  collect() |>
  rowwise() |>
  mutate(name = paste(lastName, firstName, sep = ", ")) |>
  select(auID, name)

test <- test |>
  left_join(
    au |> select(au1 = auID, au1Name = name),
    by = "au1"
  ) |>
  left_join(
    au |> select(au2 = auID, au2Name = name),
    by = "au2"
  ) |>
  arrange(desc(score))
