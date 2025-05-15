library(tidyverse)
library(visNetwork)

# Dataset

# dummy <- tribble(
#   ~auID, ~nodeID, ~lvl, ~parent, ~n,
#   1, 5, 3, 3, 3,
#   2, 5, 3, 3, 1,
#   2, 4, 3, 3, 1,
#   1, 3, 2, 1, 1,
#   1, 3, 2, 1, 1,
#   2, 2, 2, 1, 1,
#   1, 1, 1, NA, 0
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
nodeEval <- function(auID, n, allID) {
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
    n[m[, 1]],
    n[m[, 2]]
  )
  # Col 5 is score for the pair
  m <- cbind(m, apply(matrix(m[, 3:4], ncol = 2), 1, min))
  # Update col 3-4 with remainder
  m[, 3:4] <- m[, 3:4] - m[, 5]

  colnames(m) <- c("au1", "au2", "rem1", "rem2", "score")

  return(m)
}


# Get data for all nodes at lowest current level
auIDs <- sort(unique(dummy$auID))
currentNodes = dummy |>
  filter(lvl == max(lvl)) |>
  group_by(nodeID) |>
  summarise(
    scoreMatrix = nodeEval(auID, n, auIDs)
  )

nodeEval(1, 1, auIDs)
