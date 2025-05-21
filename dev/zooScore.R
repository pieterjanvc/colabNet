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

zooScore <- function(auID, nodeID, parent, lvl, n) {
  # Not enough data to build a tree
  if (n_distinct(auID) < 2) {
    return(NULL)
  }

  # Setup the run
  auIDs <- sort(unique(auID))
  finalScore <- combn(auIDs, 2) |> t()
  finalScore <- cbind(finalScore, 0)
  colnames(finalScore) <- c("au1", "au2", "score")

  # Start at leaves
  curLvl <- max(lvl)
  result <- list()

  # Process the tree level by level for all author pairs
  while (curLvl > 0) {
    # Get data for all nodes at lowest current level
    lvlData <- data.frame(auID, nodeID, parent, lvl, n) |>
      filter(lvl == curLvl)

    currentNodes <- lvlData |>
      select(nodeID, parent) |>
      distinct()

    # Run the node eval function for all nodes
    result <- map(currentNodes$nodeID, function(nodeID) {
      nodeEval(
        auID = lvlData$auID[lvlData$nodeID == nodeID],
        n = lvlData$n[lvlData$nodeID == nodeID],
        allID = auIDs,
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
          Reduce(
            `+`,
            lapply(matches, function(x) {
              x[, 3:5]
            })
          )
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

  as.data.frame(finalScore)
}

treeFromID <- function(uids) {
  treenums <- tbl(conn, "meshTree") |>
    filter(uid %in% local(unique(uids))) |>
    pull(treenum)

  treenums <- c(treenums, missingTreeNums(treenums))

  treenums <- tbl(conn, "meshTree") |>
    filter(treenum %in% local(treenums)) |>
    left_join(tbl(conn, "meshLink"), by = "uid") |>
    collect()
}

branchID <- function(parent, child, total_value = 1000) {
  if (length(parent) != length(child)) {
    stop("Error: 'parent' and 'child' vectors must be the same length.")
  }

  # Build adjacency list
  children <- split(child, parent)

  # Identify all nodes
  all_nodes <- unique(c(parent, child))

  # Find root (a node that is never a child)
  root <- setdiff(parent, child)
  if (length(root) != 1)
    stop(
      "Tree must have exactly one root. Found: ",
      paste(root, collapse = ", ")
    )
  root <- root[1]

  # Initialize maps
  node_to_id <- setNames(rep(NA, length(all_nodes)), all_nodes)
  node_values <- setNames(rep(0, length(all_nodes)), all_nodes)
  id_counter <- 0

  # Recursive DFS function
  dfs <- function(node, parent = NULL, value = total_value) {
    # Branch ID assignment
    if (!is.null(parent) && length(children[[parent]]) == 1) {
      node_to_id[[node]] <<- node_to_id[[parent]]
    } else {
      node_to_id[[node]] <<- id_counter
      id_counter <<- id_counter + 1
    }

    # Safe child lookup
    child_nodes <- if (!is.null(children[[node]])) children[[node]] else
      character(0)
    num_children <- length(child_nodes)

    if (num_children > 0) {
      node_values[[node]] <<- 0
      split_value <- value / num_children
      for (child in child_nodes) {
        dfs(child, node, split_value)
      }
    } else {
      node_values[[node]] <<- value
    }
  }

  dfs(root)
  return(list(branch_id = node_to_id, node_value = node_values))
}

paperMesh <- function(auIDs, dbInfo) {
  # Get all MeshTerms for papers
  tbl(conn, "coauthor") |>
    filter(auID %in% auIDs) |>
    left_join(
      tbl(conn, "mesh_article"),
      by = "arID"
    ) |>
    filter(!is.na(maID)) |>
    left_join(
      tbl(conn, "meshLink"),
      by = "meshui"
    ) |>
    collect()
}

authorMeshTree <- function(paperMesh, dbInfo) {
  tree <- treeFromID(paperMesh$uid)
  tree <- tree |>
    left_join(
      tbl(conn, "meshTerm") |>
        filter(meshui %in% local(unique(tree$meshui))) |>
        group_by(meshui) |>
        filter(row_number() == 1) |>
        collect(),
      by = "meshui"
    )

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

  tree <- bind_rows(meshRoots, tree)

  tree <- tree |>
    mutate(
      level = str_count(treenum, "\\.") + 2,
      level = as.integer(ifelse(is.na(uid), 1, level)),
      parent = str_remove(treenum, "\\.\\d+$"),
      parent = case_when(
        parent == treenum & is.na(uid) ~ "root",
        parent == treenum ~ str_extract(parent, "^\\w"),
        TRUE ~ parent
      )
    )

  branchIDs = branchID(
    tree$parent,
    tree$treenum
  )

  tree <- tree |>
    left_join(
      data.frame(
        treenum = names(branchIDs$branch_id),
        branchID = unname(branchIDs$branch_id),
        value = unname(branchIDs$node_value)
      ),
      by = "treenum"
    )

  tree <- tree |>
    left_join(
      tree |> select(parent = treenum, parentBranchID = branchID),
      by = "parent"
    )

  return(tree)
}

mergeTree <- function(tree) {
  # Merge branches with only one child into a single node
  tree <- tree |>
    group_by(branchID) |>
    summarise(
      parentBranchID = min(parentBranchID),
      leafNodeTreenum = treenum[n()],
      treenum = paste(treenum, collapse = " -> <br>"),
      meshterm = paste(meshterm, collapse = " -> <br>"),
      value = sum(value),
      .groups = "drop"
    )

  # Get the new parent info for the collapsed data and the root
  tree <- tree |>
    left_join(
      tree |>
        select(
          parent = treenum,
          parentBranchID = branchID,
          parentMeshterm = meshterm
        ) |>
        distinct(),
      by = "parentBranchID"
    ) |>
    mutate(
      parentMeshterm = ifelse(
        is.na(parentMeshterm),
        "MeSH Tree",
        parentMeshterm
      ),
      root = str_extract(tree$treenum, "^[^\\.\\s]+")
    )

  return(tree)
}

### TEST WITH ACTUAL DATA

dbSetup("data/PGG.db", checkSchema = T)
conn <- dbGetConn()
auIDs <- tbl(conn, "author") |>
  filter(authorOfInterest == 1) |>
  pull(auID)

papermesh <- paperMesh(auIDs)
tree <- authorMeshTree(papermesh)
plotData <- mergeTree(tree)

# Treemap
plot_ly(
  type = "treemap",
  ids = plotData$branchID,
  parents = plotData$parentBranchID,
  labels = plotData$meshterm,
  text = str_wrap(plotData$meshterm, 12),
  values = plotData$value,
  textinfo = "text",
  hovertext = plotData$meshterm,
  hoverinfo = "text",
  maxdepth = 2,
)

# SCORE CALCULATION

# Use this tree for author tree
dummy <- papermesh |>
  group_by(auID, meshui) |>
  summarise(n = n(), .groups = "drop") |>
  full_join(
    tree |>
      mutate(root = str_extract(treenum, "^.")),
    select(
      meshui,
      nodeID = branchID,
      parent = parentBranchID,
      lvl = level,
      root
    ) |>
      distinct(),
    by = "meshui",
    relationship = "many-to-many"
  ) |>
  mutate(
    auID = ifelse(is.na(auID), auID[1], auID),
    n = ifelse(is.na(n), 0, n),
  )

# Zoo score by tree
root <- unique(dummy$root)[1]
zooscore <- lapply(unique(dummy$root), function(root) {
  x <- dummy |> filter(root == {{ root }})
  zooScore(x$auID, x$treenum, x$parent, x$level, x$n)
})

test <- Reduce(rbind, zooscore) |>
  as.data.frame() |>
  group_by(au1, au2) |>
  summarise(zooscore = sum(score), .groups = "drop")

zooscore <- zooScore(dummy$auID, dummy$nodeID, dummy$parent, dummy$lvl, dummy$n)

# Add author names to zoo score for easier reading
au <- tbl(conn, "author") |>
  filter(auID %in% c(zooscore$au1, zooscore$au2)) |>
  select(auID) |>
  left_join(
    tbl(conn, "authorName") |> filter(default == 1)
  ) |>
  collect() |>
  rowwise() |>
  mutate(name = paste(lastName, firstName, sep = ", ")) |>
  select(auID, name)

zooscore <- zooscore |>
  left_join(
    au |> select(au1 = auID, au1Name = name),
    by = "au1"
  ) |>
  left_join(
    au |> select(au2 = auID, au2Name = name),
    by = "au2"
  ) |>
  arrange(desc(score))

authorTree <- dummy

# Add author names to zoo score for easier reading
au <- tbl(conn, "author") |>
  filter(auID %in% local(unique(authorTree$auID))) |>
  select(auID) |>
  left_join(
    tbl(conn, "authorName") |> filter(default == 1),
    by = "auID"
  ) |>
  collect() |>
  rowwise() |>
  mutate(name = paste(lastName, firstName, sep = ", ")) |>
  select(auID, name)

authorTree <- authorTree |>
  left_join(
    au |> select(auID, name),
    by = "auID"
  ) |>
  mutate(
    name = ifelse(n == 0, "", name)
  )

nodeSum <- function(parent, child, value) {
  # Coerce to character to avoid indexing issues
  parent <- as.character(parent)
  child <- as.character(child)

  if (length(parent) != length(child)) {
    stop("Error: 'parent' and 'child' vectors must be the same length.")
  }

  # Build adjacency list
  children <- split(child, parent)

  # Identify all nodes
  all_nodes <- unique(c(parent, child))

  # Find root (a node that is never a child)
  root <- setdiff(parent, child)
  if (length(root) != 1)
    stop(
      "Tree must have exactly one root. Found: ",
      paste(root, collapse = ", ")
    )
  root <- root[1]

  # Map values to child nodes
  node_values_raw <- c(setNames(0, "0"), setNames(value, child))

  # Initialize result container
  node_values <- setNames(rep(0, length(all_nodes)), all_nodes)

  # Recursive DFS
  dfs <- function(node) {
    node_value <- node_values_raw[[node]]
    if (is.null(node_value)) node_value <- 0

    total <- node_value

    for (child_node in children[[node]]) {
      total <- total + dfs(child_node)
    }

    node_values[[node]] <<- total
    return(total)
  }

  dfs(root)
  return(data.frame(node = names(node_values), value = unname(node_values)))
}


treemap <- function(authorTree) {
  # Merge branches with only one child into a single node
  tree <- authorTree |>
    arrange(treenum) |>
    group_by(branchID) |>
    summarise(
      parentBranchID = min(parentBranchID),
      meshterm = paste(unique(meshterm), collapse = " -> "),
      value = sum(value),
      n = sum(n),
      authors = paste(
        sort(unique(sprintf("%s (%i)", name, n))),
        collapse = "\n"
      ),
      root = root[1],
      .groups = "drop"
    ) |>
    mutate(
      authors = ifelse(authors == " (0)", "", authors)
    )

  tree <- tree |>
    mutate(parentBranchID = ifelse(is.na(parentBranchID), 0, parentBranchID))
  meshSum <- nodeSum(
    parent = tree$parentBranchID,
    child = tree$branchID,
    value = tree$n
  ) |>
    transmute(branchID = as.integer(node), meshSum = value)

  tree <- tree |> left_join(meshSum, by = "branchID")

  # # Get the new parent info for the collapsed data and the root
  # tree <- tree |>
  #   left_join(
  #     tree |>
  #       select(
  #         parent = treenum,
  #         parentBranchID = branchID,
  #         parentMeshterm = meshterm
  #       ) |>
  #       distinct(),
  #     by = "parentBranchID"
  #   ) |>
  #   mutate(
  #     parentMeshterm = ifelse(
  #       is.na(parentMeshterm),
  #       "MeSH Tree",
  #       parentMeshterm
  #     ),
  #     root = str_extract(tree$treenum, "^[^\\.\\s]+")
  #   )

  return(tree)
}

plotData <- treemap(authorTree)

# Treemap
plot_ly(
  type = "treemap",
  ids = plotData$branchID,
  parents = plotData$parentBranchID,
  labels = paste(plotData$meshSum, plotData$meshterm, sep = " | "),
  text = str_wrap(paste(plotData$meshSum, plotData$meshterm, sep = " | "), 12),
  values = plotData$value,
  textinfo = "text",
  hovertext = plotData$authors,
  hoverinfo = "text",
  maxdepth = 3,
)

# branchID <- function(parent, child) {
#   # Build adjacency list
#   children <- split(child, parent)

#   # Identify all nodes
#   all_nodes <- unique(c(parent, child))

#   # Find root (a node that is never a child)
#   root <- setdiff(parent, child)
#   if (length(root) != 1)
#     stop(
#       "Tree must have exactly one root. Found: ",
#       paste(
#         root,
#         collapse = ", "
#       )
#     )
#   root <- root[1]

#   # Initialize ID map
#   node_to_id <- setNames(rep(NA, length(all_nodes)), all_nodes)
#   id_counter <- 0

#   # Recursive DFS function
#   dfs <- function(node, parent = NULL) {
#     # Check if this node continues a linear chain from parent
#     if (!is.null(parent) && length(children[[parent]]) == 1) {
#       node_to_id[[node]] <<- node_to_id[[parent]]
#     } else {
#       node_to_id[[node]] <<- id_counter
#       id_counter <<- id_counter + 1
#     }

#     for (child in children[[node]]) {
#       dfs(child, node)
#     }
#   }

#   dfs(root)
#   return(node_to_id)
# }

# # Get the full diftree
# difftree <- diffTree(auIDs, pruneDuplicates = F)

# # Only consider tree D (Chemicals and Drugs) for test
# simpleTree <- difftree |> filter(str_detect(treenum, "^D"))

# # Transform into table compatible with algorithm
# dummy <- simpleTree |>
#   select(x = parent, nodeID = mtrID, auID = auIDs, lvl = level, hasArticle)
# dummy <- dummy |>
#   left_join(
#     simpleTree |> select(x = treenum, parent = mtrID),
#     by = "x"
#   ) |>
#   select(-x) |>
#   mutate(
#     parent = ifelse(is.na(parent), 0, parent),
#     lvl = lvl + 1
#   ) |>
#   separate_rows(auID, sep = ",") |>
#   mutate(
#     n = 1,
#     across(everything(), as.integer)
#   )

# # Add the root
# dummy <- bind_rows(
#   data.frame(
#     nodeID = as.integer(0),
#     auID = 1,
#     lvl = 1,
#     parent = NA,
#     n = as.integer(0)
#   ),
#   dummy
# )

## COUNT papers
# toCheck <- auIDs
# toCheck <- c(1084, 4924)
# test <- tbl(conn, "coauthor") |>
#   filter(auID %in% toCheck) |>
#   left_join(
#     tbl(conn, "mesh_article"),
#     by = "arID"
#   ) |>
#   filter(!is.na(maID)) |>
#   left_join(
#     tbl(conn, "meshLink"),
#     by = "meshui"
#   ) |>
#   collect()

# left_join(
#   tbl(conn, "meshTree") |> select(mtrID, uid),
#   by = "uid"
# ) |>
#   left_join(
#     tbl(conn, "meshTerm") |> select(meshui, meshterm),
#     by = "meshui"
#   ) |>
#   collect()

# test1 <- tbl(conn, "meshTree") |>
#   filter(uid %in% local(unique(test$uid))) |>
#   collect()
# missingTreeNums(test1$treenum)

# treenums <- c("D08.811.682.047.820.475")

# treeIntegrityCheck <- function(treenums) {
#   toAdd = c()
#   # Check if a treenum exists and any missing previous nodes
#   while (length(treenums) > 0) {
#     missing = setdiff(
#       treenums,
#       tbl(conn, "meshTree") |> filter(treenum %in% treenums) |> pull(treenum)
#     )

#     # Keep track of missing ones
#     toAdd = c(toAdd, missing)

#     # Go up one level for remaining
#     treenums <- str_remove(missing, "\\.\\d+$|^[^.]+$")
#     treenums <- treenums[treenums != ""]
#   }
# }
