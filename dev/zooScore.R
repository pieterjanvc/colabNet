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

paperMat <- function(auID, allID, n, m) {
  # Fill in missing authors with 0 papers
  missing <- setdiff(allID, auID)
  auID <- c(auID, missing)
  n <- c(n, rep(0, length(missing)))

  # Order by ID for easier merge later
  o <- order(auID)
  auID <- auID[o]
  n <- n[o]

  # Create the scoring array
  # Cols 1-2 are author ID combinations (m)
  # Col 3-4 are number of papers for each pair, 5 is the score
  cbind(
    m,
    n[match(m[, 1], auID)],
    n[match(m[, 2], auID)],
    0
  )
}

# Function to calculate node score and remainder to be pushed up to parent
nodeEval <- function(lvlData, allID, carry, carryOrder, m) {
  a <- lvlData |>
    select(auID, mtrID, nPapers) |>
    group_by(mtrID) |>
    group_map(~ paperMat(.x$auID, allID, .x$nPapers, m)) |>
    simplify2array()

  # Add any remainders from previous levels
  if (length(carry) > 0) {
    # a[, 3:4, ] <- a[, 3:4, ] + carry[, 3:4, , ]
    # Make sure the order is the same fro children carrying on to parents
    mtrIDs <- lvlData$mtrID |> unique() |> sort()
    a[, 3:4, mtrIDs %in% carryOrder] <- a[,
      3:4,
      mtrIDs %in% carryOrder,
      drop = F
    ] +
      carry[, 3:4, order(carryOrder), drop = F]
  }

  #Update Score (col 5) for each pair
  a[, 5, ] <- (a[, 3, ] + a[, 4, ] - abs(a[, 3, ] - a[, 4, ])) / 2

  # Update col 3-4 with remainder
  if (dim(a)[3] == 1) {
    # If only one element in 3rd dim, slightly different operation needed
    a[, 3:4, ] <- a[, 3:4, , drop = F] -
      array(matrix(rep(a[, 5, ], 2), ncol = 2), dim = c(dim(a)[1], 2, 1))
  } else {
    a[, 3:4, ] <- a[, 3:4, ] -
      array(
        a[, 5, ][, rep(1:dim(a)[3], each = 2)],
        dim = c(dim(a)[1], 2, dim(a)[3])
      )
  }
  return(a)
}

zooScore_tree <- function(tree, auIDs) {
  # Not enough data to build a tree
  if (n_distinct(tree$auID) < 2) {
    return(NULL)
  }

  # Setup the run
  finalScore <- combn(auIDs, 2) |> t()
  m <- finalScore # This will prevent need for recalculation below
  finalScore <- cbind(finalScore, 0)
  colnames(finalScore) <- c("au1", "au2", "score")

  # Start at leaves
  curLvl <- max(tree$level)
  result <- NULL
  carryOrder <- NULL

  # Process the tree level by level for all author pairs
  while (curLvl > 0) {
    # Get data for all nodes at lowest current level
    lvlData <- tree |>
      filter(level == curLvl)

    currentNodes <- lvlData |>
      select(mtrID, parent) |>
      distinct()

    result <- nodeEval(
      lvlData = lvlData,
      allID = auIDs,
      carry = result,
      carryOrder = carryOrder,
      m = m
    )

    # If there are multiple nodes feeding into the same parent, merge them
    #  this cannot happen when you are at the root
    if (curLvl > 1) {
      result <- lapply(currentNodes$parent |> unique(), function(parent) {
        matches <- which(currentNodes$parent == parent)

        if (length(matches) == 1) {
          return(result[,, matches])
        }

        matches <- result[,, matches]

        cbind(
          matches[, 1:2, 1],
          Reduce(
            `+`,
            apply(
              matches,
              3,
              function(m) {
                m[, 3:5]
              },
              simplify = F
            )
          )
        )
      }) |>
        simplify2array()
    }

    carryOrder <- currentNodes$parent |> unique()

    # Add the level scores to the final score
    finalScore[, "score"] <- finalScore[, "score"] +
      rowSums(result[, 5, , drop = F]) * curLvl

    # Next level
    curLvl <- curLvl - 1
  }

  as.data.frame(finalScore)
}

zooScore <- function(papermeshtree) {
  roots <- setNames(unique(papermeshtree$root), unique(papermeshtree$root))
  auIDs <- sort(unique(papermeshtree$auID))

  # Get results for each tree (if multiple roots)
  map_df(
    roots,
    function(root) {
      zooScore_tree(tree = papermeshtree |> filter(root == {{ root }}), auIDs)
    },
    .id = "tree"
  )
}

treeFromID <- function(uids) {
  treenums <- tbl(conn, "meshTree") |>
    filter(uid %in% local(unique(uids))) |>
    pull(treenum)

  treenums <- missingTreeNums(treenums, includeOriginal = T, includeRoots = T)

  treenums <- tbl(conn, "meshTree") |>
    filter(treenum %in% local(treenums)) |>
    left_join(tbl(conn, "meshLink"), by = "uid") |>
    collect()
}

branchID <- function(parent, child) {
  if (length(parent) != length(child)) {
    stop("Error: 'parent' and 'child' vectors must be the same length.")
  }

  # Check the data type
  parent <- as.character(parent)
  child <- as.character(child)

  # Build adjacency list
  children <- split(child, parent)

  # Identify all nodes
  all_nodes <- unique(c(parent, child))

  # Find root (a node that is never a child)
  root <- setdiff(parent, child)
  if (length(root) != 1) {
    stop(
      "Tree must have exactly one root. Found: ",
      paste(root, collapse = ", ")
    )
  }

  # Initialize maps
  node_to_id <- setNames(all_nodes, all_nodes)
  node_values <- setNames(rep(0, length(all_nodes)), all_nodes)
  id_counter <- 0

  # Recursive DFS function
  dfs <- function(
    node,
    parent = NULL,
    value = 2^ceiling(log2(length(all_nodes)))
  ) {
    # Branch ID assignment
    if (!is.null(parent) && length(children[[parent]]) == 1) {
      node_to_id[[node]] <<- node_to_id[[parent]]
    } else {
      # node_to_id[[node]] <<- id_counter
      # id_counter <<- id_counter + 1
    }

    # Safe child lookup
    child_nodes <- if (!is.null(children[[node]])) {
      children[[node]]
    } else {
      character(0)
    }
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

meshTree <- function(papermesh, dbInfo) {
  tree <- treeFromID(papermesh$uid)
  tree <- tree |>
    left_join(
      tbl(conn, "meshTerm") |>
        filter(meshui %in% local(unique(tree$meshui))) |>
        group_by(meshui) |>
        filter(row_number() == 1) |>
        collect(),
      by = "meshui"
    )

  # meshRoots <- data.frame(
  #   uid = 1:16,
  #   treenum = c(LETTERS[1:14], "V", "Z"),
  #   meshterm = c(
  #     "Anatomy",
  #     "Organisms",
  #     "Diseases",
  #     "Chemicals and Drugs",
  #     "Analytical, Diagnostic and Therapeutic Techniques, and Equipment",
  #     "Psychiatry and Psychology",
  #     "Phenomena and Processes",
  #     "Disciplines and Occupations",
  #     "Anthropology, Education, Sociology, and Social Phenomena",
  #     "Technology, Industry, and Agriculture",
  #     "Humanities",
  #     "Information Science",
  #     "Named Groups",
  #     "Health Care",
  #     "Publication Characteristics",
  #     "Geographicals"
  #   )
  # )

  # tree <- bind_rows(meshRoots, tree)

  tree <- tree |>
    mutate(
      level = str_count(treenum, "\\.") + 2,
      level = as.integer(ifelse(str_length(treenum) == 1, 1, level)),
      link = str_remove(treenum, "\\.\\d+$"),
      link = case_when(
        link == treenum & str_length(link) == 1 ~ "root",
        link == treenum ~ str_extract(link, "^\\w"),
        TRUE ~ link
      )
    )

  tree <- tree |>
    left_join(
      tree |> select(link = treenum, parent = mtrID),
      by = "link"
    ) |>
    select(-link) |>
    mutate(parent = ifelse(is.na(parent), as.integer(0), parent))

  branchIDs <- branchID(
    parent = tree$parent,
    child = tree$mtrID
  )

  tree <- tree |>
    left_join(
      data.frame(
        mtrID = as.integer(names(branchIDs$branch_id)),
        branchID = as.integer(unname(branchIDs$branch_id)),
        value = unname(branchIDs$node_value)
      ),
      by = "mtrID"
    )

  tree <- tree |>
    group_by(branchID) |>
    mutate(link = parent[1]) |>
    ungroup() |>
    left_join(
      tree |> select(link = mtrID, parentBranchID = branchID),
      by = "link"
    ) |>
    select(-link)

  return(
    tree |>
      select(mtrID, parent, treenum, branchID, parentBranchID, everything())
  )
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

dbSetup(dbInfo = "data/PGG.db", checkSchema = T)
conn <- dbGetConn()
auIDs <- tbl(conn, "author") |>
  filter(authorOfInterest == 1) |>
  pull(auID)

papermesh <- paperMesh(auIDs)

meshtree <- meshTree(papermesh)

paperMeshTree <- function(papermesh, meshtree) {
  papermesh |>
    group_by(auID, meshui) |>
    summarise(nPapers = n(), .groups = "drop") |>
    full_join(
      meshtree |>
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
      nPapers = ifelse(is.na(nPapers), as.integer(0), nPapers),
    )
}

papermeshtree <- paperMeshTree(papermesh, meshtree)

# Add author names
au <- tbl(conn, "author") |>
  filter(auID %in% local(unique(papermeshtree$auID))) |>
  select(auID) |>
  left_join(
    tbl(conn, "authorName") |> filter(default == 1),
    by = "auID"
  ) |>
  collect() |>
  rowwise() |>
  mutate(name = paste(lastName, firstName, sep = ", ")) |>
  select(auID, name)

papermeshtree <- papermeshtree |>
  left_join(
    au |> select(auID, name),
    by = "auID"
  ) |>
  mutate(
    name = ifelse(nPapers == 0, "", name)
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
  if (length(root) != 1) {
    stop(
      "Tree must have exactly one root. Found: ",
      paste(root, collapse = ", ")
    )
  }
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


treemapData <- function(papermeshtree) {
  # Merge branches with only one child into a single node
  treemap <- papermeshtree |>
    arrange(treenum) |>
    mutate(
      parentBranchID = ifelse(
        is.na(parentBranchID),
        as.integer(0),
        parentBranchID
      )
    ) |>
    group_by(branchID) |>
    summarise(
      parentBranchID = min(parentBranchID),
      meshterm = paste(unique(meshterm), collapse = " -> "),
      value = sum(value),
      nPapers = sum(nPapers),
      authors = paste(
        sort(unique(sprintf("%s", name))),
        collapse = "\n"
      ),
      root = root[1],
      .groups = "drop"
    )

  treemap <- treemap |>
    mutate(
      parentBranchID = ifelse(is.na(parentBranchID), 0, parentBranchID),
      hasChildren = branchID %in% parentBranchID
    )

  meshSum <- nodeSum(
    parent = treemap$parentBranchID,
    child = treemap$branchID,
    value = treemap$nPapers
  ) |>
    transmute(branchID = as.integer(node), meshSum = value)

  treemap <- treemap |> left_join(meshSum, by = "branchID")

  bind_rows(
    data.frame(
      branchID = as.integer(0),
      parentBranchID = NA,
      meshterm = "MeSH Tree",
      value = 0,
      nPapers = 0,
      meshSum = NA
    ),
    treemap
  )
}

plotData <- treemapData(papermeshtree)

# Treemap Colour scale
mapColour <- function(vals, minCol = "#f2f3f5", maxCol = "#4682B4") {
  result <- rep("#FFFFFF", length(vals))
  nas <- is.na(vals)
  vals <- vals[!nas]
  vals <- log(vals + 1)
  val2col <- colorRamp(c(minCol, maxCol))
  vals_norm <- (vals - min(vals)) / (max(vals) - min(vals))
  rgb_matrix <- val2col(vals_norm)
  vals <- rgb(
    rgb_matrix[, 1],
    rgb_matrix[, 2],
    rgb_matrix[, 3],
    maxColorValue = 255
  )
  result[!nas] = vals
  result
}

# Render Treemap
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
  values = plotData$value,
  marker = list(colors = mapColour(plotData$meshSum)),
  textinfo = "text",
  hovertext = plotData$authors,
  hoverinfo = "text",
  maxdepth = 3,
)


# Zoo score by tree
# root <- unique(dummy$root)[1]
# zooscore <- lapply(unique(dummy$root), function(root) {
#   x <- dummy |> filter(root == {{ root }})
#   zooScore(
#     auID = x$auID,
#     nodeID = x$treenum,
#     parent = x$parent,
#     lvl = x$level,
#     n = x$nPapers
#   )
# })

# roots <- setNames(unique(papermeshtree$root), unique(papermeshtree$root))
# zooscore <- map_df(
#   roots,
#   function(root) {
#     x <- papermeshtree |> filter(root == {{ root }})
#     zooScore(
#       auID = x$auID,
#       nodeID = x$treenum,
#       parent = x$parent,
#       lvl = x$level,
#       n = x$nPapers
#     )
#   },
#   .id = "tree"
# )

zooscore <- zooScore(papermeshtree)


# Now summarise by author pair (adjust for tree depth in future?)
zooscore <- zooscore |>
  group_by(au1, au2) |>
  summarise(score = sum(score), .groups = "drop")

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
