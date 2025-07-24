#' Check if MeSH tree numbers are valid
#'
#' @param treenums vector of MeSH tree numbers
#' @param output "errorOnly" (Default) will only raise error, not output.
#' Alternatively "bool" will return TRUE / FALSE for no errors / errors respectively
#' "vector" will return a list of TRUE / FALSE with the check for each number
#'
#' @importFrom stringr str_detect
#'
#' @return Dependent on output argument
#' @export
#'
checkTreeNums <- function(treenums, output = "errorOnly") {
  check <- str_detect(treenums, r"(^[A-Z]\d{2}(\.\d{3}){0,}$)")

  if (output == "vector") {
    return(check)
  } else if (output == "bool") {
    return(all(check))
  } else if (!all(check)) {
    stop(
      "Invalid tree numbers: ",
      paste(treenums[!check], collapse = ", ")
    )
  }
}

#' Extract all missing Mesh tree numbers for leaf nodes to reach their root
#'
#' This will allow to reconstruct the MeSH tree later by filling in gaps from
#' leaves to the root
#'
#' @param treenums A vector of MeshTreeNumbers (e.g. N.06.850.290.200).
#' Use checkTreeNums() is you want to make sure the numbers are in valid format
#' @param includeOriginal Default = FALSE. If TRUE, the provided treenums will be
#' part of the result
#' @param includeRoots Default = FALSE. If TRUE, the provided root (category)
#' letter will be part of the result
#'
#' @importFrom stringr str_remove
#'
#' @return vector of treenums representing missing nodes between leaves and root
#'
#' @export
missingTreeNums <- function(treenums, includeOriginal = F, includeRoots = F) {
  treenums <- unique(treenums)

  currentNodes <- treenums
  allNodes <- currentNodes

  # Chop off the last node from the treenum to get parent, then repeat until root
  while (length(currentNodes) > 0) {
    nextUp <- str_remove(currentNodes, "\\.\\d+$")
    nextUp <- nextUp[nextUp != currentNodes]
    allNodes <- c(allNodes, nextUp)
    currentNodes <- nextUp
  }

  allNodes <- allNodes %>% unique()

  if (includeRoots) {
    allNodes <- c(unique(str_extract(allNodes, "^\\w")), allNodes)
  }

  if (!includeOriginal) {
    allNodes <- allNodes[!allNodes %in% treenums]
  }

  if (length(allNodes) == 0) {
    return(NULL)
  } else {
    allNodes
  }
}

#' Find and label linear branches (single chain of nodes)
#' These will be labelled with the same branchID, as they can be collapse
#'
#' Meanwhile, a treemapVal for each leaf node will be calculated such that
#' that when rendered as a treemap all cells should roughly be of the same size
#'
#' @param node Vector of node IDs
#' @param parent Vector of parent ID for each node above
#'
#' @return A list with two elements
#'  - branchID: The new branch ID for each node
#'  - treemapVal: The value used to scale a treemap and force all cells to
#'  become roughly the same size
#'
#' @export
branchID <- function(node, parent) {
  if (length(parent) != length(node)) {
    stop("Error: 'node' and 'parent' vectors must be of the same length.")
  }

  # Make sure node IDs are character used as keys
  parent <- as.character(parent)
  node <- as.character(node)

  # Build adjacency list
  children <- split(node, parent)

  # Identify all nodes
  all_nodes <- unique(c(parent, node))

  # Find root (a node that is never a child)
  root <- setdiff(parent, node)
  if (length(root) != 1) {
    stop(
      "Tree must have exactly one root. Found: ",
      paste(root, collapse = ", ")
    )
  }

  # Initialize sets
  branchIDs <- setNames(all_nodes, all_nodes)
  nodeValues <- setNames(rep(0, length(all_nodes)), all_nodes)

  # Recursive DFS function
  dfs <- function(
    node,
    parent = NULL,
    # Estimate a total value such that treemapVals do not become too small
    value = 2^ceiling(log2(length(all_nodes)))
  ) {
    # Branch ID assignment
    if (!is.null(parent) && length(children[[parent]]) == 1) {
      branchIDs[[node]] <<- branchIDs[[parent]]
    }

    # Child lookup
    child_nodes <- if (!is.null(children[[node]])) {
      children[[node]]
    } else {
      character(0)
    }
    num_children <- length(child_nodes)

    if (num_children > 0) {
      nodeValues[[node]] <<- 0
      split_value <- value / num_children
      for (child in child_nodes) {
        dfs(child, node, split_value)
      }
    } else {
      nodeValues[[node]] <<- value
    }
  }

  # Run the recursive algorithm
  dfs(root)

  return(list(branchID = branchIDs, treemapVal = nodeValues))
}

#' Build a MeSH Tree from a list of MeSH IDs
#' Missing nodes will be filled in and the tree will be built until the root
#'
#' @param uids Vector of Mesh uid to buidll the tree
#' @param roots (Optional) Vector of single letter representing the tree roots
#' to include. If not specified, all categories are returned
#' @param dbInfo (optional if dbSetup() has been run)
#'  Path to the ColabNet database or existing connection
#'
#' @import RSQLite
#' @import dplyr
#'
#' @return Dataframe with tree info (mtrID, uid, meshui, treenum)
#' @export
#'
dbTreeFromMesh <- function(uids, roots, dbInfo) {
  conn <- dbGetConn(dbInfo)
  treenums <- tbl(conn, "meshTree") |>
    filter(uid %in% local(unique(uids)))

  # Filter if roots are provided
  if (!missing(roots)) {
    roots <- toupper(roots)

    if (any(str_length(roots) > 1)) {
      stop("Tree root categories are a single letter")
    }

    treenums <- treenums |>
      filter(sql(sprintf("treenum GLOB '[%s]*'", paste(roots, collapse = ""))))
  }

  treenums <- treenums |> pull(treenum)

  treenums <- missingTreeNums(treenums, includeOriginal = T, includeRoots = T)

  tbl(conn, "meshTree") |>
    filter(treenum %in% local(treenums)) |>
    left_join(tbl(conn, "meshLink"), by = "uid") |>
    collect() |>
    select(mtrID, uid, meshui, treenum)
}

#' Get all of the MeSH terms from papers by a set of authors
#'
#' @param auIDs Vector auIDs
#' @param roots (Optional) Vector of single letter representing the tree roots
#' to include. If not specified, all categories are returned
#' @param dbInfo (optional if dbSetup() has been run)
#'  Path to the ColabNet database or existing connection
#'
#' @import RSQLite
#' @import dplyr
#'
#' @return Dataframe with paper Mesh terms for authors
#'
#' @export
dbPaperMesh <- function(auIDs, roots, dbInfo) {
  conn <- dbGetConn(dbInfo)

  # Get all MeshTerms for papers
  papermesh <- tbl(conn, "coauthor") |>
    filter(auID %in% auIDs) |>
    left_join(
      tbl(conn, "mesh_article"),
      by = "arID"
    ) |>
    filter(!is.na(maID)) |>
    left_join(
      tbl(conn, "meshLink"),
      by = "meshui"
    )

  # Filter the tree by only considering selected categories (roots)
  if (!missing(roots)) {
    roots <- toupper(roots)

    if (any(str_length(roots) > 1)) {
      stop("Tree root categories are a single letter")
    }

    papermesh <- papermesh |>
      left_join(
        tbl(conn, "meshTree"),
        by = "uid"
      ) |>
      filter(sql(sprintf("treenum GLOB '[%s]*'", paste(roots, collapse = ""))))
  }

  papermesh |> collect()
}

#' Build a full MeSH tree from a papermesh data frame
#'
#' @param papermesh Data frame generated by dbPaperMesh()
#' @param roots (Optional) Vector of single letter representing the tree roots
#' to include. If not specified, all categories are returned
#' @param dbInfo (optional if dbSetup() has been run)
#'  Path to the ColabNet database or existing connection
#'
#' @import RSQLite
#' @import dplyr
#'
#' @return Dataframe containing the MeSH tree based on papermesh info
#'  This will also contain parentIDs, branchIDs and parentBranchIDs
#'  the tree root name and treemapVals
#'
#' @export
dbMeshTree <- function(papermesh, roots, dbInfo) {
  # Check if papermesh is empty
  if (nrow(papermesh) == 0) {
    return(data.frame(
      mtrID = integer(),
      parent = integer(),
      treenum = character(),
      branchID = integer(),
      parentBranchID = integer(),
      root = character(),
      uid = integer(),
      meshui = character(),
      mteID = integer(),
      meshterm = character(),
      level = integer(),
      treemapVal = numeric()
    ))
  }

  conn <- dbGetConn(dbInfo)

  # Build the basic MeSH tree
  tree <- dbTreeFromMesh(papermesh$uid, roots = roots)

  # Add the MeSH term (actual description)
  tree <- tree |>
    left_join(
      tbl(conn, "meshTerm") |>
        filter(meshui %in% local(unique(tree$meshui))) |>
        group_by(meshui) |>
        # There are many synonyms, pick the first
        filter(row_number() == 1) |>
        collect(),
      by = "meshui"
    )

  # Add a column with the parent ID for each mtrID
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

  # Add branch IDs and treemapVals
  branchIDs <- branchID(
    node = tree$mtrID,
    parent = tree$parent
  )

  if (!missing(roots) && length(roots) == 1) {
    branchIDs[[1]][2] <- names(branchIDs[[1]][2])
  }

  tree <- tree |>
    left_join(
      data.frame(
        mtrID = as.integer(names(branchIDs$branchID)),
        branchID = as.integer(unname(branchIDs$branchID)),
        treemapVal = unname(branchIDs$treemapVal)
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
    mutate(root = str_extract(treenum, "^[^\\.\\s]+")) |>
    select(-link)

  return(
    tree |>
      select(
        mtrID,
        parent,
        treenum,
        branchID,
        parentBranchID,
        root,
        everything()
      )
  )
}

#' Expand a MeSH tree with author level info from papermesh
#'
#' @param papermesh Data frame generated by dbPaperMesh()
#' @param meshtree Data frame generated by dbMeshTree()
#'
#' @import dplyr
#'
#' @return Dataframe containing the MeSH tree expanded with author details
#'
#' @export
paperMeshTree <- function(papermesh, meshtree) {
  papermesh |>
    group_by(auID, meshui) |>
    summarise(nPapers = n(), .groups = "drop") |>
    full_join(
      meshtree |>
        mutate(root = str_extract(treenum, "^.")),
      by = "meshui",
      relationship = "many-to-many"
    ) |>
    mutate(
      auID = ifelse(is.na(auID), 0, auID),
      nPapers = ifelse(is.na(nPapers), as.integer(0), nPapers),
    )
}
