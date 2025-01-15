#' Simplify special characters in text for easier comparisons
#'
#' @param text
#'
#' @importFrom stringi stri_trans_nfd
#' @importFrom stringr str_replace_all
#'
#' @return simplified text
#' @export
#'
simpleText <- function(text) {
  str_replace_all(stri_trans_nfd(tolower(text)), "\\p{Mn}", "")
}

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
#'
#' @importFrom stringr str_remove
#'
#' @return vector of treenums representing missing nodes between leaves and root
#'
#' @export
missingTreeNums <- function(treenums) {
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
  allNodes <- allNodes[!allNodes %in% treenums]

  if (length(allNodes) == 0) {
    return(NULL)
  } else {
    allNodes
  }
}

#' Timestamp
#'
#' @returns Time formatted as %Y-%m-%d %H:%M:%S
timeStamp <- function() {
  format(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
}

#' Extract the MeSH tree for a specific author
#'
#' @param auID Author ID (stored in the author table of the DB)
#' @param dbPath Path to the ColabNet database. Can be left blank if setup with dbConn()
#'
#' @import dplyr
#' @import stringr
#'
#' @return A list with two data frames:
#'  - auTree: The full meshtree for the author (including intermediate nodes)
#'  - arMesh: The set of MeSH terms actually referenced in the author's articles
#' @export
#'
authorMeshTree <- function(auID, dbPath) {
  conn <- dbGetConn(dbPath)

  # Get all MeSH tree entries for the author
  auMeshui <- tbl(conn, "author_affiliation") |>
    filter(auID == local(auID)) |>
    select(arID, auID) |>
    distinct() |>
    left_join(
      tbl(conn, "mesh_article"),
      by = "arID"
    ) |>
    left_join(
      tbl(conn, "meshLink"),
      by = "meshui"
    ) |>
    left_join(
      tbl(conn, "meshTree"),
      by = "uid"
    ) |>
    # Ignore papers with no MeSH terms
    filter(!is.na(meshui)) |>
    collect()

  # Use the meshTree to get all intermediate meshuis
  auTree <- auMeshui |>
    select(mtrID, uid, treenum) |>
    distinct() |>
    mutate(hasArticle = T)

  # Cut off one level of the treenum and find its parents
  nextNums <- ifelse(str_detect(auTree$treenum, "\\."),
    str_remove(auTree$treenum, "\\.\\d+$"), NA
  ) |> unique(na.rm = T)

  while (length(nextNums) > 0) {
    new <- tbl(conn, "meshTree") |>
      filter(treenum %in% local(nextNums)) |>
      collect()
    newTreenums <- setdiff(new$treenum, auTree$treenum)
    auTree <- bind_rows(auTree, new |> filter(treenum %in% newTreenums)) |> distinct()
    nextNums <- ifelse(str_detect(newTreenums, "\\."),
      str_remove(newTreenums, "\\.\\d+$"), NA
    ) |> unique(na.rm = T)
  }

  auTree <- auTree |>
    mutate(
      hasArticle = ifelse(is.na(hasArticle), F, T),
      auID = as.integer({{ auID }}), .before = 1
    ) |>
    arrange(treenum)

  dbDisconnect(conn)

  return(list(auTree = auTree, arMesh = auMeshui))
}

#' Compare two author MeSH trees and find overlapping areas
#'
#' @param auID1 Author ID for author 1 (stored in the author table of the DB)
#' @param auID1 Author ID for author 2 (stored in the author table of the DB)
#' @param pruneDuplicates Prune parts of the MeSH tree that contain duplicated terms
#' @param dbPath Path to the ColabNet database. Can be left blank if setup with dbConn()
#'
#' @import dplyr
#' @import stringr
#'
#' @return A data frame with the MeSH tree information of both authors and their overlap
#' @export
#'
diffTree <- function(auID1, auID2, pruneDuplicates = F, dbPath) {
  # E05.242
  amt1 <- authorMeshTree(auID1)
  amt2 <- authorMeshTree(auID2)
  conn <- dbGetConn(dbPath)

  # Get for each treenum whether there is overlap or not
  diffTree <- bind_rows(amt1$auTree, amt2$auTree) |>
    group_by(treenum) |>
    mutate(auID = ifelse(n() == 1, auID, 0) |> as.integer()) |>
    filter(hasArticle == max(hasArticle)) |>
    ungroup() |>
    distinct() |>
    mutate(level = as.integer(str_count(treenum, "\\.") + 1))

  # Add the MeSH term (description)
  meshTerm <- tbl(conn, "meshLink") |>
    filter(uid %in% local(diffTree$uid)) |>
    left_join(tbl(conn, "meshTerm"), by = "meshui") |>
    distinct() |>
    # If there are multiple term descriptions, only keep one
    group_by(uid) |>
    filter(mteID == min(mteID, na.rm = T)) |>
    ungroup() |>
    collect()

  dbDisconnect(conn)

  diffTree <- diffTree |>
    left_join(meshTerm |> select(uid, meshterm), by = "uid") |>
    filter(!is.na(meshterm))

  # Get the parent for each treenum ("" =  root)
  diffTree <- diffTree |> mutate(
    parent = str_remove(diffTree$treenum, "\\.\\d+$"),
    parent = ifelse(diffTree$treenum == parent, "", parent)
  )

  # Add the number of children for each treenum and sort the tree by treenum (important for next step)
  diffTree <- diffTree |>
    left_join(
      diffTree |> group_by(treenum = parent) |> summarise(children = n(), .groups = "drop"),
      by = "treenum"
    ) |>
    mutate(children = as.integer(ifelse(is.na(children), 0, children))) |>
    arrange(treenum)

  # Check if treenums can be merged if they don't branch off
  b <- 1
  bID <- c(b, rep(NA, nrow(diffTree) - 1))
  for (i in 2:nrow(diffTree)) {
    if (diffTree$parent[i] != diffTree$treenum[i - 1] | diffTree$treenum[i] == "" |
      diffTree$children[i] > 1 | diffTree$children[i - 1] > 1 | diffTree$auID[i] != diffTree$auID[i - 1]) {
      b <- b + 1
    }
    bID[i] <- b
  }

  diffTree <- diffTree |>
    mutate(
      branchID = as.integer({{ bID }})
    ) |>
    select(treenum, branchID, children, parent, meshterm, everything())

  # Get the parent branchID
  diffTree <- diffTree |> left_join(
    diffTree |> select(parent = treenum, parentBranchID = branchID) |> distinct(),
    by = "parent"
  )

  # Stop here if no need to prune duplicate branches
  if (!pruneDuplicates) {
    return(diffTree)
  }

  ## REMOVE DUPLICATE BRANCHES

  # Find MeSH terms that are duplicated
  diffTree <- diffTree |>
    group_by(meshterm) |>
    mutate(duplicated = n() > 1) |>
    ungroup()

  # Group are created as follows:
  # - Start with duplicates at the highest level (closest to tree root)
  # - If a child is also a duplicate, it's part of the same group, if not the group ends
  getDup <- diffTree |>
    filter(duplicated) |>
    mutate(nDup = 1, groupID = mtrID) |>
    select(meshterm, level, treenum, parent, nDup, groupID, mtrID)

  # Keep going from top to bottom until all duplicate groups have been defined
  curLvl <- sort(unique(getDup$level))[2]
  for (curLvl in sort(unique(getDup$level))) {
    nextLvl <- getDup |> filter(level == {{ curLvl }})

    if (nrow(nextLvl) == 0) {
      next
    }

    nextLvl <- nextLvl |>
      mutate(newGroupID = ifelse(groupID == 0, mtrID, groupID)) |>
      select(parent = treenum, newGroupID)

    getDup <- getDup |>
      left_join(nextLvl, by = "parent") |>
      mutate(groupID = ifelse(is.na(newGroupID), groupID, newGroupID)) |>
      select(-newGroupID)
  }

  # Add all terms that have a duplicate parent but themselves are not duplicated to the group
  duplicates <- getDup |>
    pull(treenum) |>
    unique()
  duplicates <- diffTree |>
    filter(parent %in% duplicates, !treenum %in% duplicates) |>
    pull(parent)
  getDup$uniqueChild <- getDup$treenum %in% duplicates

  # Now remove redundant duplications according to the following rules
  # - Every duplicated group that has a unique child is kept (non-redundant duplication)
  # - For duplpicated groups with no unique children, keep the group with the largest size
  #   (this will effectively prune small duplications in various places in favour of a large group
  #    containing multiple duplications)
  getDup <- getDup |>
    group_by(groupID) |>
    mutate(
      uniqueChild = any(uniqueChild),
      groupSize = n()
    ) |>
    group_by(meshterm) |>
    arrange(desc(groupSize), treenum) |>
    filter((!any(uniqueChild) & treenum == treenum[1]) | uniqueChild) |>
    ungroup()

  # Now create the new difftree with (redundant) duplicates removed
  diffTree <- bind_rows(
    diffTree |> filter(!duplicated),
    diffTree |> filter(treenum %in% getDup$treenum)
  )

  return(diffTree)
}

diffTree2 <- function(auIDs, pruneDuplicates = F, dbPath) {
  # auIDs = c(1,2,31)
  amt <- map_df(auIDs, authorMeshTree)
  conn <- dbGetConn(dbPath)

  # Get for each treenum whether there is overlap or not
  diffTree <- bind_rows(amt1$auTree, amt2$auTree) |>
    group_by(treenum) |>
    mutate(auID = ifelse(n() == 1, auID, 0) |> as.integer()) |>
    filter(hasArticle == max(hasArticle)) |>
    ungroup() |>
    distinct() |>
    mutate(level = as.integer(str_count(treenum, "\\.") + 1))

  # Add the MeSH term (description)
  meshTerm <- tbl(conn, "meshLink") |>
    filter(uid %in% local(diffTree$uid)) |>
    left_join(tbl(conn, "meshTerm"), by = "meshui") |>
    distinct() |>
    # If there are multiple term descriptions, only keep one
    group_by(uid) |>
    filter(mteID == min(mteID, na.rm = T)) |>
    ungroup() |>
    collect()

  dbDisconnect(conn)

  diffTree <- diffTree |>
    left_join(meshTerm |> select(uid, meshterm), by = "uid") |>
    filter(!is.na(meshterm))

  # Get the parent for each treenum ("" =  root)
  diffTree <- diffTree |> mutate(
    parent = str_remove(diffTree$treenum, "\\.\\d+$"),
    parent = ifelse(diffTree$treenum == parent, "", parent)
  )

  # Add the number of children for each treenum and sort the tree by treenum (important for next step)
  diffTree <- diffTree |>
    left_join(
      diffTree |> group_by(treenum = parent) |> summarise(children = n(), .groups = "drop"),
      by = "treenum"
    ) |>
    mutate(children = as.integer(ifelse(is.na(children), 0, children))) |>
    arrange(treenum)

  # Check if treenums can be merged if they don't branch off
  b <- 1
  bID <- c(b, rep(NA, nrow(diffTree) - 1))
  for (i in 2:nrow(diffTree)) {
    if (diffTree$parent[i] != diffTree$treenum[i - 1] | diffTree$treenum[i] == "" |
      diffTree$children[i] > 1 | diffTree$children[i - 1] > 1 | diffTree$auID[i] != diffTree$auID[i - 1]) {
      b <- b + 1
    }
    bID[i] <- b
  }

  diffTree <- diffTree |>
    mutate(
      branchID = as.integer({{ bID }})
    ) |>
    select(treenum, branchID, children, parent, meshterm, everything())

  # Get the parent branchID
  diffTree <- diffTree |> left_join(
    diffTree |> select(parent = treenum, parentBranchID = branchID) |> distinct(),
    by = "parent"
  )

  # Stop here if no need to prune duplicate branches
  if (!pruneDuplicates) {
    return(diffTree)
  }

  ## REMOVE DUPLICATE BRANCHES

  # Find MeSH terms that are duplicated
  diffTree <- diffTree |>
    group_by(meshterm) |>
    mutate(duplicated = n() > 1) |>
    ungroup()

  # Group are created as follows:
  # - Start with duplicates at the highest level (closest to tree root)
  # - If a child is also a duplicate, it's part of the same group, if not the group ends
  getDup <- diffTree |>
    filter(duplicated) |>
    mutate(nDup = 1, groupID = mtrID) |>
    select(meshterm, level, treenum, parent, nDup, groupID, mtrID)

  # Keep going from top to bottom until all duplicate groups have been defined
  curLvl <- sort(unique(getDup$level))[2]
  for (curLvl in sort(unique(getDup$level))) {
    nextLvl <- getDup |> filter(level == {{ curLvl }})

    if (nrow(nextLvl) == 0) {
      next
    }

    nextLvl <- nextLvl |>
      mutate(newGroupID = ifelse(groupID == 0, mtrID, groupID)) |>
      select(parent = treenum, newGroupID)

    getDup <- getDup |>
      left_join(nextLvl, by = "parent") |>
      mutate(groupID = ifelse(is.na(newGroupID), groupID, newGroupID)) |>
      select(-newGroupID)
  }

  # Add all terms that have a duplicate parent but themselves are not duplicated to the group
  duplicates <- getDup |>
    pull(treenum) |>
    unique()
  duplicates <- diffTree |>
    filter(parent %in% duplicates, !treenum %in% duplicates) |>
    pull(parent)
  getDup$uniqueChild <- getDup$treenum %in% duplicates

  # Now remove redundant duplications according to the following rules
  # - Every duplicated group that has a unique child is kept (non-redundant duplication)
  # - For duplpicated groups with no unique children, keep the group with the largest size
  #   (this will effectively prune small duplications in various places in favour of a large group
  #    containing multiple duplications)
  getDup <- getDup |>
    group_by(groupID) |>
    mutate(
      uniqueChild = any(uniqueChild),
      groupSize = n()
    ) |>
    group_by(meshterm) |>
    arrange(desc(groupSize), treenum) |>
    filter((!any(uniqueChild) & treenum == treenum[1]) | uniqueChild) |>
    ungroup()

  # Now create the new difftree with (redundant) duplicates removed
  diffTree <- bind_rows(
    diffTree |> filter(!duplicated),
    diffTree |> filter(treenum %in% getDup$treenum)
  )

  return(diffTree)
}

#' Check whether to use black or white text on a colour backgound
#'
#' @param colours A vector of colours
#'
#' @return A vector of "black" or "white" for each colour
#' @export
textBW <- function(colours) {
  colours <- col2rgb(colours)

  # Calculate the luminance (0 - 255)
  lum <- apply(colours, 2, function(x) {
    0.2126 * x[1] + 0.7152 * x[2] + 0.0722 * x[3]
  })

  return(ifelse(lum > 127.5, "black", "white"))
}

#' Compare two author MeSH trees and find overlapping areas
#'
#' @param diffTree a diffTree dataframe returned by diffTree()
#' @param colours A list of 3 colours for colouring shared,
#'  author 1 unique and author 2 unique MeSH terms
#'
#' @import dplyr
#' @import stringr
#' @importFrom plotly plot_ly
#'
#' @return A plotly Treemap
#' @export
#'
plotDiffTree <- function(diffTree, colours = c("#69BE28", "#3DB7E4", "#FF8849")) {
  if (length(colours) != 3) {
    stop("A list of three colours needs to be provided")
  }

  # Highlight mesh Terms that are shared between authors
  plotData <- diffTree |> mutate(
    meshterm = ifelse(auID == 0, sprintf("<b>%s</b>", meshterm), meshterm)
  )

  # If the same MeSH term is used in different branches it will cause an error in the treeplot
  # Rename duplicates to make them unique
  plotData <- plotData |>
    group_by(meshterm) |>
    mutate(dupID = cur_group_id(), duplicate = n() > 1) |>
    ungroup()

  plotData <- bind_rows(
    plotData |> filter(!duplicate),
    plotData |> filter(duplicate) |> group_by(dupID) |>
      mutate(meshterm = sprintf("%s(%i)", meshterm, 1:n())) |> ungroup()
  )

  # Add the parent MeSH term
  plotData <- plotData |>
    left_join(
      plotData |> select(parent = treenum, parentMeshterm = meshterm),
      by = "parent"
    ) |>
    mutate(parentMeshterm = ifelse(is.na(parentMeshterm), "", parentMeshterm))
  plotData$parentMeshterm[!plotData$parentMeshterm %in% plotData$meshterm]

  # Merge branches with only one child into a single node
  plotData <- plotData |>
    group_by(branchID, auID) |>
    summarise(
      parentBranchID = min(parentBranchID),
      treenum = paste(treenum, collapse = " -> <br>"),
      meshterm = paste(meshterm, collapse = " -> <br>"),
      .groups = "drop"
    )

  # Get the new parent info for the collapsed data and the root
  plotData <- plotData |>
    left_join(
      plotData |> select(parent = treenum, parentBranchID = branchID, parentMeshterm = meshterm) |> distinct(),
      by = "parentBranchID"
    ) |>
    mutate(
      parentMeshterm = ifelse(is.na(parentMeshterm), "MeSH Tree", parentMeshterm),
      root = str_extract(plotData$treenum, "^[^\\.\\s]+")
    )

  # The first colour is the one where authors share a MeSH terms the other two their unique ones
  colours <- plotData |>
    select(auID) |>
    distinct() |>
    arrange(auID) |>
    mutate(colour = colours[1:n()])

  plotData <- plotData |> left_join(colours, by = "auID")

  # Meshterm plot (for app)
  fig <- plot_ly(
    type = "treemap",
    labels = plotData$meshterm,
    parents = plotData$parentMeshterm,
    marker = list(colors = plotData$colour),
    textfont = list(
      color = textBW(plotData$colour)
    ),
    maxdepth = -1
  )

  # htmlwidgets::saveWidget(fig, "D:/Desktop/PJ-Lorenzo.html")
  return(fig)
}
