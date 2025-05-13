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
  str_replace_all(stri_trans_nfd(tolower(text)), "\\p{Mn}", "") |> str_trim()
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
#' @param dbInfo Path to the ColabNet database. Can be left blank if setup with dbConn()
#'
#' @import dplyr
#' @import stringr
#'
#' @return A list with two data frames:
#'  - auTree: The full meshtree for the author (including intermediate nodes)
#'  - arMesh: The set of MeSH terms actually referenced in the author's articles
#' @export
#'
authorMeshTree <- function(auID, dbInfo) {
  conn <- dbGetConn(dbInfo)

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
  nextNums <- ifelse(
    str_detect(auTree$treenum, "\\."),
    str_remove(auTree$treenum, "\\.\\d+$"),
    NA
  ) |>
    unique(na.rm = T)

  while (length(nextNums) > 0) {
    new <- tbl(conn, "meshTree") |>
      filter(treenum %in% local(nextNums)) |>
      collect()
    newTreenums <- setdiff(new$treenum, auTree$treenum)
    auTree <- bind_rows(auTree, new |> filter(treenum %in% newTreenums)) |>
      distinct()
    nextNums <- ifelse(
      str_detect(newTreenums, "\\."),
      str_remove(newTreenums, "\\.\\d+$"),
      NA
    ) |>
      unique(na.rm = T)
  }

  auTree <- auTree |>
    mutate(
      hasArticle = ifelse(is.na(hasArticle), F, T),
      auID = as.integer({{ auID }}),
      .before = 1
    ) |>
    arrange(treenum)

  dbDisconnect(conn)

  return(list(auTree = auTree, arMesh = auMeshui))
}

#' Compare two author MeSH trees and find overlapping areas
#'
#' @param auIDs Vector author IDs (stored in the author table of the DB)
#' @param pruneDuplicates (default = TRUE) Prune parts of the MeSH tree that contain duplicated terms
#' @param dbInfo Path to the ColabNet database. Can be left blank if setup with dbConn()
#'
#' @import dplyr
#' @import stringr
#'
#' @return A data frame with the MeSH tree information of both authors and their overlap
#' @export
#'
diffTree <- function(auIDs, pruneDuplicates = T, dbInfo) {
  amt <- lapply(auIDs, authorMeshTree)
  conn <- dbGetConn(dbInfo)

  # Get for each treenum whether there is overlap or not
  difftree <- bind_rows(lapply(amt, "[[", c("auTree"))) |>
    group_by(treenum) |>
    mutate(
      nAuth = n_distinct(auID),
      auIDs = auID |> unique() |> sort() |> paste(collapse = ",")
    ) |>
    select(-auID) |>
    filter(hasArticle == max(hasArticle)) |>
    ungroup() |>
    distinct() |>
    mutate(level = as.integer(str_count(treenum, "\\.") + 1))

  # Add the MeSH term (description)
  meshTerm <- tbl(conn, "meshLink") |>
    filter(uid %in% local(difftree$uid)) |>
    left_join(tbl(conn, "meshTerm"), by = "meshui") |>
    distinct() |>
    # If there are multiple term descriptions, only keep one
    group_by(uid) |>
    filter(mteID == min(mteID, na.rm = T)) |>
    ungroup() |>
    collect()

  dbDisconnect(conn)

  difftree <- difftree |>
    left_join(meshTerm |> select(uid, meshterm), by = "uid") |>
    filter(!is.na(meshterm))

  # Get the parent for each treenum ("" =  root)
  difftree <- difftree |>
    mutate(
      parent = str_remove(difftree$treenum, "\\.\\d+$"),
      parent = ifelse(difftree$treenum == parent, "", parent)
    )

  # Add the number of children for each treenum and sort the tree by treenum (important for next step)
  difftree <- difftree |>
    left_join(
      difftree |>
        group_by(treenum = parent) |>
        summarise(children = n(), .groups = "drop"),
      by = "treenum"
    ) |>
    mutate(children = as.integer(ifelse(is.na(children), 0, children))) |>
    arrange(treenum)

  # Check if treenums can be merged if they don't branch off
  b <- 1
  bID <- c(b, rep(NA, nrow(difftree) - 1))
  for (i in 2:nrow(difftree)) {
    if (
      difftree$parent[i] != difftree$treenum[i - 1] |
        difftree$treenum[i] == "" |
        difftree$children[i] > 1 |
        difftree$children[i - 1] > 1 |
        difftree$auIDs[i] != difftree$auIDs[i - 1]
    ) {
      b <- b + 1
    }
    bID[i] <- b
  }

  difftree <- difftree |>
    mutate(
      branchID = as.integer({{ bID }})
    ) |>
    select(treenum, branchID, children, parent, meshterm, everything())

  # Get the parent branchID
  difftree <- difftree |>
    left_join(
      difftree |>
        select(parent = treenum, parentBranchID = branchID) |>
        distinct(),
      by = "parent"
    )

  # Stop here if no need to prune duplicate branches
  if (!pruneDuplicates) {
    return(difftree)
  }

  ## REMOVE DUPLICATE BRANCHES

  # Step 1 - Find duplicated meshterms in different (parts of) tree
  difftree <- difftree |>
    group_by(uid) |>
    mutate(duplicated = n() > 1, nDup = n() - 1) |>
    ungroup()

  # Step 2 - Start at bottom and work way up the tree. For each treenum add:
  # - number of unique children
  # - number of duplicated children

  difftree <- difftree |> mutate(uniqueChildren = 0, dupChildren = 0)

  for (level in sort(unique(difftree$level), decreasing = T)) {
    # THe number of unique / dup children is the previous plus current
    currentNums <- difftree |>
      filter(level == {{ level }}) |>
      select(treenum, duplicated, parent, uniqueChildren, dupChildren) |>
      mutate(
        addUnique = uniqueChildren + !duplicated,
        addDup = dupChildren + duplicated,
      ) |>
      select(treenum = parent, addUnique, addDup) |>
      filter(treenum != "") |>
      group_by(treenum) |>
      summarise(
        addUnique = sum(addUnique),
        addDup = sum(addDup),
        .groups = "drop"
      )

    difftree <- difftree |>
      left_join(currentNums, by = "treenum") |>
      mutate(
        uniqueChildren = ifelse(
          is.na(addUnique),
          uniqueChildren,
          uniqueChildren + addUnique
        ),
        dupChildren = ifelse(is.na(addDup), dupChildren, dupChildren + addDup)
      ) |>
      select(-addUnique, -addDup)
  }

  # Step 3 - Pruning
  # The highest level that is duplicated and has all duplicate children is removed
  # first
  remainingDup <- data.frame(uid = c(), remaining = c())
  pruned <- c()

  toPrune <- difftree |>
    filter(!treenum %in% pruned) |>
    filter(duplicated, uniqueChildren == 0) |>
    filter(dupChildren == max(dupChildren)) |>
    filter(level == min(level))

  while (nrow(toPrune) > 0) {
    for (i in 1:nrow(toPrune)) {
      # Get the node of interest and all children (all should be duplicated)
      x <- difftree |>
        filter(str_detect(treenum, paste(toPrune[i, ]$treenum, collapse = "|")))

      # Check how many duplicates are left in the original dataset (don't prune last one)
      remainingDup <- bind_rows(
        remainingDup,
        x |>
          select(uid, remaining = nDup) |>
          group_by(uid) |>
          slice(1) |>
          ungroup() |>
          filter(!uid %in% remainingDup$uid)
      )

      toRemove <- x |>
        select(treenum, uid) |>
        left_join(remainingDup, by = "uid") |>
        filter(remaining > 0)

      if (nrow(toRemove) == 0) {
        next
      }

      # Check for rare case where child of to be removed has no remaining duplicates
      toKeep <- x |>
        filter(str_detect(treenum, paste(toRemove$treenum, collapse = "|")))
      if (nrow(toKeep) > nrow(toRemove)) {
        toRemove <- toRemove |> filter(!treenum %in% toKeep$treenum)

        if (nrow(toRemove) == 0) {
          next
        }
      }

      x <- toRemove

      remainingDup <- remainingDup |>
        left_join(
          x |> group_by(uid) |> summarise(n = n(), .groups = "drop"),
          by = "uid"
        ) |>
        mutate(remaining = ifelse(is.na(n), remaining, remaining - n)) |>
        select(-n)

      toRemove <- x$treenum
      # Rare case where duplicates are nested in the same part of the tree
      # and would all be pruned removing them completely (keep at least one)
      if (any(remainingDup$remaining < 0)) {
        toFix <- remainingDup$uid[remainingDup$remaining < 0]
        for (y in toFix) {
          # Select one to keep (least nested)
          toKeep <- x |>
            filter(uid == y) |>
            filter(nchar(treenum) == min(nchar(treenum))) |>
            slice(1)
          toKeep <- c(missingTreeNums(toKeep$treenum), toKeep$treenum)
          toRemove <- setdiff(toRemove, toKeep)
        }
        # Manually set the remaining to 0 to not trigger the same one next round
        remainingDup$remaining[remainingDup$remaining < 0] = 0
      }

      difftree <- difftree |> filter(!treenum %in% toRemove)
    }

    toPrune <- difftree |>
      filter(!treenum %in% pruned) |>
      filter(duplicated, uniqueChildren == 0) |>
      filter(dupChildren == max(dupChildren)) |>
      filter(level == min(level))

    pruned <- c(pruned, toPrune$treenum)
  }

  return(difftree)
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
#' @param difftree a diffTree dataframe returned by diffTree()
#' @param colours (optional) A list of 3 colours
#' - In case of comparing two authors, the first two colours represent the individual author's
#'  MeSH terms and the last colour shared terms
#' - In case of 2+, the first colour represents a term only belonging to a single author,
#'  the next two colours form a scale between which the number of authors per MeSH term will
#'  be interpolated. Min number of authors is colour 2, max is colour 3
#' @param dbInfo Path to the ColabNet database. Can be left blank if setup with dbConn()
#'
#' @import dplyr
#' @import stringr
#' @importFrom plotly plot_ly
#'
#' @return A dataset that can be used to create a (plotly) Treemap
#' @export
#'
plotDiffTree <- function(difftree, colours, dbInfo) {
  if (!missing(colours) && length(colours) != 3) {
    stop("A list of three colours needs to be provided")
  }

  # Highlight mesh Terms that are shared between authors
  plotData <- difftree |>
    mutate(
      meshterm = ifelse(
        str_detect(auIDs, ","),
        sprintf("<b>%s</b>", meshterm),
        meshterm
      )
    )

  # If the same MeSH term is used in different branches it will cause an error in the treeplot
  # Rename duplicates to make them unique
  plotData <- plotData |>
    group_by(meshterm) |>
    mutate(dupID = cur_group_id(), duplicate = n() > 1) |>
    ungroup()

  plotData <- bind_rows(
    plotData |> filter(!duplicate),
    plotData |>
      filter(duplicate) |>
      group_by(dupID) |>
      mutate(meshterm = sprintf("%s(%i)", meshterm, 1:n())) |>
      ungroup()
  )

  # Add the parent MeSH term
  plotData <- plotData |>
    left_join(
      plotData |> select(parent = treenum, parentMeshterm = meshterm),
      by = "parent"
    ) |>
    mutate(parentMeshterm = ifelse(is.na(parentMeshterm), "", parentMeshterm))

  # Merge branches with only one child into a single node
  plotData <- plotData |>
    group_by(branchID, auIDs, nAuth) |>
    summarise(
      parentBranchID = min(parentBranchID),
      leafNodeTreenum = treenum[n()],
      treenum = paste(treenum, collapse = " -> <br>"),
      meshterm = paste(meshterm, collapse = " -> <br>"),
      .groups = "drop"
    )

  # Get the new parent info for the collapsed data and the root
  plotData <- plotData |>
    left_join(
      plotData |>
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
      root = str_extract(plotData$treenum, "^[^\\.\\s]+")
    )

  # The first colour is the one where authors share a MeSH terms the other two their unique ones
  fillCol <- plotData |>
    select(nAuth) |>
    filter(nAuth > 1) |>
    distinct()

  # Set default colour scheme if not provided
  if (missing(colours)) {
    if (max(c(1, fillCol$nAuth)) < 3) {
      colours <- c("#3DB7E4", "#FF8849", "#69BE28")
    } else {
      colours <- c("#3DB7E4", "#b0df8c", "#408d06")
    }
  }

  # Set the colour scheme based on comparing 2 or comparing 2+
  if (max(c(1, fillCol$nAuth)) < 3) {
    fillCol <- plotData |>
      select(auIDs, nAuth) |>
      distinct() |>
      arrange(nAuth) |>
      mutate(colour = colours[1:n()]) |>
      select(-nAuth)
    plotData <- plotData |> left_join(fillCol, by = "auIDs")
  } else {
    fillCol <- fillCol |>
      arrange(nAuth) |>
      mutate(colour = colorRampPalette(colours[2:3])(n()))
    fillCol <- bind_rows(list(nAuth = 1, colour = colours[1]), fillCol)
    plotData <- plotData |> left_join(fillCol, by = "nAuth")
  }

  # Add the auhtor names to the hover text
  auInfo <- plotData$auIDs |>
    unique() |>
    map_df(
      function(x) {
        data.frame(auID = str_split(x, ",")[[1]])
      },
      .id = "gID"
    ) |>
    mutate(auID = as.integer(auID))

  # Get the author names to display on hover
  conn <- dbGetConn(dbInfo)
  auNames <- tbl(conn, "authorName") |>
    filter(auID %in% local(unique(auInfo$auID))) |>
    group_by(auID) |>
    filter(row_number() == 1) |>
    ungroup() |>
    collect() |>
    mutate(
      lastName = ifelse(is.na(lastName), collectiveName, lastName),
      firstName = ifelse(is.na(firstName), "", firstName),
      name = paste(lastName, firstName, sep = ", ")
    ) |>
    select(auID, name)
  dbDisconnect(conn)

  auInfo <- auInfo |>
    left_join(auNames, by = "auID") |>
    group_by(gID) |>
    summarise(
      auIDs = paste(auID, collapse = ","),
      auNames = paste(name, collapse = "<br>"),
      .groups = "drop"
    )

  plotData <- plotData |> left_join(auInfo, by = "auIDs")

  return(plotData)
}

#' Score all author pairs based on shared MeSH terms
#'
#' @param difftree a diffTree dataframe returned by diffTree(pruneDuplicates = T)
#' @param minLevel (Default 1, root) The minimum level to consider a term shared
#'
#' @import dplyr
#' @importFrom tidyr separate_rows
#'
#' @return A dataset that can be used to create a (plotly) Treemap
#' @export
#'
authorSimScore <- function(difftree, minLevel = 1) {
  # Only keep overlapping parts of the MeSH tree and separate info per author again
  overlap <- difftree |>
    filter(nAuth > 1, hasArticle, level >= minLevel) |>
    select(mtrID, auIDs, level, hasArticle) |>
    separate_rows(auIDs, sep = ",")

  auIDs <- unique(overlap$auIDs)

  # Calculate a simple similarity score (to be refined later):
  # - Add up all levels of the tree that overlap and have an article
  # - This assumes that deeper nested is more specific and thus more overlap
  apply(combn(auIDs, 2), 2, function(x) {
    simScore <- overlap |>
      filter(auIDs %in% x) |>
      group_by(mtrID) |>
      filter(n() > 1) |>
      slice(1) |>
      pull(level) |>
      sum()
    list(auID1 = x[1], auID2 = x[2], simScore = simScore)
  }) |>
    bind_rows()
}

#' Decorate an HTML element with a message
#'
#' @param elementID The ID of the element as set in Shiny
#' @param message (Optional) If set, this message will be shown, if not any
#' existing message for this ID will be removed
#' @param type (Default = "error"). info, error or success (will define the colour)
#' @param where (Default = "afterEnd) Relative position of the message to the
#' ID. Any of "beforeStart", "afterStart", "beforeEnd" or "afterEnd"
#' @param session (Default = getDefaultReactiveDomain()). Shiny session object
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom shiny insertUI removeUI getDefaultReactiveDomain
#'
#' @return A dataset that can be used to create a (plotly) Treemap
#' @export
#'
elementMsg <- function(
  elementID,
  message,
  type = "error",
  where = "afterEnd",
  session = getDefaultReactiveDomain()
) {
  # Make sure the tag ID starts with # or .
  tagID <- ifelse(
    str_detect(elementID, "^[.#]"),
    elementID,
    paste0("#", elementID)
  )

  # Remove existing element
  removeUI(paste0(tagID, "_msg"), session = session)

  # Stop if no new message
  if (missing(message)) {
    return()
  }

  # Add / update message
  msg <- tags$div(
    tags$i(
      message,
      style = sprintf(
        "color:%s",
        case_when(
          type == "info" ~ "#2196f3",
          type == "error" ~ "#f44336",
          type == "success" ~ "#4caf50",
          TRUE ~ "#262626"
        )
      )
    ),
    id = paste0(elementID, "_msg")
  )

  insertUI(selector = tagID, where = where, msg, session = session)
}

#' Filter publicationDetails object based on author of interest affiliation match
#'
#' This function is useful in case there are a lot of suprious matches that
#' would be too tedious to manage one by one
#'
#' @param publicationDetails Object generated by ncbi_publicationDetails
#' @param regex Search term (regular expression) to filter affiliations
#'
#' @import dplyr stringr
#'
#' @return Filtered publicationDetails object
#'
#' @export
filter_affiliation <- function(publicationDetails, regex, includeMissing = F) {
  # Get PMIDs that have an affilitation match using the regex for author of interest
  toKeep <- publicationDetails$affiliations |>
    filter(str_detect(affiliation, regex)) |>
    left_join(
      publicationDetails$coAuthors |>
        filter(
          lastName %in% publicationDetails$author$lastName,
          firstName %in% publicationDetails$author$firstName,
        ) |>
        select(PMID, authorOrder, tempId),
      by = c("PMID", "authorOrder")
    ) |>
    filter(!is.na(tempId)) |>
    pull(PMID) |>
    unique()

  if (includeMissing) {
    noAffiliations <- setdiff(
      publicationDetails$articles$PMID,
      publicationDetails$affiliations$PMID |> unique()
    )
    toKeep <- c(toKeep, noAffiliations)
  }

  # Filter based on PMIDs retained
  publicationDetails$articles = publicationDetails$articles |>
    filter(PMID %in% toKeep)
  publicationDetails$coAuthors = publicationDetails$coAuthors |>
    filter(PMID %in% toKeep)
  publicationDetails$affiliations = publicationDetails$affiliations |>
    filter(PMID %in% toKeep)
  publicationDetails$meshDescriptors = publicationDetails$meshDescriptors |>
    filter(PMID %in% toKeep)
  publicationDetails$meshQualifiers = publicationDetails$meshQualifiers |>
    filter(PMID %in% toKeep)

  # Only keep author name variations that are in any of the retained articles
  publicationDetails$author = publicationDetails$author |>
    inner_join(
      publicationDetails$coAuthors |>
        select(lastName, firstName, initials) |>
        distinct(),
      by = c("lastName", "firstName", "initials")
    )

  publicationDetails
}

#' Filter publicationDetails object based on author of interest affiliation match
#'
#' This function is useful in case there are a lot of suprious matches that
#' would be too tedious to manage one by one
#'
#' @param publicationDetails Object generated by ncbi_publicationDetails
#' @param PMIDs Vector of PMIDs to filter
#' @param remove (Default = F). If T the provided PMIDs will be removed instead of
#' @param showWarnings (Default = T). Display warnings if PMIDs don't match
#' filtered out
#'
#' @import dplyr stringr
#'
#' @return Filtered publicationDetails object
#'
#' @export
filter_PMID <- function(
  publicationDetails,
  PMIDs,
  remove = F,
  showWarnings = T
) {
  toKeep = intersect(publicationDetails$articles$PMID, PMIDs)

  if (length(toKeep) < length(PMIDs)) {
    warning(sprintf(
      "The following PMIDs were not found are are ignored: %s",
      paste(setdiff(PMIDs, publicationDetails$articles$PMID), collapse = ",")
    ))
  }

  # Invert in case of removing
  if (remove) {
    toKeep = setdiff(publicationDetails$articles$PMID, toKeep)
  }

  publicationDetails$articles = publicationDetails$articles |>
    filter(PMID %in% toKeep)
  publicationDetails$coAuthors = publicationDetails$coAuthors |>
    filter(PMID %in% toKeep)
  publicationDetails$affiliations = publicationDetails$affiliations |>
    filter(PMID %in% toKeep)
  publicationDetails$meshDescriptors = publicationDetails$meshDescriptors |>
    filter(PMID %in% toKeep)
  publicationDetails$meshQualifiers = publicationDetails$meshQualifiers |>
    filter(PMID %in% toKeep)

  # Only keep author name variations that are in any of the retained articles
  publicationDetails$author = publicationDetails$author |>
    inner_join(
      publicationDetails$coAuthors |>
        select(lastName, firstName, initials) |>
        distinct(),
      by = c("lastName", "firstName", "initials")
    )

  publicationDetails
}
