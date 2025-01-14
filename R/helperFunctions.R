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
simpleText = function(text){
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
checkTreeNums = function(treenums, output = "errorOnly"){
  check = str_detect(treenums, r"(^[A-Z]\d{2}(\.\d{3}){0,}$)")

  if(output == "vector"){
    return(check)
  } else if(output == "bool"){
    return(all(check))
  } else if(!all(check)){
    stop("Invalid tree numbers: ",
         paste(treenums[!check], collapse = ", "))
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
missingTreeNums = function(treenums){

  treenums = unique(treenums)

  currentNodes = treenums
  allNodes = currentNodes

  # Chop off the last node from the treenum to get parent, then repeat until root
  while(length(currentNodes) > 0){
    nextUp = str_remove(currentNodes, "\\.\\d+$")
    nextUp = nextUp[nextUp != currentNodes]
    allNodes = c(allNodes, nextUp)
    currentNodes = nextUp
  }

  allNodes = allNodes %>% unique()
  allNodes = allNodes[!allNodes %in% treenums]

  if(length(allNodes) == 0){
    return(NULL)
  } else {
    allNodes
  }

}

#' Timestamp
#'
#' @returns Time formatted as %Y-%m-%d %H:%M:%S
timeStamp = function(){
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
authorMeshTree <- function(auID, dbPath){

  conn <- dbGetConn(dbPath)

  # Get all MeSH tree entries for the author
  auMeshui <- tbl(conn, "author_affiliation") |> filter(auID == local(auID)) |> 
    select(arID, auID) |> distinct() |> left_join(
      tbl(conn, "mesh_article"), by = "arID" 
    ) |> left_join(
      tbl(conn, "meshLink"), by = "meshui"
    ) |> left_join(
      tbl(conn, "meshTree"), by = "uid"
    ) |> 
    # Ignore papers with no MeSH terms
    filter(!is.na(meshui)) |> 
    collect()

  # Use the meshTree to get all intermediate meshuis
  auTree <- auMeshui |> select(mtrID, uid, treenum) |> distinct() 

  # Cut off one level of the treenum and find its parents
  nextNums <- ifelse(str_detect(auTree$treenum, "\\."), 
    str_remove(auTree$treenum, "\\.\\d+$"), NA) |> unique(na.rm = T)

  while(length(nextNums) > 0){
    new <- tbl(conn, "meshTree") |> filter(treenum %in% local(nextNums)) |> 
      collect() 
    newTreenums <- setdiff(new$treenum, auTree$treenum)
    auTree <- bind_rows(auTree, new |> filter(treenum %in% newTreenums)) |> distinct()
    nextNums <- ifelse(str_detect(newTreenums, "\\."), 
      str_remove(newTreenums, "\\.\\d+$"), NA) |> unique(na.rm = T)
  }

  auTree <- auTree |> mutate(auID = as.integer({{auID}}), .before = 1)

  dbDisconnect(conn)

  return(list(auTree = auTree, arMesh = auMeshui))
}

diffTree <- function(auID1, auID2, dbPath){

  conn <- dbGetConn(dbPath)

  # Get for each treenum whether there is overlap or not
  diffTree <- bind_rows(amt1$auTree, amt2$auTree) |> group_by(treenum) |> 
    mutate(auID = ifelse(n() == 1, auID, 0) |> as.integer()) |> 
    ungroup() |> distinct() |> 
    mutate(level = as.integer(str_count(treenum, "\\.") + 1))

  # Add the MeSH term (description)
  meshTerm <- tbl(conn, "meshLink") |> filter(uid %in% local(diffTree$uid)) |> 
    left_join(tbl(conn, "meshTerm"), by = "meshui") |> distinct() |> 
    # If there are multiple term descriptions, only keep one
    group_by(uid) |> filter(mteID == min(mteID)) |> ungroup() |> collect()

  diffTree <- diffTree |> left_join(meshTerm |> select(uid, meshterm), by = "uid") |> 
    filter(!is.na(meshterm))

  # Get the parent for each treenum ("" =  root)
  diffTree <- diffTree |> mutate(
    parent = str_remove(diffTree$treenum, "\\.\\d+$"),
    parent = ifelse(diffTree$treenum == parent, "", parent)
  ) 

  # Add the number of children for each treenum and sort the tree by treenum (important for next step)
  diffTree <- diffTree |> left_join(
    diffTree |> group_by(treenum = parent) |> summarise(children = n(), .groups = "drop"),
    by = "treenum"
  ) |> mutate(children = as.integer(ifelse(is.na(children), 0, children))) |> 
    arrange(treenum)

  # Check if treenums can be merged if they don't branch off
  b = 1
  bID = c(b, rep(NA, nrow(diffTree) - 1))
  for(i in 2:nrow(diffTree)){
    
    if(diffTree$parent[i] != diffTree$treenum[i-1] | diffTree$treenum[i] == "" |
      diffTree$children[i] > 1 |diffTree$children[i-1] > 1){
      b = b + 1
    }
    bID[i] = b
  }

  diffTree <- diffTree |> 
    mutate(
      branchID = as.integer({{bID}})
    ) |>  select(treenum, branchID, children, parent, meshterm, everything())

  # Get the parent branchID
  diffTree <- diffTree |> left_join(
    diffTree |> select(parent = treenum, parentBranchID = branchID),
    by = "parent"
  )

  dbDisconnect(conn)
}