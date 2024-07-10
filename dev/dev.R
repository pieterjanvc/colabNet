
ncbi_meshInfo = function(terms = NULL, uid = NULL) {

  if(is.null(terms) & is.null(terms)){
    stop("You need at least one MeSH term / descriptor or one MeSH Entrez uid")
  }

  if(!is.null(terms)){

    group = (nchar(terms) %>% cumsum()) %/% 2000 + 1

    # Search the mesh database for the uid of each term
    uid = lapply(seq(1, max(group)), function(i){
      rentrez::entrez_search("mesh", paste(terms[group == i], collapse = " OR "),
                             retmax = 500)$ids
    })

    uid = unlist(uid)
  }

  # Get the MeSH data from NCBI
  meshInfo = sapply(seq(1, length(uid), by = 250), function(i){
    getui = uid[i:min(i+249, length(uid))]
    rentrez::entrez_summary("mesh", id = getui)
  })

  if(max(group) > 1){
    meshInfo = do.call(c, meshInfo)
  }


  # Extract the mesh terms
  meshterms = purrr::map_df(meshInfo, function(x){
    data.frame(
      meshui = x$ds_meshui,
      meshterm = x$ds_meshterms
    )
  })

  # Extract the tree info
  meshTree = purrr::map_df(meshInfo, function(x){
    data.frame(
      uid = x$uid,
      meshui = x$ds_meshui,
      treenum = x$ds_idxlinks$treenum
    )
  })

  return(list(meshterms = meshterms, meshTree = meshTree))

}

missingTreeNums = function(known){
  # Get all possible tree nodes that are higher than the any given mesh term
  currentNodes = known
  allNodes = currentNodes

  while(length(currentNodes) > 0){
    nextUp = stringr::str_remove(currentNodes, "\\.\\d+$")
    nextUp = nextUp[nextUp != currentNodes]
    allNodes = c(allNodes, nextUp)
    currentNodes = nextUp
  }

  allNodes = allNodes %>% unique() %>% sort()

  return(allNodes[!allNodes %in% known])
}

# Get meshUI from papers
x = ncbi_authorPublicationInfo("PJ", "Van Camp")
meshui = unique(x$meshDescriptors$DescriptorUI)

meshInfo = ncbi_meshInfo(terms = meshui)
missingNodes = missingTreeNums(meshInfo$meshTree$treenum)

# Check DB if the missing nodes are already known
myConn = RSQLite::dbConnect(RSQLite::SQLite(),"dev/test.db")

knownNodes = tbl(myConn, "meshTree") %>% filter(treenum %in% local(missingNodes)) %>%
  pull(treenum)

missingNodes = setdiff(missingNodes, knownNodes)

# Only get new info on nodes not already in the database
missingNodes = ncbi_meshInfo(missingNodes)

# Add to database
x = RSQLite::dbWriteTable(myConn, "meshTerms", missingNodes$meshterms, append = T)
x = RSQLite::dbWriteTable(myConn, "meshTree", missingNodes$meshTree, append = T)

RSQLite::dbDisconnect(myConn)
