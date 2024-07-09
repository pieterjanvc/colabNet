
# Get meshUI from papers
x = authorPublicationInfo("PJ", "Van Camp")
meshui = unique(x$meshDescriptors$DescriptorUI)

# Search the mesh database for the uid of each term
uid = rentrez::entrez_search("mesh", paste(meshui, collapse = " OR "), retmax = 100)

# Get all info
meshInfo = rentrez::entrez_summary("mesh", id = uid$ids)


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



# Get all possible tree nodes that are higher than the any given mesh term
currentNodes = meshTree$treenum
allNodes = nextUp
while(length(currentNodes) > 0){
  nextUp = stringr::str_remove(currentNodes, "\\.\\d+$")
  nextUp = nextUp[nextUp != currentNodes]
  allNodes = c(allNodes, nextUp)
  currentNodes = nextUp
}

allNodes = allNodes %>% unique() %>% sort()
newNodes = allNodes[!allNodes %in% meshTree$treenum]

# repeat the orginal process (make function)
uid = rentrez::entrez_search("mesh", paste(newNodes, collapse = " OR "), retmax = 200)
#...
