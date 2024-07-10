

#' Get the best matching Pubmed author name
#'
#' @param first First name
#' @param lastLast name
#'
#' @importFrom rentrez entrez_search entrez_summary
#' @importFrom stringr str_detect
#'
#' @return Author name as string "lastName Initials" as used in Pubmed
#' @export
#'
ncbi_author = function(first, last){
  result = entrez_search("pubmed", term = sprintf("%s %s[Author]", last, first), retmax = 1)

  if(length(result$ids) == 0) {
    warning("This name might not get good PubMed matches")
    return(paste(last, first))
  }

  result = entrez_summary("pubmed", result$ids[1])
  result$authors$name[str_detect(simpleText(result$authors$name), simpleText(last))]
}

#' Get author names, papers, co-authors and affiliations for a specific researcher
#'
#' @param firstName First name of author of interest
#' @param lastName Last name of author of interest
#' @param n Number of papers to fetch from Pubmed
#'
#' @importFrom rentrez entrez_search entrez_fetch
#' @import xml2
#' @import dplyr
#' @importFrom stringr str_match
#'
#' @return List with 5 data frames
#'  author: Names the author used in papers
#'  articles List of author's articles on Pubmed
#'  coAuthors Co-authors on each paper
#'  affiliations Affiliation of each author
#'  meshDescriptors MeSH term descriptor associated with each paper
#'  meshQualifiers MeSH term qualifier associated with each paper
#'
#' @export
#'
ncbi_authorPublicationInfo = function(firstName, lastName, n = -1){

  # Max 10000 papers (PubMed limit)
  n = ifelse(n == -1, 10000, n)

  # Get the best matching author name form PubMed
  author = ncbi_author(first = firstName, lastName)

  # Search for all their publication PMIDs (up to n)
  PMIDs = entrez_search("pubmed", term = sprintf("%s[Author]", author),
                        retmax = n, sort = "relevance", use_history = TRUE)

  # Fetch all paper related info
  info = read_xml(entrez_fetch("pubmed", rettype="xml",
                               web_history = PMIDs$web_history, retmax = n))
  info = xml_find_all(info, ".//PubmedArticle//MedlineCitation")

  # Generate a dataframe of paper info
  articleInfo = data.frame(
    PMID = PMIDs$ids,
    title = xml_find_first(info, "./Article/ArticleTitle") %>% xml_text(),
    journal = xml_find_first(info, "./Article/Journal/Title") %>% xml_text(),
    year = xml_find_first(info, "./Article/Journal/JournalIssue/PubDate/Year") %>%
      xml_text(),
    month = xml_find_first(info, "./Article/Journal/JournalIssue/PubDate/Month") %>%
      xml_text(),
    day = xml_find_first(info, "./Article/Journal/JournalIssue/PubDate/Day") %>%
      xml_text()
  ) %>% mutate(
    year = as.integer(year),
    day = as.integer(day)
  )

  # Get the author list from each paper

  authorInfo = xml_find_first(info, "./Article/AuthorList")

  # Author names
  lastNames = xml_find_all(authorInfo, "./Author/LastName")
  # The ids will check in which articles author info is present so it can
  #  be properly joined later by PMID
  ids = str_match(xml_path(lastNames), r"(PubmedArticle\[(\d+)\].+Author(\[(\d+)\])?)")

  lastNames = data.frame(
    lastName = lastNames %>% xml_text(),
    articleID = as.integer(ids[,2]),
    authorOrder = as.integer(ids[,4])
  ) %>%
    # Articles with only one author did not match id regex so we put in 1 manually
    mutate(authorOrder = ifelse(is.na(authorOrder), 1, authorOrder))

  # Rarely authors only have last name so we need to process rest seperately
  otherNames = xml_find_all(authorInfo, "./Author/ForeName")
  ids = str_match(xml_path(otherNames), r"(PubmedArticle\[(\d+)\].+Author(\[(\d+)\])?)")

  otherNames = data.frame(
    firstName = otherNames %>% xml_text(),
    initials = xml_find_all(authorInfo, "./Author/Initials") %>% xml_text(),
    articleID = as.integer(ids[,2]),
    authorOrder = as.integer(ids[,4])
  ) %>%
    mutate(authorOrder = ifelse(is.na(authorOrder), 1, authorOrder))

  # Bind first, last and initials together
  authorNames = lastNames %>%
    left_join(otherNames, by = c("articleID", "authorOrder"))

  # Collective names - Names of consortia etc. who can be authors
  collectiveNames = xml_find_all(authorInfo, "./Author/CollectiveName")
  ids = str_match(xml_path(collectiveNames),
                  r"(PubmedArticle\[(\d+)\].+Author(\[(\d+)\])?)")

  collectiveNames = data.frame(
    collectiveName = collectiveNames %>% xml_text(),
    articleID = as.integer(ids[,2]),
    authorOrder = as.integer(ids[,4])
  ) %>%
    mutate(authorOrder = ifelse(is.na(authorOrder), 1, authorOrder))

  # Bind all author info together into a single data frame
  authors = bind_rows(
    authorNames %>%  left_join(
      data.frame(
        articleID = 1:length(PMIDs$ids),
        PMID = PMIDs$ids
      ), by = "articleID"),

    collectiveNames %>%  left_join(
      data.frame(
        articleID = 1:length(PMIDs$ids),
        PMID = PMIDs$ids
      ), by = "articleID")
  ) %>% select(PMID, authorOrder, everything()) %>%
    arrange(articleID, authorOrder) %>%
    select(-articleID)

  # Affiliations
  affiliations = xml_find_all(authorInfo, "./Author/AffiliationInfo/Affiliation")
  ids = str_match(xml_path(affiliations), r"(PubmedArticle\[(\d+)\].+Author\[(\d+)\])")

  affiliations = data.frame(
    affiliation = affiliations %>% xml_text(),
    articleID = as.integer(ids[,2]),
    authorOrder = as.integer(ids[,3])
  ) %>% left_join(
    data.frame(
      articleID = 1:length(PMIDs$ids),
      PMID = PMIDs$ids
    ), by = "articleID") %>%
    # IN case there is only one affiliation for a whole author list it applies
    # to all and we set the authorOrder to NA
    group_by(articleID) %>%
    mutate(
      n = n(),
      authorOrder = ifelse(n == 1, NA, authorOrder)) %>%
    ungroup() %>%
    select(-n, -articleID)

  # Extract the MeSH term descriptors and qualifiers per paper (if any)
  descriptors = xml_find_all(info, "./MeshHeadingList/MeshHeading/DescriptorName")
  ids = str_match(xml_path(descriptors), r"(PubmedArticle\[(\d+)\].+MeshHeading\[(\d+)\])")

  descriptors = data.frame(
    DescriptorName = descriptors %>% xml_text(),
    DescriptorUI = descriptors %>% xml_attr("UI"),
    DescriptorMajor = descriptors %>% xml_attr("MajorTopicYN"),
    articleID = as.integer(ids[,2]),
    meshLink = as.integer(ids[,3])
  ) %>% left_join(
    data.frame(
      articleID = 1:length(PMIDs$ids),
      PMID = PMIDs$ids
    ), by = "articleID") %>%
    select(-articleID)

  qualifiers = xml_find_all(info, "./MeshHeadingList/MeshHeading/QualifierName")
  ids = str_match(xml_path(qualifiers), r"(PubmedArticle\[(\d+)\].+MeshHeading\[(\d+)\])")

  qualifiers = data.frame(
    QualifierName = qualifiers %>% xml_text(),
    QualifierUI = qualifiers %>% xml_attr("UI"),
    QualifierMajor = qualifiers %>% xml_attr("MajorTopicYN"),
    articleID = as.integer(ids[,2]),
    meshLink = as.integer(ids[,3])
  ) %>% left_join(
    data.frame(
      articleID = 1:length(PMIDs$ids),
      PMID = PMIDs$ids
    ), by = "articleID") %>%
    select(-articleID)


  # Find all name variations the author of interest has used in papers
  author = authors %>%
    select(lastName, firstName, initials) %>%
    distinct() %>%
    filter(simpleText(lastName) == simpleText({{lastName}}))

  # Return the results
  return(list(author = author, articles = articleInfo, coAuthors = authors,
              affiliations = affiliations,
              meshDescriptors = descriptors, meshQualifiers = qualifiers))

}

#' Get detailed MeSH info based on MeSH IDs or terms
#'
#' You should only use one parameter:
#' @param terms vector of MeSH descriptor IDs, tree numbers, or other terms
#' @param uid MeSH uid (integer values)
#'
#' @importFrom rentrez entrez_search entrez_summary
#' @importFrom purrr map_df
#'
#' @return a list with two elements: meshTerms and meshTree
#' @export
#'
ncbi_meshInfo = function(terms = NULL, uid = NULL) {

  if(is.null(terms) & is.null(terms)){
    stop("You need at least one MeSH term / descriptor or one MeSH Entrez uid")
  }

  if(!is.null(terms) & !is.null(uid)){
    stop("You can only search by MeSH term OR MeSH Entrez uid, not both")
  }

  if(!is.null(terms)){

    # Make sure pasting together multiple terms is not exceeding the URL limit
    group = (nchar(terms) %>% cumsum()) %/% 2000 + 1

    # Search the mesh database for the uid of each term
    uid = lapply(seq(1, max(group)), function(i){
      entrez_search("mesh", paste(terms[group == i], collapse = " OR "),
                             retmax = 500)$ids
    })

    uid = unlist(uid)
  }

  # Get the MeSH data from NCBI
  meshInfo = sapply(seq(1, length(uid), by = 250), function(i){
    getui = uid[i:min(i+249, length(uid))]
    entrez_summary("mesh", id = getui)
  })

  if(max(group) > 1){
    meshInfo = do.call(c, meshInfo)
  }


  # Extract the mesh terms
  meshTerms = map_df(meshInfo, function(x){
    data.frame(
      meshui = x$ds_meshui,
      meshterm = x$ds_meshterms
    )
  })

  # Extract the tree info
  meshTree = map_df(meshInfo, function(x){
    data.frame(
      uid = x$uid,
      meshui = x$ds_meshui,
      treenum = x$ds_idxlinks$treenum
    )
  })

  # Remove any invalid treenums
  invalidTreenum = !checkTreeNums(meshTree$treenum, output = "vector")
  invlaidMeshui = meshTree$meshui[invalidTreenum]
  meshTerms = meshTerms %>% filter(!meshui %in% invlaidMeshui)
  meshTree = meshTree[!invalidTreenum,]

  return(list(meshTerms = meshTerms, meshTree = meshTree))

}
