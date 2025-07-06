#' Get the best matching Pubmed author name
#'
#' @param lastName Last name
#' @param firstName First name. Add any middle initials the author uses to publish.
#' E.g. 'Joseph E Murray' --> 'Joseph E'. To be broader in case of multiple spellings,
#' only supply initials e.g. 'Joseph E Murray' --> 'JE'.
#' @param samples (Default = 10). Number of papers to check for variations in
#' name spelling
#' @param showWarnings (Default = T) Show warnings
#'
#' @importFrom rentrez entrez_search entrez_summary
#' @importFrom stringr str_detect
#'
#' @return A data frame with lastName, firstName, initials based on top hit
#' articles. If there are multiple name versions, they are grouped based on
#' similarity (using simpleText function)
#' @export
#'
ncbi_author <- function(
  lastName,
  firstName,
  samples = 10,
  showWarnings = T
) {
  # Find the top hit for the provided author name
  result <- entrez_search(
    "pubmed",
    term = sprintf(
      '"%s %s"[Author]',
      lastName,
      firstName
    ),
    retmax = samples
  )

  if (length(result$ids) == 0) {
    if (showWarnings) {
      warning("This name might not get good PubMed matches")
    }
    return(data.frame(
      lastName = character(0),
      firstName = character(0),
      initials = character(0),
      group = integer(0)
    ))
  }

  # Get article details
  result <- read_xml(entrez_fetch("pubmed", result$ids, rettype = "xml"))

  # Extract the author list from the article and keep the one of interest
  result <- xml_find_all(result, "//MedlineCitation/Article/AuthorList")

  authorInfo <- map_df(result, function(x) {
    authorinfo <- data.frame(
      lastName = xml_find_all(x, "./Author/LastName") |> xml_text(),
      firstName = xml_find_all(x, "./Author/ForeName") |> xml_text(),
      initials = xml_find_all(x, "./Author/Initials") |> xml_text()
    )

    authorinfo <- authorinfo |>
      filter(
        str_detect(simpleText(lastName), simpleText({{ lastName }})),
        str_detect(simpleText(initials), simpleText({{ firstName }})) |
          str_detect(simpleText(firstName), simpleText({{ firstName }}))
      )

    authorinfo
  }) |>
    distinct() |>
    group_by(
      simpleText(lastName),
      simpleText(firstName),
      simpleText(initials)
    ) |>
    mutate(group = cur_group_id()) |>
    ungroup() |>
    select(lastName, firstName, initials, group)

  return(authorInfo)
}

#' Get basic article info for an author if name is likely unique
#'
#' @param lastName Author last name
#' @param firstName First name. Add any middle initials the author uses to publish.
#' E.g. 'Joseph E Murray' --> 'Joseph E'. To be broader in case of multiple spellings,
#' only supply initials e.g. 'Joseph E Murray' --> 'JE'.
#' @param PMIDs (optional) If set limit the search to the PMIDs for the given author
#' @param PMIDonly Return only valid PMID, not the data table
#' @param returnHistory (Default = False). If true, will return rentrez web history object
#' @param stopFetching (Default = 1000) If more than this number of articles are
#' found, it is very likely that results for multiple authors with the same name are
#' found. In this case is might be better to manually provide the PMID instead
#'
#' @importFrom rentrez entrez_search entrez_summary
#' @importFrom stringr str_extract
#'
#' @return List with 4 elements
#' - success: TRUE if success; If FALSE, number of matches > stopFetching, nothing returned
#' - n: Total number of articles found (returned even when stopFetching reached)
#' - PMIDs: when not stopFetching
#' - articles: Data frame with basic article info (when PMIDonly = F and not stopFetching)
#' - history: If returnHistory = T: History object that can be used for subsequent entrez
#' calls (for large datasets), otherwise NA
#'
#' @export
ncbi_authorArticleList <- function(
  lastName,
  firstName,
  PMIDs,
  returnHistory = F,
  PMIDonly = F,
  simpletext = T,
  stopFetching = 1000
) {
  if (!missing(PMIDs)) {
    # Only keep numeric values
    PMIDs <- as.numeric(PMIDs)
    PMIDs <- PMIDs[!is.na(PMIDs)]

    addFilter = ifelse(
      length(PMIDs) > 0,
      sprintf(" AND (%s[pmid])", paste(PMIDs, collapse = ",")),
      ""
    )
  } else {
    addFilter = ""
  }

  if (simpletext) {
    lastName = simpleText(lastName)
    firstName = simpleText(firstName)
  }

  # Search on Pubmed for author
  searchResult <- entrez_search(
    "pubmed",
    term = sprintf(
      '("%s %s"[Author])%s',
      lastName,
      firstName,
      addFilter
    ),
    retmax = stopFetching,
    use_history = returnHistory
  )

  if (returnHistory) {
    history = searchResult$web_history
  } else {
    history = NA
  }

  # Check if the search had too many results
  if (searchResult$count > stopFetching) {
    return(list(
      success = F,
      n = searchResult$count,
      PMID = NA,
      articles = NA,
      history = NA
    ))
  }

  PMID = searchResult$ids

  # Don't fetch more data if PMIDonly = T
  if (PMIDonly | length(PMID) == 0) {
    return(list(
      success = T,
      n = length(PMID),
      PMID = PMID,
      articles = NA,
      history = history
    ))
  }

  # Get article info
  if (returnHistory) {
    result <- entrez_summary(
      "pubmed",
      web_history = history,
      always_return_list = T
    )
  } else {
    result <- entrez_summary("pubmed", PMID, always_return_list = T)
  }

  articles <- data.frame(
    PMID = sapply(result, "[[", "uid"),
    date = sapply(result, "[[", "sortpubdate") |> str_extract("^[^\\s]+"),
    title = sapply(result, "[[", "title"),
    journal = sapply(result, "[[", "source"),
    authors = sapply(result, "[[", c("authors", "name"), simplify = T) |>
      sapply(paste, collapse = ", ")
  )

  return(list(
    success = T,
    n = nrow(articles),
    PMID = articles$PMID,
    articles = articles,
    history = history
  ))
}

#' Get author names, papers, co-authors and affiliations for a specific researcher
#'
#' @param PMIDs Vector of PMIDs. Use ncbi_authorArticleList if you want to search by name.
#' @param lastName Author last name of the author of interest
#' @param firstName Author first name of the author of interest
#' @param initials Author initials of the author of interest
#' @param matchOnFirstOnly (Default = FALSE). If set to TRUE, exact matching on first name is
#' performed (can lead to missed matches but useful when common last name)
#' @param history (Default NA) Use rentrez history object in case of long PMIDs list.
#' this is useful in case of a large number of articles
#' @param n (Default = -1 or all) Number of papers to fetch from Pubmed
#'
#' @importFrom rentrez entrez_search entrez_fetch entrez_post
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
ncbi_publicationDetails <- function(
  PMIDs,
  lastName,
  firstName,
  initials,
  matchOnFirstOnly = F,
  history = NA,
  n = -1
) {
  # Max 10000 papers (PubMed limit)
  n <- ifelse(n == -1, 10000, n)

  # Fetch all paper details
  if (all(is.na(history))) {
    info <- read_xml(entrez_fetch("pubmed", PMIDs, rettype = "xml", retmax = n))
  } else {
    info <- read_xml(entrez_fetch(
      "pubmed",
      web_history = history,
      rettype = "xml",
      retmax = n
    ))
  }

  info <- xml_find_all(info, ".//PubmedArticle")

  # Generate a dataframe of paper info
  articleInfo <- data.frame(
    PMID = xml_find_first(info, "./MedlineCitation/PMID") |> xml_text(),
    title = xml_find_first(info, "./MedlineCitation/Article/ArticleTitle") |>
      xml_text(),
    journal = xml_find_first(info, "./MedlineCitation/Article/Journal/Title") |>
      xml_text(),
    year = xml_find_first(
      info,
      './PubmedData/History/PubMedPubDate[@PubStatus="medline"]/Year'
    ) |>
      xml_text(),
    month = xml_find_first(
      info,
      './PubmedData/History/PubMedPubDate[@PubStatus="medline"]/Month'
    ) |>
      xml_text(),
    day = xml_find_first(
      info,
      './PubmedData/History/PubMedPubDate[@PubStatus="medline"]/Day'
    ) |>
      xml_text()
  ) |>
    mutate(
      year = as.integer(year),
      day = as.integer(day)
    )

  if (length(setdiff(articleInfo$PMID, PMIDs)) > 0) {
    warning(
      "The following PMIDs were not found are are ignored:",
      paste(setdiff(articleInfo$PMID, PMIDs), collapse = ", ")
    )
  }

  PMIDs <- articleInfo$PMID

  # Get the author list from each paper

  authorInfo <- xml_find_first(info, "./MedlineCitation/Article/AuthorList")

  # Author names
  lastNames <- xml_find_all(authorInfo, "./Author/LastName")
  # The ids will check in which articles author info is present so it can
  #  be properly joined later by PMID
  ids <- str_match(
    xml_path(lastNames),
    r"(PubmedArticle(\[(\d+)\])?\/.*Author(\[(\d+)\])?)"
  )

  lastNames <- data.frame(
    lastName = lastNames |> xml_text(),
    articleID = as.integer(ids[, 3]),
    authorOrder = as.integer(ids[, 5])
  ) |>
    # Articles with only one author did not match id regex so we put in 1 manually
    mutate(
      articleID = ifelse(is.na(articleID), 1, articleID),
      authorOrder = ifelse(is.na(authorOrder), 1, authorOrder)
    )

  # Rarely authors only have last name so we need to process rest seperately
  otherNames <- xml_find_all(authorInfo, "./Author/ForeName")
  ids <- str_match(
    xml_path(otherNames),
    r"(PubmedArticle(\[(\d+)\])?\/.*Author(\[(\d+)\])?)"
  )

  otherNames <- data.frame(
    firstName = otherNames |> xml_text(),
    initials = xml_find_all(authorInfo, "./Author/Initials") |> xml_text(),
    articleID = as.integer(ids[, 3]),
    authorOrder = as.integer(ids[, 5])
  ) |>
    mutate(
      articleID = ifelse(is.na(articleID), 1, articleID),
      authorOrder = ifelse(is.na(authorOrder), 1, authorOrder)
    )

  # Bind first, last and initials together
  authorNames <- lastNames |>
    left_join(otherNames, by = c("articleID", "authorOrder"))

  # Collective names - Names of consortia etc. who can be authors
  collectiveNames <- xml_find_all(authorInfo, "./Author/CollectiveName")
  ids <- str_match(
    xml_path(collectiveNames),
    r"(PubmedArticle(\[(\d+)\])?\/.*Author(\[(\d+)\])?)"
  )

  collectiveNames <- data.frame(
    collectiveName = collectiveNames |> xml_text(),
    articleID = as.integer(ids[, 3]),
    authorOrder = as.integer(ids[, 5])
  ) |>
    mutate(
      articleID = ifelse(is.na(articleID), 1, articleID),
      authorOrder = ifelse(is.na(authorOrder), 1, authorOrder)
    )

  # Bind all author info together into a single data frame
  authors <- bind_rows(
    authorNames |>
      left_join(
        data.frame(
          articleID = 1:length(PMIDs),
          PMID = PMIDs
        ),
        by = "articleID"
      ),
    collectiveNames |>
      left_join(
        data.frame(
          articleID = 1:length(PMIDs),
          PMID = PMIDs
        ),
        by = "articleID"
      )
  ) |>
    select(PMID, authorOrder, everything()) |>
    arrange(articleID, authorOrder) |>
    select(-articleID)

  # Find all name variations the author of interest has used in papers and pick default

  # In case the first and last name are identical, the initials should be too,
  # so pick the most common one
  authors <- authors |>
    group_by(
      x = simpleText(lastName),
      y = simpleText(firstName),
      z = simpleText(collectiveName)
    ) |>
    mutate(
      initials = ifelse(is.na(initials), NA, names(which.max(table(initials))))
    ) |>
    ungroup() |>
    select(-x, -y, -z)

  # Now use last name and initials to find name variations
  authors <- authors |>
    group_by(lastName, firstName, initials, collectiveName) |>
    mutate(n = n()) |>
    group_by(
      simple = simpleText(lastName),
      initials,
      simpleText(collectiveName)
    ) |>
    arrange(desc(n)) |>
    mutate(tempId = cur_group_id(), default = row_number() == 1) |>
    group_by(lastName, firstName, initials, collectiveName) |>
    mutate(default = any(default)) |>
    ungroup() |>
    select(-simple, -n)

  if (matchOnFirstOnly) {
    author <- authors |>
      select(tempId, lastName, firstName, initials, default) |>
      filter(
        simpleText(lastName) == simpleText({{ lastName }}),
        simpleText(firstName) == simpleText({{ firstName }})
      ) |>
      distinct()
  } else {
    author <- authors |>
      select(tempId, lastName, firstName, initials, default) |>
      filter(
        simpleText(lastName) == simpleText({{ lastName }}),
        simpleText(initials) == simpleText({{ initials }})
      ) |>
      distinct()
  }

  # Affiliations
  affiliations <- xml_find_all(
    authorInfo,
    "./Author/AffiliationInfo/Affiliation"
  )
  ids <- str_match(
    xml_path(affiliations),
    r"(PubmedArticle(\[(\d+)\])?\/.*Author(\[(\d+)\])?)"
  )

  affiliations <- data.frame(
    affiliation = affiliations |> xml_text(),
    articleID = as.integer(ids[, 3]),
    authorOrder = as.integer(ids[, 5])
  ) |>
    mutate(
      articleID = ifelse(is.na(articleID), 1, articleID),
      authorOrder = ifelse(is.na(authorOrder), 1, authorOrder)
    ) |>
    left_join(
      data.frame(
        articleID = 1:length(PMIDs),
        PMID = PMIDs
      ),
      by = "articleID"
    ) |>
    # In case there is only one affiliation for a whole author list note that
    group_by(articleID) |>
    mutate(
      n = n(),
      sharedAffiliation = ifelse(n == 1, T, F)
    ) |>
    ungroup() |>
    select(-n, -articleID)

  # Duplicate shared affiliations
  shared <- affiliations |> filter(sharedAffiliation) |> select(-authorOrder)
  shared <- shared |>
    left_join(
      authors |> filter(PMID %in% shared$PMID) |> select(PMID, authorOrder),
      by = "PMID"
    ) |>
    filter(authorOrder > 1)

  affiliations <- bind_rows(affiliations, shared)

  # Extract the MeSH term descriptors and qualifiers per paper (if any)
  descriptors <- xml_find_all(
    info,
    "./MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName"
  )
  ids <- str_match(
    xml_path(descriptors),
    r"(PubmedArticle(\[(\d+)\])?\/.*MeshHeading(\[(\d+)\])?)"
  )

  descriptors <- data.frame(
    DescriptorName = descriptors |> xml_text(),
    DescriptorUI = descriptors |> xml_attr("UI"),
    DescriptorMajor = descriptors |> xml_attr("MajorTopicYN"),
    articleID = as.integer(ids[, 3]),
    meshLink = as.integer(ids[, 5])
  ) |>
    mutate(
      articleID = ifelse(is.na(articleID), 1, articleID),
      meshLink = ifelse(is.na(meshLink), 1, meshLink)
    ) |>
    left_join(
      data.frame(
        articleID = 1:length(PMIDs),
        PMID = PMIDs
      ),
      by = "articleID"
    ) |>
    select(-articleID)

  qualifiers <- xml_find_all(
    info,
    "./MedlineCitation/MeshHeadingList/MeshHeading/QualifierName"
  )
  ids <- str_match(
    xml_path(qualifiers),
    r"(PubmedArticle(\[(\d+)\])?\/.*MeshHeading(\[(\d+)\])?)"
  )

  qualifiers <- data.frame(
    QualifierName = qualifiers |> xml_text(),
    QualifierUI = qualifiers |> xml_attr("UI"),
    QualifierMajor = qualifiers |> xml_attr("MajorTopicYN"),
    articleID = as.integer(ids[, 3]),
    meshLink = as.integer(ids[, 5])
  ) |>
    mutate(
      articleID = ifelse(is.na(articleID), 1, articleID),
      meshLink = ifelse(is.na(meshLink), 1, meshLink)
    ) |>
    left_join(
      data.frame(
        articleID = 1:length(PMIDs),
        PMID = PMIDs
      ),
      by = "articleID"
    ) |>
    select(-articleID)

  # Return the results
  return(list(
    author = author,
    articles = articleInfo,
    coAuthors = authors,
    affiliations = affiliations,
    meshDescriptors = descriptors,
    meshQualifiers = qualifiers
  ))
}

#' Get detailed MeSH info based on MeSH IDs or terms
#'
#' You should only use one parameter:
#' @param values vector of MeSH values to search for
#' @param type The type needs to be meshui (MeSH ui) , treenum (tree number) or uid (MeSH Entrez uid)
#'
#' @importFrom rentrez entrez_search entrez_summary
#' @importFrom purrr map_df
#'
#' @return a list with two elements: meshTerms and meshTree
#' @export
#'
ncbi_meshInfo <- function(values, type = c("meshui", "treenum", "uid")) {
  if (!type[1] %in% c("meshui", "treenum", "uid")) {
    stop(
      "The type needs to be meshui (MeSH ui) , treenum (tree number) or uid (MeSH Entrez uid)"
    )
  }

  values <- unique(values)
  type <- type[1]

  if (type != "uid") {
    # Make sure pasting together multiple values is not exceeding the URL limit
    group <- ((nchar(values) + nchar(type) + 2) |> cumsum()) %/% 2000 + 1

    # Search the mesh database for the uid of each term
    uid <- lapply(seq(1, max(group)), function(i) {
      entrez_search(
        "mesh",
        paste(
          paste0(values[group == i], sprintf("[%s]", type)),
          collapse = " OR "
        ),
        retmax = 500
      )$ids
    })

    uid <- unlist(uid)
  }

  # Get the MeSH data from NCBI
  meshInfo <- lapply(seq(1, length(uid), by = 250), function(i) {
    getui <- uid[i:min(i + 249, length(uid))]
    entrez_summary("mesh", id = getui, always_return_list = T)
  })

  meshInfo <- do.call(c, meshInfo)

  # Extract the mesh terms
  meshTerms <- map_df(meshInfo, function(x) {
    data.frame(
      meshui = x$ds_meshui,
      meshterm = x$ds_meshterms
    )
  })

  # Extract the tree info
  meshTree <- map_df(meshInfo, function(x) {
    data.frame(
      uid = x$uid,
      meshui = x$ds_meshui,
      treenum = x$ds_idxlinks$treenum
    )
  })

  # Remove any invalid treenums
  invalidTreenum <- !checkTreeNums(meshTree$treenum, output = "vector")
  invlaidMeshui <- meshTree$meshui[invalidTreenum]
  meshTerms <- meshTerms |> filter(!meshui %in% invlaidMeshui)
  meshTree <- meshTree[!invalidTreenum, ]

  return(list(meshTerms = meshTerms, meshTree = meshTree))
}
