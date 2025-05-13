library(dplyr)

# Connect to the database or setup a new one
dbSetup("dev/PGG.db", checkSchema = T)
# Load faculty data
faculty <- readxl::read_xlsx("data/PGG Faculties.xlsx")
# Filter for articles where the author of interest has Harvard in their affiliations
affiliationFilter <- "[Hh]arvard"

i = 54
# Strict search for faculty based on last and first name (if they have a middle name
#  the initial must be after first name)
authorPublications_all <- lapply(41:60, function(i) {
  firstName = faculty$First_Name[i]
  lastName = faculty$Last_Name[i]
  print(paste(firstName, lastName))
  authorinfo <- ncbi_author(lastName, firstName)

  if (n_distinct(authorinfo$group) > 1) {
    warning(sprintf(
      "Multiple matches for %s, %s [%i]. Skipped",
      lastName,
      firstName,
      i
    ))
    return(NULL)
  }

  result <- ncbi_authorArticleList(
    authorinfo$lastName[1],
    authorinfo$firstName[1],
    authorinfo$initials[1],
    PMIDonly = T,
    stopFetching = 1000,
    returnHistory = T,
    searchInitials = F,
    simpletext = T
  )

  if (!result$success) {
    warning(sprintf(
      "Too many results for %s, %s [%i]. Skipped",
      lastName,
      firstName,
      i
    ))
    return(NULL)
  }

  authorPublications <- ncbi_publicationDetails(
    PMIDs = result$PMID,
    lastName = authorinfo$lastName[1],
    firstName = authorinfo$firstName[1],
    initials = authorinfo$initials[1],
    history = result$history
  ) |>
    filter_affiliation(affiliationFilter)

  if (nrow(authorPublications$articles) == 0) {
    warning(sprintf(
      "No articles found for %s, %s [%i]. Skipped",
      lastName,
      firstName,
      i
    ))
    return(NULL)
  }
  # authorPublications_all[[14]] <- authorPublications
  authorPublications
})

# authorPublications <- authorPublications[!sapply(authorPublications, is.null)]
# saveRDS(authorPublications_all, "data/PGG3.rds")
authorPublications_all <- readRDS("data/PGG3.rds")

authorPublications <- authorPublications_all[[10]]
# ℹ In index: 5.
# Caused by error:
# ! NOT NULL constraint failed: author_affiliation.auID
# DBoperations.R:509:3
result <- map_df(authorPublications_all, function(x) {
  print("Next one")
  if (is.null(x)) {
    warning("Skipped NULL value")
    return(NULL)
  }
  dbAddAuthorPublications(x)
})

test <- filter_PMID(authorPublications, "dhier", remove = T)
