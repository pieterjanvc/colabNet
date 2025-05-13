dbSetup("data/alumni.db", checkSchema = T)

# Load student data
alumni <- readr::read_csv("data/PGG alumni.csv")
# Filter for articles where the author of interest has Harvard in their affiliations
affiliationFilter <- "[Hh]arvard"

lastName = ""
firstName = ""

i = 2
# Strict search for alumni based on last and first name (if they have a middle name
#  the initial must be after first name)
authorPublications_all <- lapply(1:96, function(i) {
  firstName = alumni$First_name[i]
  lastName = alumni$Last_name[i]
  print(paste(firstName, lastName))
  authorinfo <- ncbi_author(lastName, firstName)

  if (n_distinct(authorinfo$group) > 1) {
    authorinfo <- authorinfo[1, ]
    warning(sprintf(
      "Multiple matches for %s, %s [%i]. Best match: %s, %s",
      lastName,
      firstName,
      i,
      authorinfo$lastName,
      authorinfo$firstName
    ))
    # return(NULL)
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

  # publicationDetails <- authorPublications
  # regex <- affiliationFilter
  # includeMissing <- T
  authorPublications <- ncbi_publicationDetails(
    PMIDs = result$PMID,
    lastName = authorinfo$lastName[1],
    firstName = authorinfo$firstName[1],
    initials = authorinfo$initials[1],
    history = result$history
  ) |>
    filter_affiliation(affiliationFilter, includeMissing = T)

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

# authorPublications_all <- authorPublications_all[!sapply(authorPublications_all, is.null)]
# saveRDS(authorPublications_all, "data/alumni.rds")
authorPublications_all <- readRDS("data/alumni.rds")

toKeep <- (1:nrow(alumni))[!sapply(authorPublications_all, is.null)]

authorPublications_all <- authorPublications_all[
  !sapply(authorPublications_all, is.null)
]

toKeep <- toKeep[sapply(sapply(authorPublications_all, "[", 1), nrow) == 1]

authorPublications_all <- authorPublications_all[
  sapply(sapply(authorPublications_all, "[", 1), nrow) == 1
]

#alum <- authorPublications_all[[1]]
result = map_df(
  authorPublications_all,
  function(alum) {
    cbind(
      data.frame(
        lastName = alum$author$lastName,
        firstName = alum$author$firstName
      ),
      alum$articles |>
        left_join(
          alum$coAuthors |>
            group_by(PMID) |>
            filter(
              authorOrder == max(authorOrder) | tempId == alum$author$tempId
            ) |>
            summarise(
              authorPosition = authorOrder[tempId == alum$author$tempId],
              seniorAuthorFirstName = firstName[
                authorOrder == max(authorOrder)
              ],
              seniorAuthorLastName = lastName[authorOrder == max(authorOrder)],
              .groups = "drop"
            ),
          by = "PMID"
        )
    )
  },
  .id = "x"
) |>
  left_join(
    data.frame(x = as.character(1:length(authorPublications_all)), id = toKeep),
    by = "x"
  ) |>
  left_join(
    alumni |> mutate(id = 1:n()) |> select(id, Graduation_year, Lab_advisor),
    by = "id"
  ) |>
  select(-c(x, id)) |>
  mutate(month = as.integer(month))

readr::write_csv(result, "data/alumni_papers.csv")
