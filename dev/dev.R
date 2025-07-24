file.copy("data/PGG_dev.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"

# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()

# dbDisconnect(pool)

# articleInfo <- allArticles

branchID <- selected
filterIDs <- branchIDs

selected %in% branchIDs


id = treemap$branchID
parent = treemap$parentBranchID
colourCode = treemap$colourCode
leaves <- id[!id %in% parent]

shared[id %in% leaves] = status[id %in% leaves]


sharedCheck <- function(treemap) {
  sharedCheck <- treemap |>
    select(branchID, parentBranchID, colourCode) |>
    group_by(parentBranchID) |>
    mutate(shared = n_distinct(colourCode[colourCode != 1]) > 1) |>
    ungroup()

  sharedBranches <- sharedCheck |>
    filter(branchID %in% leaves) |>
    filter(shared) |>
    pull(parentBranchID)

  while (length(sharedBranches) > 0) {
    sharedCheck <- sharedCheck |>
      mutate(shared = shared | (branchID %in% sharedBranches))

    sharedBranches <- sharedCheck |>
      filter(branchID %in% sharedBranches) |>
      filter(shared) |>
      pull(parentBranchID) |>
      unique()
  }

  treemap$shared = sharedCheck$shared
  return(treemap)
}

test <- sharedCheck(treemap)


colourCode[id %in% leaves]
#Function to find the leave values
leafVal <- function(curID, curVal) {
  if (curID %in% leaves) {
    return(setNames(curID, colourCode[id == curID]))
  } else {
    ids <- id[parent == curID]
    ids <- ids[!is.na(ids)]
    newVal <- colourCode[id == curID]
    results <- sapply(ids, leafVal, curVal = newVal)
    return(results)
  }
}

test <- leafVal(0, 1)


sharedCheck <- function(treemap) {
  sharedCheck <- treemap |>
    select(branchID, parentBranchID, colourCode, nAuthors)

  parentBranchIDs <- sharedCheck$parentBranchID
  sharedBranches <- sharedCheck |>
    filter(!branchID %in% parentBranchIDs) |>
    group_by(branchID = parentBranchID) |>
    summarise(
      code = ifelse(
        n_distinct(colourCode[colourCode != 1]) > 1,
        1,
        colourCode[1]
      )
    )

  while (nrow(sharedBranches) > 0) {
    sharedCheck <- sharedCheck |>
      left_join(sharedBranches, by = "branchID") |>
      mutate(
        colourCode = ifelse(!is.na(code) & nAuthors == 0, code, colourCode)
      ) |>
      select(-code)

    sharedBranches <- sharedCheck |>
      filter(
        branchID %in% sharedBranches$branchID[!is.na(sharedBranches$code)]
      ) |>
      group_by(branchID = parentBranchID) |>
      summarise(
        code = ifelse(
          n_distinct(colourCode[colourCode != 1]) > 1,
          1,
          colourCode[1]
        )
      )
  }

  treemap$colourCode = sharedCheck$colourCode
  return(treemap)
}


test <- sharedCheck(treemap)

sharedCheck <- function(treemap) {
  sharedCheck <- treemap |>
    select(branchID, parentBranchID, colourCode, nAuthors, minLvl)

  for (lvl in max(sharedCheck$minLvl):1) {
    lvlInfo <- sharedCheck |>
      filter(minLvl == lvl) |>
      group_by(branchID = parentBranchID) |>
      summarise(
        code = case_when(
          any(colourCode == 1) ~ 1,
          n_distinct(colourCode) > 1 ~ 1,
          TRUE ~ colourCode[1]
        )
      )

    sharedCheck <- sharedCheck |>
      left_join(lvlInfo, by = "branchID") |>
      mutate(
        colourCode = case_when(
          is.na(code) ~ colourCode,
          nAuthors == 0 ~ code,
          colourCode != code ~ 1,
          TRUE ~ colourCode
        )
      ) |>
      select(-code)
  }

  treemap$colourCode = sharedCheck$colourCode
  return(treemap)
}
