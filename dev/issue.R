# file.copy("../local/test.db", "../local/dev.db", overwrite = T)
# colabNetDB <- "../local/dev.db"

colabNetDB <- "D:/Desktop/dev.db"
file.remove(colabNetDB)
dbSetup(colabNetDB, checkSchema = T)

lastName = "Church"
firstName = "George M"

auth <- ncbi_author(lastName, firstName) |> filter(group == 1) |> slice(1)

aal <- ncbi_authorArticleList(auth$lastName, auth$firstName, returnHistory = T)

pubDet <- ncbi_publicationDetails(
  aal$PMID,
  lastName = auth$lastName,
  firstName = auth$firstName,
  initials = auth$initials,
  history = aal$history)

test <- filter_affiliation(pubDet, "Harvard")
