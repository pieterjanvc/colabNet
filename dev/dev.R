file.copy("data/dbmi.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"

# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()

author <- ncbi_author("Cai", "Tianxi")

test <- ncbi_publicationDetails(
  PMIDs = "39755324",
  lastName = author$lastName,
  firstName = author$firstName,
  initials = author$initials
)
