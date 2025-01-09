# 1 - Create blank database

firstName = "PJ"
lastName = "Van Camp"

firstName = "Lorenzo"
lastName = "Gesuita"

conn <- dbGetConn("dev/colabNet.db", checkSchema = T)

authorPublications <- ncbi_authorPublications(firstName, lastName)
result <- dbAddAuthorPublications(conn, authorPublications)
