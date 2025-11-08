file.copy("data/dbmi.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"

devtools::install_github("pieterjanvc/sqlife", ref = "expandConnections")
colabNetDB <- "D:/Desktop/newCN.db"
file.remove(colabNetDB)

sqlife::dbSetup(colabNetDB, schema = "../sqlife/inst/example.sql")

dbNewFromSchema("local/test.db", schema = "inst/create_colabNetDB.sql")


result <- entrezSearch(
  "pubmed",
  term = sprintf(
    '"%s %s"[Author]',
    "Van Camp",
    "PJ"
  ),
  retmax = 10
)
