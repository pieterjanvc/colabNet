file.copy("data/dbmi.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"

devtools::install_github("pieterjanvc/sqlife", ref = "v0.1.3")
colabNetDB <- "D:/Desktop/newCN.db"
file.remove(colabNetDB)

sqlife::dbSetup(colabNetDB, schema = "../sqlife/inst/example.sql")

sqlife::dbNewFromSchema(colabNetDB, schema = "inst/create_colabNetDB.sql")
