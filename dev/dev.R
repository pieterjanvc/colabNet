# Create a database to work with

# Install package for additional testing
# devtools::install()
# library("colabNet")

devtools::load_all()
dbSetup("dev/colabNet.db", checkSchema = T)

authors = data.frame(
  firstName = c("PJ", "Lorenzo", "Cristina", "Irene", "Grey", "Kayla", "Lauren"),
  lastName = c("Van Camp", "Gesuita","Deoliveira", "Wong", 'Kuling', "Nygaard", "Essler")
)

i =  1

for(i in 1:nrow(authors)){  
  firstName = authors$firstName[i]
  if(firstName == "Irene") {next}
  lastName = authors$lastName[i]
  authorPublications <- ncbi_authorPublications(firstName, lastName)
  result <- dbAddAuthorPublications(authorPublications)
  print(sprintf("%s added", authors$firstName[i]))
}

# auIDs = c(1,31,75)

# Find overlap between authors based on MeSH terms of their research papers

meshRoots <- data.frame(
  treenum = c(LETTERS[1:14], "V", "Z"),
  meshterm = c(
    "Anatomy",
    "Organisms",
    "Diseases",
    "Chemicals and Drugs",
    "Analytical, Diagnostic and Therapeutic Techniques, and Equipment",
    "Psychiatry and Psychology",
    "Phenomena and Processes",
    "Disciplines and Occupations",
    "Anthropology, Education, Sociology, and Social Phenomena",
    "Technology, Industry, and Agriculture",
    "Humanities",
    "Information Science",
    "Named Groups",
    "Health Care",
    "Publication Characteristics",
    "Geographicals"
  )
)


amt1 <- authorMeshTree(1) # PJ
amt2 <- authorMeshTree(31) # Lorenzo


## GENERATE TREEMAP PLOT
# auIDs = c(1,31,69,100,137,163)
# issue with Essler 163
# row 138  - E01.370.225.500.607 -> <br>E01.370.225.500.607.512
auIDs <- tbl(dbGetConn(checkSchema = F), "author") |> 
    # TODO remove 163 exclusion once bug fixed!!
    filter(authorOfInterest == 1, auID != 163) |> 
    pull(auID)
difftree <- diffTree(auIDs, pruneDuplicates = T)

difftree <- diffTree(c(1,31), pruneDuplicates = T)

difftree |> filter(nAuth == 2) |> pull(level) |> sum()
