# Create a database to work with

# Install package for additional testing
# devtools::install()
# library("colabNet")

dbSetup("dev/colabNet.db", checkSchema = T)

firstName <- "PJ"
lastName <- "Van Camp"

firstName <- "Lorenzo"
lastName <- "Gesuita"

firstName <- "David"
lastName <- "Van Vactor"

authorPublications <- ncbi_authorPublications(firstName, lastName)
result <- dbAddAuthorPublications(authorPublications)

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

pData <- diffTree(1, 31, pruneDuplicates = T)

plotDiffTree(pData, colours = c("#69BE28", "#3DB7E4", "#4c248d"))
