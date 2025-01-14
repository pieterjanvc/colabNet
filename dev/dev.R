# Create a database to work with

# Install package for additional testing
# devtools::install()
# library("colabNet")

firstName <- "PJ"
lastName <- "Van Camp"

firstName <- "Lorenzo"
lastName <- "Gesuita"

dbSetup("dev/colabNet.db", checkSchema = T)

authorPublications <- ncbi_authorPublications(firstName, lastName)
result <- dbAddAuthorPublications(conn, authorPublications)

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


plotData <- diffTree
# Highlight mesh Terms that are shared between authors
plotData <- plotData |> mutate(
  meshterm = ifelse(auID == 0, sprintf("<b>%s</b>", meshterm), meshterm)
)

# If the same MeSH term is used in different branches it will cause an error in the treeplot
# Rename duplicates to make them unique
plotData <- plotData |>
  group_by(meshterm) |>
  mutate(dupID = cur_group_id(), duplicate = n() > 1) |>
  ungroup()

plotData <- bind_rows(
  plotData |> filter(!duplicate),
  plotData |> filter(duplicate) |> group_by(dupID) |>
    mutate(meshterm = sprintf("%s(%i)", meshterm, 1:n())) |> ungroup()
)

# Add the parent MeSH term
plotData <- plotData |>
  left_join(
    plotData |> select(parent = treenum, parentMeshterm = meshterm),
    by = "parent"
  ) |>
  mutate(parentMeshterm = ifelse(is.na(parentMeshterm), "", parentMeshterm))


# Merge branches with only one child into a single node
plotData <- plotData |>
  group_by(branchID) |>
  summarise(
    parentBranchID = min(parentBranchID),
    treenum = paste(treenum, collapse = " -> <br>"),
    meshterm = paste(meshterm, collapse = " -> <br>"),
    auID = paste(auID, collapse = ","),
    .groups = "drop"
  )

# Get the new parent info for the collapsed data and the root
plotData <- plotData |>
  left_join(
    plotData |> select(parent = treenum, parentBranchID = branchID, parentMeshterm = meshterm),
    by = "parentBranchID"
  ) |>
  mutate(
    parentMeshterm = ifelse(is.na(parentMeshterm), "MeSH Tree", parentMeshterm),
    root = str_extract(plotData$treenum, "^[^\\.\\s]+")
  )

# library(RColorBrewer)
# generate_distinct_colors <- function(n) {
#   # Load RColorBrewer library
#   if (!require(RColorBrewer)) {
#     install.packages("RColorBrewer")
#     library(RColorBrewer)
#   }

#   # Use a predefined color palette to get distinct colors
#   if (n <= 12) {
#     return(brewer.pal(n, "Set3"))
#   } else {
#     # Use a custom palette for n > 12
#     return(colorRampPalette(brewer.pal(12, "Set3"))(n))
#   }
# }

# colours <- data.frame(root = str_extract(plotData$treenum, "^[^\\.\\s]+") |> unique()) |>
#   mutate(colour = generate_distinct_colors(n()))

# The first colour is the one where authors share a MeSH terms the other two their unique ones
colours <- plotData |>
  select(auID) |>
  distinct() |>
  arrange(auID) |>
  mutate(colour = c("#69BE28", "#3DB7E4", "#FF8849")[1:n()])

plotData <- plotData |> left_join(colours, by = "auID")

# Meshterm plot (for app)
fig <- plotly::plot_ly(
  type = "treemap",
  labels = plotData$meshterm,
  parents = plotData$parentMeshterm,
  marker = list(colors = plotData$colour),
  textfont = list(
    color = ifelse(plotData$colour == "#3DB7E4", "white", "black")
  ),
  maxdepth = -1
)
fig

htmlwidgets::saveWidget(fig, "D:/Desktop/PJ-Lorenzo.html")


# Treenum plot (to check logic)
fig <- plotly::plot_ly(
  type = "treemap",
  labels = plotData$treenum,
  parents = plotData$parent,
  maxdepth = -1
)
fig
