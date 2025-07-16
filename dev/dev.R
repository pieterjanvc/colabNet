file.copy("data/PGG_dev.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"


# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()


edges <- test$edges |>
  group_by(from, to) |>
  summarise(weight = n(), .groups = "drop")

library(igraph)


g <- graph_from_data_frame(
  d = edges,
  vertices = test$nodes,
  directed = F
)

plot(g, edge.width = E(g)$weight)

# How many others have you published with
deg <- degree(g)

# How many publications you have with someone
edges$weight

# Check how many sub-graphs there are and who belongs to which
comp <- components(g)

# Check how many are not connected to anyone
sum(comp$csize == 1)

# Distance matrix between authors
dis <- distances(g, algorithm = "unweighted")
#  average distance, ignoring unconnected
dis_avg <- dis[upper.tri(dis)]
dis_avg[!is.infinite(dis_avg)] |> mean()

# How close is the graph to be being fully connected
#  i.e. every author shares at least one publication with every other
edge_density(g)

# Probability that adjacent nodes (authors are connected)
# https://transportgeography.org/contents/methods/graph-theory-measures-indices/transitivity-graph/
transitivity(g)


plotData |>
  group_by(colourCode) |>
  mutate(
    colour = treemapColour(
      meshSum,
      minCol = lightenColour(colSel[colourCode[1]], 0.9),
      maxCol = colSel[colourCode[1]]
    )
  ) |>
  ungroup()

test <- plotData |> filter(colourCode == 2)
treemapColour(
  test$meshSum,
  minCol = lightenColour(colSel[test$colourCode[1]], 0.9),
  maxCol = colSel[test$colourCode[1]]
)
