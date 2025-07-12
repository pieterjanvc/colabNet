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


test |>
  group_by(id = auID) |>
  summarise(
    label = sprintf("%s %s", lastName[1], firstName[1]),
    year = min(year),
    month = min(month),
    .groups = "drop"
  )
