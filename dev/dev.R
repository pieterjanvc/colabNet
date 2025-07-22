file.copy("data/PGG_dev.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"

# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()

articleInfo <- allArticles

## Get the nodes and edges
nodes <- articleInfo |>
  group_by(id = auID) |>
  summarise(
    label = sprintf("%s %s", lastName[1], firstName[1]),
    year = min(year),
    month = min(month),
    .groups = "drop"
  )

edges <- articleInfo |>
  group_by(arID, year, month) |>
  filter(n() > 1)

if (nrow(edges) == 0) {
  edges <- data.frame(
    id = integer(),
    from = integer(),
    to = integer(),
    weight = integer()
  )
} else {
  edges <- edges |>
    reframe(as.data.frame(combn(auID, 2) |> t())) |>
    rename(from = V1, to = V2) |>
    group_by(from, to) |>
    summarise(id = cur_group_id(), weight = n(), .groups = "drop")
}

### Get the graph stats
colabGraphStats <- function(nodes, egdes) {
  # Create an igraph graph object
  g <- graph_from_data_frame(
    d = edges |> select(from, to, weight),
    vertices = nodes$id,
    directed = F
  )

  statsTable <- data.frame(
    id = sort(nodes$id)
  )

  # Total number of authors
  nAuthors <- nrow(nodes)

  # Collaborations per author
  colabs <- rbind(
    edges |> select(from, to, weight),
    edges |> select(from = to, to = from, weight)
  ) |>
    group_by(id = from) |>
    summarise(nColabs = sum(weight), .groups = "drop")

  colabs <- rbind(
    colabs,
    data.frame(id = nodes$id[!nodes$id %in% colabs$id], nColabs = 0)
  ) |>
    mutate(
      colabPerc = nColabs / sum(nColabs) * 100
    )

  statsTable <- statsTable |> left_join(colabs, by = "id")

  # How many others have you published with
  deg <- degree(g)
  deg <- data.frame(id = as.integer(names(deg)), degree = unname(deg))

  statsTable <- statsTable |> left_join(deg, by = "id")

  # Check how many sub-graphs there are and who belongs to which
  # Can be used to calculate
  comp <- components(g)
  comp$membership <- data.frame(
    id = as.integer(names(comp$membership)),
    membership = unname(comp$membership)
  )
  comp$largest_n = max(comp$csize)
  comp$largest_perc = max(comp$csize) / sum(comp$csize) * 100

  statsTable <- statsTable |>
    left_join(comp$membership, by = "id") |>
    mutate(unconnected = colabPerc == 0)

  # Check how many are not connected to anyone
  unconnected <- sum(comp$csize == 1)

  # Distance matrix between authors (Inf = unconnected)
  dis <- distances(g, algorithm = "unweighted", weights = NA)

  # The graph diameters (longest distance between authors who are connected)
  diam <- max(dis[!is.infinite(dis)])

  # How close is the graph to be being fully connected
  #  i.e. every author shares at least one publication with every other
  dens <- edge_density(g)

  # How often does an author lie on shortest path between two others (i.e. connects them)
  betw <- betweenness(g, directed = F, weights = NA)
  betw <- data.frame(id = as.integer(names(betw)), betweenness = unname(betw))
  statsTable <- statsTable |> left_join(betw, by = "id")

  # How how close an author is to all others in the network (normalised)
  closeNorm <- closeness(g, weights = NA, normalized = T)
  closeNorm <- data.frame(
    id = as.integer(names(closeNorm)),
    closeness = unname(closeNorm)
  )
  statsTable <- statsTable |> left_join(closeNorm, by = "id")

  # Global efficiency
  glob <- global_efficiency(g, weights = NA, directed = F)

  # Probability that adjecent nodes (authors are connected)
  # https://transportgeography.org/contents/methods/graph-theory-measures-indices/transitivity-graph/
  trans <- transitivity(g)

  return(list(
    authorStats = statsTable,
    nAuthors = nAuthors,
    components = comp,
    nUnconnected = unconnected,
    distances = dis,
    diameter = diam,
    density = dens,
    globalEffiency = glob,
    transitivity = trans
  ))
}

test <- colabGraphStats(nodes, edges)

copubGraphStats(
  auIDs = nodes$id,
  copub = edges |> select(au1 = from, au2 = to, n = weight)
)
