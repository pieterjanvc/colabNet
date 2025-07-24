file.copy("data/PGG_dev.db", "local/dev.db", overwrite = T)
colabNetDB <- "local/dev.db"

# colabNetDB <- "D:/Desktop/dev.db"
# file.remove(colabNetDB)

dbSetup(colabNetDB, checkSchema = T)

pool <- dbGetConn()

# dbDisconnect(pool)

# articleInfo <- allArticles

# Create time dataframe
library(ggplot2)

trendInfo <- function(articleInfo, windowSize = 5) {
  trends <- articleInfo

  # Calculate network stats for each window
  trends <- lapply(min(trends$year):max(trends$year), function(curYear) {
    trends <- trends |>
      filter(between(
        year,
        max(min(trends$year), curYear - windowSize),
        curYear
      ))

    graphElements <- copubGraphElements(trends)
    graphStats <- copubGraphStats(graphElements)

    data.frame(
      year = curYear,
      nAuthors = graphStats$nAuthors,
      nCopubs_avg = graphStats$authorStats$nCopubs |> mean(),
      nColabs_avg = graphStats$authorStats$degree |> mean(),
      largestComp = graphStats$components$largest_n,
      unconnected = graphStats$nUnconnected,
      distance_avg = graphStats$distance_avg,
      diameter = graphStats$diameter,
      density = graphStats$density,
      globalEfficiency = graphStats$globalEffiency,
      transitivity = graphStats$transitivity
    )
  }) |>
    bind_rows() |>
    mutate(
      # The first years the sliding window is not complete and the last year
      # is possible having missing data (year not compelte yet)
      fullWindow = ((year - windowSize) >= min(year)) &
        year < max(year) - 1,
      unconnected_perc = unconnected / nAuthors,
      # Diameter and distance need to be adjusted by network size
      diameter_adj = diameter / log(nAuthors),
      distance_avg_adj = distance_avg / log(nAuthors)
    )

  # Convert the data into a long format for plotting
  plotData <- trends |>
    select(
      year,
      fullWindow,
      nCopubs_avg,
      nColabs_avg,
      largestComp,
      density,
      globalEfficiency,
      transitivity,
      unconnected,
      diameter_adj,
      distance_avg_adj
    ) |>
    pivot_longer(cols = -c(year, fullWindow))

  # Set the labels for the columns (will become facets)
  facet_labels <- c(
    nCopubs_avg = "Average co-publications",
    nColabs_avg = "Average collaborators",
    largestComp = "Size of largest graph component",
    density = "Density",
    globalEfficiency = "Global efficiency",
    transitivity = "Transitivity",
    unconnected = "Authors with no co-publications",
    diameter_adj = "Size adjusted diameter",
    distance_avg_adj = "Distance adjusted distance"
  )

  # Generate the ggplot
  plot <- ggplot(plotData, aes(x = year, y = value)) +
    geom_line(aes(color = fullWindow, group = 1)) +
    scale_color_manual(values = c("TRUE" = "#4791e5", "FALSE" = "#E59B47")) +
    facet_wrap(
      ~name,
      scales = "free_y",
      labeller = labeller(name = facet_labels)
    ) +
    labs(
      title = sprintf(
        "Co-publication network statistics with %s year sliding window",
        windowSize
      ),
      color = "Full 5 year data"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.box = "horizontal")

  return(list(trends = trends, plot = plot))
}
