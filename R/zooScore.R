#' Generate a matrix for a node in the graph with author paper info
#'  This function is used inside of nodeEval()
#'
#' @param auID A vector of author IDs for the node of interest
#' @param allID A vector of all authors IDs (even if no papers for this node)
#' @param n The number of papers per author (in oder of auID)
#' @param m Matrix (n x 2) with all pairs of auIDs
#'
#' @return Paper count matrix for each author pair for the given node
#'
paperMat <- function(auID, allID, n, m) {
  # Fill in missing authors with 0 papers
  missing <- setdiff(allID, auID)
  auID <- c(auID, missing)
  n <- c(n, rep(0, length(missing)))

  # Order by ID for easier merge later
  o <- order(auID)
  auID <- auID[o]
  n <- n[o]

  # Create the scoring array
  # Cols 1-2 are author ID combinations (m)
  # Col 3-4 are number of papers for each pair, 5 is the score
  cbind(
    m,
    n[match(m[, 1], auID)],
    n[match(m[, 2], auID)],
    0
  )
}


#' Function to calculate node score and remainder to be pushed up to parent
#'  This function is used inside of zooScore_tree()
#'
#' @param lvlData Data frame with tree info for the current level (see zooScore_tree)
#' @param allID A vector of all authors IDs (even if no papers for this node)
#' @param carry The papers carried over from child nodes. These were not used to
#' score at the previous level and can now be used at the current level with lower score
#' @param carryOrder Order of node IDs in carry, needed for proper matching
#' @param m Matrix (n x 2) with all pairs of auIDs
#'
#' @return A matrix (n x 5)
#'  - Cols 1 & 2 are all auID combinations
#'  - Cols 3 & 4 are remaining papers for au1 and au2, resp to be carrier over
#'  - Col 5 is the score for the author pair for this level (i.e. number of overlapping papers)
#'
nodeEval <- function(lvlData, allID, carry, carryOrder, m) {
  # Get a 3d array of paper matrices for each node in this level
  a <- lvlData |>
    select(auID, mtrID, nPapers) |>
    group_by(mtrID) |>
    group_map(~ paperMat(.x$auID, allID, .x$nPapers, m)) |>
    simplify2array()

  # Add any remainders from previous levels
  if (length(carry) > 0) {
    # Make sure the order is the same for children carrying on to parents
    mtrIDs <- lvlData$mtrID |> unique() |> sort()
    a[, 3:4, mtrIDs %in% carryOrder] <- a[,
      3:4,
      mtrIDs %in% carryOrder,
      drop = F
    ] +
      carry[, 3:4, order(carryOrder), drop = F]
  }

  #Update Score (col 5) for each pair
  a[, 5, ] <- (a[, 3, ] + a[, 4, ] - abs(a[, 3, ] - a[, 4, ])) / 2

  # Update col 3-4 with remainder to carry over
  a[, 3, ] <- a[, 3, ] - a[, 5, ]
  a[, 4, ] <- a[, 4, ] - a[, 5, ]

  # if (dim(a)[3] == 1) {
  #   # If only one element in 3rd dim, slightly different operation needed
  #   a[, 3:4, ] <- a[, 3:4, , drop = F] -
  #     array(matrix(rep(a[, 5, ], 2), ncol = 2), dim = c(dim(a)[1], 2, 1))
  # } else {
  #   a[, 3:4, ] <- a[, 3:4, ] -
  #     array(
  #       a[, 5, ][, rep(1:dim(a)[3], each = 2)],
  #       dim = c(dim(a)[1], 2, dim(a)[3])
  #     )
  # }

  return(a)
}

#' Calculate the zoo scores for all author combos in a single tree
#'  This function is used inside of zooScore()
#'
#' @param tree A single Mesh Tree (one root) generated as part of the
#' paperMeshTree() function (generates all trees)
#'
#' @return A data frame with 3 columns
#' - Cols 1 & 2 are all auID combinations
#' - Col 3 is the zoo score
#'
zooScore_tree <- function(tree) {
  auIDs <- sort(unique(tree$auID))

  # Not enough data to build a tree
  if (length(auIDs) < 2) {
    return(NULL)
  }

  # Setup the run
  finalScore <- combn(auIDs, 2) |> t()
  m <- finalScore # This will prevent need for recalculation below
  finalScore <- cbind(finalScore, 0)
  colnames(finalScore) <- c("au1", "au2", "score")

  # Start at leaves
  curLvl <- max(tree$level)
  result <- NULL
  carryOrder <- NULL

  # Process the tree level by level for all author pairs
  while (curLvl > 0) {
    # Get data for all nodes at lowest current level
    lvlData <- tree |>
      filter(level == curLvl)

    currentNodes <- lvlData |>
      select(mtrID, parent) |>
      distinct()

    result <- nodeEval(
      lvlData = lvlData,
      allID = auIDs,
      carry = result,
      carryOrder = carryOrder,
      m = m
    )

    # If there are multiple nodes feeding into the same parent, merge them
    #  this cannot happen when you are at the root
    if (curLvl > 1) {
      result <- lapply(currentNodes$parent |> unique(), function(parent) {
        matches <- which(currentNodes$parent == parent)

        if (length(matches) == 1) {
          x <- array(result[,, matches], dim = c(dim(result)[1], 5))
          return(x)
        }

        matches <- result[,, matches, drop = F]

        cbind(
          array(matches[, 1:2, 1], dim = c(dim(matches)[1], 2)),
          Reduce(
            `+`,
            apply(
              matches,
              3,
              function(m) {
                m[, 3:5, drop = F]
              },
              simplify = F
            )
          )
        )
      }) |>
        simplify2array()
    }

    carryOrder <- currentNodes$parent |> unique()

    # Add the level scores to the final score
    finalScore[, "score"] <- finalScore[, "score"] +
      rowSums(result[, 5, , drop = F]) * curLvl

    # Next level
    curLvl <- curLvl - 1
  }

  as.data.frame(finalScore)
}

#' Calculate the zoo scores for a paper MeSH tree
#'
#' @param papermeshtree A Mesh Tree generated by paperMeshTree()
#' @param roots Optional. If set, a vector of roots in paperMeshTree to run the
#' algorithm for, otherwise all roots (i.e. trees) are used
#'
#' @return A data frame with 3 columns
#' - Cols 1 & 2 are all auID combinations
#' - Col 3 is the zoo score
#'
#' @export
zooScore <- function(papermeshtree, roots = NULL) {
  # We need at least two authors
  if (n_distinct(papermeshtree$auID) < 2) {
    return(data.frame(
      tree = character(),
      au1 = integer(),
      au2 = integer(),
      score = numeric()
    ))
  }

  # Check which roots to use
  if (length(roots) == 0) {
    roots <- setNames(unique(papermeshtree$root), unique(papermeshtree$root))
  } else {
    check <- setdiff(
      roots,
      setNames(unique(papermeshtree$root), unique(papermeshtree$root))
    )

    if (length(check) > 0) {
      stop(
        "The following specified roots are not part of the papermeshtree: ",
        paste(check, sep = ", ")
      )
    }
  }

  # Get results for each tree (if multiple roots)
  map_df(
    roots,
    function(root) {
      zooScore_tree(tree = papermeshtree |> filter(root == {{ root }}))
    },
    .id = "tree"
  )
}
