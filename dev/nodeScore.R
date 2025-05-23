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
  # Cols 1-2 are author ID combinations
  # m <- combn(auID, 2) |> t()
  # Col 3-4 are number of papers for each pair, 5 is the score
  cbind(
    m,
    n[match(m[, 1], auID)],
    n[match(m[, 2], auID)],
    0
  )
}

# Function to calculate node score and remainder to be pushed up to parent
nodeEval2 <- function(lvlData, allID, carry, carryOrder, m) {
  # a <- array(finalScore, dim = c(dim(finalScore), 5))
  # dimnames(test) <- list(NULL, c("au1", "au2", "score"), LETTERS[1:5])

  # auID = lvlData |> filter(mtrID %in% currentNodes$mtrID) |> pull(auID),
  # n = lvlData |> filter(mtrID %in% currentNodes$mtrID) |> pull(nPapers),

  a <- lvlData |>
    select(auID, mtrID, nPapers) |>
    group_by(mtrID) |>
    group_map(~ paperMat(.x$auID, allID, .x$nPapers, m)) |>
    simplify2array()

  # Add any remainders from previous levels
  if (length(carry) > 0) {
    # a[, 3:4, ] <- a[, 3:4, ] + carry[, 3:4, , ]
    # Make sure the order is the same fro children carrying on to parents
    mtrIDs <- lvlData$mtrID |> unique() |> sort()
    a[, 3:4, mtrIDs %in% carryOrder] <- a[,
      3:4,
      mtrIDs %in% carryOrder,
      drop = F
    ] +
      carry[, 3:4, order(carryOrder), drop = F]
  }

  #Update Score (col 5) for each pair
  # check1 <- apply(
  #   a[, 3:4, , drop = F],
  #   3,
  #   function(m) {
  #     apply(matrix(m, ncol = 2), 1, min)
  #   },
  #   simplify = F
  # ) |>
  #   simplify2array()

  check2 <- (a[, 3, ] + a[, 4, ] - abs(a[, 3, ] - a[, 4, ])) / 2

  # if(!all(check1 == check2)) stop()

  a[, 5, ] <- check2
  # Update col 3-4 with remainder

  # a <- array(c(10:1, 11:20), dim = c(2,5,2))
  #a <- array(c(10:1), dim = c(2,5,1))
  # check1 <- apply(
  #   a,
  #   3,
  #   function(m) {
  #     m[, 3:4] - m[, 5]
  #   },
  #   simplify = F
  # ) |>
  #   simplify2array()

  if (dim(a)[3] == 1) {
    check2 <- a[, 3:4, , drop = F] -
      array(matrix(rep(a[, 5, ], 2), ncol = 2), dim = c(dim(a)[1], 2, 1))
  } else {
    check2 <- a[, 3:4, ] -
      array(
        a[, 5, ][, rep(1:dim(a)[3], each = 2)],
        dim = c(dim(a)[1], 2, dim(a)[3])
      )
  }

  # if(!all(check1 == check2)) stop()
  a[, 3:4, ] <- check2

  # # Extract the reference values (column 5), shape: [n1, 1, n2]
  # ref <- a[, 5, , drop = FALSE]
  #
  # # Replicate along the 2nd dimension to shape [n1, 2, n2]
  # ref_rep <- array(rep(ref, times = 2), dim = c(dim(a)[1], 2, dim(a)[3]))
  #
  # # Subtract and assign
  # all(test == (a[, 3:4, , drop = FALSE] - ref_rep))

  return(a)
}

zooScore_tree2 <- function(tree, auIDs) {
  # Not enough data to build a tree
  if (n_distinct(tree$auID) < 2) {
    return(NULL)
  }

  # Setup the run
  finalScore <- combn(auIDs, 2) |> t()
  m <- finalScore
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

    result <- nodeEval2(
      lvlData = lvlData,
      allID = auIDs,
      carry = result,
      carryOrder = carryOrder,
      m = m
    )

    # Run the node eval function for all nodes
    # result <- map(currentNodes$mtrID, function(mtrID) {
    #   print(result[[as.character(mtrID)]])
    #   nodeEval(
    #     auID = lvlData$auID[lvlData$mtrID == mtrID],
    #     n = lvlData$nPapers[lvlData$mtrID == mtrID],
    #     allID = auIDs,
    #     carry = result[[as.character(mtrID)]]
    #   )
    # })

    # If there are multiple nodes feeding into the same parent, merge them
    #  this cannot happen when you are at the root
    if (curLvl > 1) {
      result <- lapply(currentNodes$parent |> unique(), function(parent) {
        matches <- which(currentNodes$parent == parent)

        if (length(matches) == 1) {
          return(result[,, matches])
        }

        matches <- result[,, matches]

        cbind(
          matches[, 1:2, 1],
          Reduce(
            `+`,
            apply(
              matches,
              3,
              function(m) {
                m[, 3:5]
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

zooScore2 <- function(papermeshtree) {
  roots <- setNames(unique(papermeshtree$root), unique(papermeshtree$root))
  auIDs <- sort(unique(papermeshtree$auID))
  map_df(
    roots,
    function(root) {
      zooScore_tree2(tree = papermeshtree |> filter(root == {{ root }}), auIDs)
    },
    .id = "tree"
  )
}

profvisRender <- function(expr) {
  test <- profvis::profvis(
    rlang::eval_tidy(rlang::enquo(expr)),
    prof_output = "D:/Desktop/proftest.Rprofvis"
  )

  htmlwidgets::saveWidget(test, "D:/Desktop/proftest.html")
  browseURL("D:/Desktop/proftest.html")
}
