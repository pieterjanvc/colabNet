# simpleText()
test_that("simpleText", {
  expect_equal(simpleText("résumé"), "resume")
  expect_equal(simpleText("België"), "Belgie")
  expect_equal(simpleText("Hello"), "Hello")
  expect_failure(expect_equal(simpleText("résumé"), "résumé"))
})


# Test checkTreeNums()
test_that("checkTreeNums", {
  treenums <- c("G17.035", "G17.800.500", "G02.111.873.750")
  expect_equal(checkTreeNums(treenums), NULL)
  expect_equal(checkTreeNums(treenums, output = "vector"), c(T, T, T))
  expect_equal(checkTreeNums(treenums, output = "bool"), T)

  treenums <- c("G17.035", "G17.80.500", "G02.111.873.750")
  expect_equal(checkTreeNums(treenums, output = "vector"), c(T, F, T))
  expect_equal(checkTreeNums(treenums, output = "bool"), F)
  expect_error(checkTreeNums(treenums))
})

# Test missingTreeNums()
test_that("missingTreeNums", {
  treenums <- c("G17.035", "G17.800.500", "G02.111.873.750")
  a <- missingTreeNums(treenums)
  b <- c("G17", "G17.800", "G02.111.873", "G02.111", "G02")
  expect_equal(a, b)

  treenums <- c("G17")
  a <- missingTreeNums(treenums)
  b <- NULL
  expect_equal(a, b)

  treenums <- c()
  a <- missingTreeNums(treenums)
  b <- NULL
  expect_equal(a, b)
})
