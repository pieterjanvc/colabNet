

# Test simpleText()
test_that("simpleText", {
  expect_equal(simpleText("résumé"), "resume")
  expect_equal(simpleText("België"), "Belgie")
})


# Test getAuthor()
test_that("getAuthor", {
  expect_equal(getAuthor("Pieter-Jan", 'Van Camp'), "Van Camp PJ")
})
