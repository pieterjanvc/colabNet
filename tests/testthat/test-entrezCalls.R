#**********************************************************
#* All these functions require NCBI API access with ENTREZ
#**********************************************************

# Test getAuthor()
test_that("ncbi_author", {
  expect_equal(ncbi_author("Pieter-Jan", "Van Camp"), "Van Camp PJ")
})
