context('catmaid_selection')

test_that("Can read/write CATMAID selections", {
  skels=system.file("catmaid-skeletons.json", package = "catmaid")
  expect_is(skeldf<-read_catmaid_selection(skels, getNames = FALSE), 'data.frame')
  tf <- tempfile(fileext = '.json')
  write_catmaid_selection(skeldf, f = tf)
  expect_equal(readLines(skels, warn = F), readLines(tf))
})
