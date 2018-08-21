context("test-skeleton.R")

conn=try(catmaid_login(), silent = TRUE)

test_that("", {
  if(inherits(conn, 'try-error'))
    skip('No catmaid connection')
  
  sk=catmaid_skids("ORN PNs")[1:2]
  expect_is(nc <- catmaid_get_node_count(sk), 'integer')
  sk2=c(rep(sk, 2), NA)
  nc2=c(rep(nc, 2), NA)
  # make sure that we can cope with duplicates and NAs
  expect_equal(catmaid_get_node_count(sk2), nc2)
})
