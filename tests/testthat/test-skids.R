context("process skid specifications")

test_that("process numeric skids", {
  expect_equal(catmaid_skids(1L), 1L)
  expect_equal(catmaid_skids(2.0), 2L)
  expect_equal(catmaid_skids("3"), 3L)
  expect_equal(catmaid_skids(as.character(1:3)), 1:3)
  expect_error(catmaid_skids(1:3, several.ok = F))
  expect_error(catmaid_skids(as.character(1:3), several.ok = F))
})
