context("process skid specifications")

test_that("process numeric skids", {
  expect_equal(catmaid_skids(1L), 1L)
  expect_equal(catmaid_skids(2.0), 2L)
  expect_equal(catmaid_skids("3"), 3L)
  expect_equal(catmaid_skids(as.character(1:3)), 1:3)
  expect_error(catmaid_skids(1:3, several.ok = F))
  expect_error(catmaid_skids(as.character(1:3), several.ok = F))
})

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("process skid queries", {
  if(!inherits(conn, 'try-error')){
    expect_is(catmaid_skids("name:ORN"), "integer")
    expect_is(catmaid_skids("annotation:^ORN PNs$"), "integer")
  }
})
