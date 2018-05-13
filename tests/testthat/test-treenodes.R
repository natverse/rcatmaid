context("treenode queries")

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("catmaid_get_treenode_table", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(tnt<-catmaid_get_treenode_table(10418394), 'data.frame')
  # check that we have appropriate columns
  expect_true(all(c("x","y","z") %in% colnames(tnt)))
})

test_that("catmaid_get_treenodes_detail", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(tn<-catmaid_get_treenodes_detail(c(9943214L, 25069047L, 12829015L)),
            'data.frame')
  # check that we have appropriate columns
  expect_equal(nrow(tn), 3L)
  expect_equal(colnames(tn), c("treenode_id", "parent_id", "x", "y", "z", 
                              "confidence", "radius", "skid", "edition_time", 
                              "user_id"))
})
