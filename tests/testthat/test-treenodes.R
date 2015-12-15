context("treenode queries")

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("read.neuron(s).catmaid and connectors", {
  if(!inherits(conn, 'try-error')){
    expect_is(tnt<-catmaid_get_treenode_table(10418394), 'data.frame')
    # check that we have appropriate columns
    expect_true(all(c("x","y","z") %in% colnames(tnt)))
  }
})
