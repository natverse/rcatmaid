context("catmaid annotation queries")

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("catmaid_get_annotations_for_skeletons", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  
  expect_is(n1<-catmaid_get_annotations_for_skeletons(skids=c(10418394)), 'data.frame')
  n2=catmaid_get_annotations_for_skeletons(skids=c(4453485))
  expect_is(n12 <- catmaid_get_annotations_for_skeletons(skids=c(10418394,4453485)), 'data.frame')
  expect_equal(rbind(n1,n2), n12)
})
