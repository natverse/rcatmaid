context("nat")

test_that("copy fields", {
  lhn=readRDS('testdata/lhn.rds')
  expect_equal(copy_tags_connectors(new = lhn, old = lhn), lhn)
  
  r=resample(lhn, 1000)
  expect_equal(copy_tags_connectors(new = r, old = lhn, update_node_ids = FALSE)$tags, lhn$tags)

  
  expect_is(r2 <- copy_tags_connectors(new = r, old = lhn), 'catmaidneuron')
})
