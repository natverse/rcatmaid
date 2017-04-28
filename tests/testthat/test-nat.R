context("nat")

test_that("copy fields", {
  lhn=readRDS('testdata/lhn.rds')
  expect_equal(copy_tags_connectors(new = lhn, old = lhn), lhn)
  
  r=resample(lhn, 1000)
  expect_equal(copy_tags_connectors(new = r, old = lhn, update_node_ids = FALSE)$tags, lhn$tags)

  
  expect_is(r2 <- copy_tags_connectors(new = r, old = lhn), 'catmaidneuron')
})

test_that("summary.catmaidneuron behaves", {
  lhn = readRDS('testdata/lhn.rds')
  lhn2=lhn
  class(lhn2)='neuron'
  expect_is(s1 <- summary(lhn), 'data.frame')
  expect_is(s2 <- summary(lhn2), 'data.frame')
  # check the initial summary columns are identical
  expect_equal(s1[seq_along(s2)], s2)
  expect_equal(summary(neuronlist(lhn)), s1) 
})
