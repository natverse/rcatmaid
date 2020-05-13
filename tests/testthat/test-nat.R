context("nat")

test_that("copy fields", {
  data(AV4b1)
  lhn=AV4b1
  expect_equal(copy_tags_connectors(new = lhn, old = lhn), lhn)
  
  r=resample(lhn, 1000)
  expect_equal(copy_tags_connectors(new = r, old = lhn, update_node_ids = FALSE)$tags, lhn$tags)

  
  expect_is(r2 <- copy_tags_connectors(new = r, old = lhn), 'catmaidneuron')
})

test_that("summary.catmaidneuron behaves", {
  data(AV4b1)
  lhn2=AV4b1
  class(lhn2)='neuron'
  expect_is(s1 <- summary(AV4b1), 'data.frame')
  expect_is(s2 <- summary(lhn2), 'data.frame')
  # check the initial summary columns are identical
  expect_equal(s1[seq_along(s2)], s2)
  expect_equal(summary(neuronlist(AV4b1)), s1) 
})

test_that("Ops.catmaidneuron behaves", {
  data(AV4b1)
  expect_equal(xyzmatrix(connectors(AV4b1*2)), xyzmatrix(connectors(AV4b1))*2)
  expect_equal(xyzmatrix(connectors(AV4b1*c(2,3,4,5))), 
               t(t(xyzmatrix(connectors(AV4b1)))*c(2,3,4)))
  expect_equal((AV4b1*2)/2, AV4b1)
})
