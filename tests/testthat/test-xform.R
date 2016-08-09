context("xform")

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("xform scales connectors as well", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  n=read.neuron.catmaid(10179501)
  n10=xform(n, function(x, ...) x*10)
  expect_equivalent(xyzmatrix(connectors(n10)), xyzmatrix(connectors(n))*10)
})
