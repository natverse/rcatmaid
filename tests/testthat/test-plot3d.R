context("plot3d")

test_that("choose appropriate soma radius for plotting", {
  # tests with a neuron without an explicit soma
  x=Cell07PNs[[1]]
  expect_equal(plot3d_somarad(x), FALSE)
  expect_equal(plot3d_somarad(x, soma=TRUE), FALSE)
  expect_equal(plot3d_somarad(x, soma=2), FALSE)
  expect_equal(plot3d_somarad(x, soma=-2), 2)
  
  # now make a neuron with an explicit soma
  y=x
  y$tags$soma=y$StartPoint
  expect_equal(plot3d_somarad(y), FALSE)
  expect_equal(plot3d_somarad(y, soma=TRUE), 1.01/2)
  expect_equal(plot3d_somarad(y, soma=2), 2)
  expect_equal(plot3d_somarad(y, soma=-2), 2)
  
  # now give the neuron a bogus diameter at the soma
  y$d$W[1]=-2
  expect_equal(plot3d_somarad(y), FALSE)
  expect_equal(plot3d_somarad(y, soma=TRUE), TRUE)
  expect_equal(plot3d_somarad(y, soma=2), 2)
  expect_equal(plot3d_somarad(y, soma=-2), 2)
})
