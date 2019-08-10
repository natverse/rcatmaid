context("tags")

test_that("getting soma works", {
  c13 = nlapply(nat::Cell07PNs[1:3], function(x) {
    x$d$PointNo=x$d$PointNo+10L
    x$d$Parent=x$d$Parent+10L
    x$d$Parent[x$d$Parent==9]=-1L
    x$tags$soma = x$d$PointNo[1]
    class(x) = union('catmaidneuron', class(x))
    x
  })
  
  expect_equal(somaindex(c13), c(EBH11R = 1L, EBH20L = 1L, EBH20R = 1L))
  expect_equal(somaid(c13), c(EBH11R = 11L, EBH20L = 11L, EBH20R = 11L))
  baseline=rbind(xyzmatrix(c13[1])[1,],
                 xyzmatrix(c13[2])[1,],
                 xyzmatrix(c13[3])[1,])
  rownames(baseline)=names(c13)
  expect_equal(soma(c13), baseline)
  expect_equal(soma(c13)[1:2,], baseline[1:2,])
  expect_equal(soma(c13[1]), baseline[1,,drop=FALSE])
  expect_equal(soma(c13[[1]]), t(as.matrix(baseline[1,])))
  
  c13[[1]]$tags$soma=NULL
  c13[[2]]$tags$soma=c(c13[[2]]$tags$soma, c13[[2]]$d$PointNo[2])
  
  expect_warning(soma(c13))
  expect_warning(sidx2 <- somaindex(c13))
  expect_equal(sidx2, list(EBH11R = NA_integer_, EBH20L = 1:2, EBH20R = 1L))
  expect_warning(sid2 <- somaid(c13))
  expect_equal(sid2, list(EBH11R = NA_integer_, EBH20L = 11:12, EBH20R = 11L))
})
