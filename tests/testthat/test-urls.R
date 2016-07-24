context("catmaid urls")


test_that("catmaid_parse_url", {
  u="https://neuropil.janelia.org/tracing/fafb/v12/?pid=1&zp=177100&yp=166068&xp=373086&tool=tracingtool&sid0=7&s0=1.000000"
  baseline=data.frame(baseurl = "https://neuropil.janelia.org/tracing/fafb/v12", 
                      pid = 1L, z = 177100L, y = 166068L, x = 373086L, 
                      tool = factor("tracingtool"),sid0 = 7L, s0 = 1, 
                      stringsAsFactors = FALSE)
  expect_equal(catmaid_parse_url(u), baseline)
})
