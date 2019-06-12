context("catmaid annotation queries")

library(catmaid)
library(httptest)

fakeconn <-
  structure(
    list(
      server = "https://neuropil.janelia.org/tracing/fafb/v14",
      nologin = TRUE,
      authresponse = TRUE,
      config=httr::config()
    ),
    .class = "catmaid_connection"
  )

conn = fakeconn

set_requester(function (request) {
  gsub_request(request, "https://neuropil.janelia.org/tracing/fafb/v14/", "api/")
})

test_that("Check connection",{
            expect_equal(fakeconn$server,"https://neuropil.janelia.org/tracing/fafb/v14") 
 })
 
with_mock_api(tempval <- catmaid_skids('annotation:^ORN$'))
skid_1 <-tempval[[1]]
skid_2 <-tempval[[2]]

with_mock_api(
  test_that("catmaid_get_annotations_for_skeletons", {
    if (inherits(conn, 'try-error')) skip('No catmaid connection')
    expect_is(n1 <- catmaid_get_annotations_for_skeletons(skids = skid_1,conn = conn),'data.frame')
    n2 = catmaid_get_annotations_for_skeletons(skids = skid_2,conn = conn)
    expect_is(n12 <- catmaid_get_annotations_for_skeletons(skids = c(skid_1, skid_2),conn = conn),'data.frame')
    expect_equal(rbind(n1, n2), n12)
 
}))
