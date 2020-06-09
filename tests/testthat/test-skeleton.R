context("test-skeleton.R")

##Set up the configuration server for mocks..
privateserver <- "https://neuropil.janelia.org/tracing/fafb/v14"
fakeconn <- structure( list( server = privateserver,
                             nologin = TRUE, authresponse = TRUE, config=httr::config() ),
                       .class = "catmaid_connection")

set_requester(function (request) {
  gsub_request(request, "https://neuropil.janelia.org/tracing/fafb/v14/", "api/")
})

conn <- fakeconn
with_mock_api(
test_that("skeleton-tests", {
  if(inherits(conn, 'try-error'))
    skip('No catmaid connection')
  
  sk=catmaid_skids("^ORN$",conn=conn)[1:2]
  expect_is(nc <- catmaid_get_node_count(sk,conn=conn), 'integer')
  sk2=c(rep(sk, 2), NA)
  nc2=c(rep(nc, 2), NA)
  # make sure that we can cope with duplicates and NAs
  expect_equal(catmaid_get_node_count(sk2,conn=conn), nc2)
}))
