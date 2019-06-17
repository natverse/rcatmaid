context("xform")

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
test_that("xform scales connectors as well", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  tempval <- catmaid_skids('annotation:^ORN$',conn = conn)
  skid_1 <-tempval[[1]]
  n=read.neuron.catmaid(skid_1,conn = conn)
  n10=xform(n, function(x, ...) x*10)
  expect_equivalent(xyzmatrix(connectors(n10)), xyzmatrix(connectors(n))*10)
}))
