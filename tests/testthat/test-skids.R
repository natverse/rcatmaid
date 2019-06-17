context("process skid specifications")

##Set up the configuration server for mocks..
privateserver <- "https://neuropil.janelia.org/tracing/fafb/v14"
fakeconn <- structure( list( server = privateserver,
                             nologin = TRUE, authresponse = TRUE, config=httr::config() ),
                       .class = "catmaid_connection")

set_requester(function (request) {
  gsub_request(request, "https://neuropil.janelia.org/tracing/fafb/v14/", "api/")
})

conn <- fakeconn


test_that("process numeric skids", {
  expect_equal(catmaid_skids(1L), 1L)
  expect_equal(catmaid_skids(2.0), 2L)
  expect_equal(catmaid_skids("3"), 3L)
  expect_equal(catmaid_skids(as.character(1:3)), 1:3)
  expect_error(catmaid_skids(1:3, several.ok = F))
  expect_error(catmaid_skids(as.character(1:3), several.ok = F))
})

with_mock_api(
test_that("process skid queries", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(catmaid_skids("name:ORN", conn = conn), "integer")
  # although neuron names can get pretty unusual ...
  expect_equal(suppressWarnings(catmaid_skids("name:rhubarb crumble", conn = conn)), integer())
  expect_is(catmaid_skids("annotation:^ORN$", conn = conn), "integer")
  expect_error(suppressWarnings(catmaid_skids("annotation:ORN", conn = conn, several.ok = FALSE)))
  expect_equal(suppressWarnings(catmaid_skids("annotation:rhubarb crumble", conn = conn)), 
               integer())
  
  # interpreted as annotation
  expect_equal(catmaid_skids('ORN', conn = conn), catmaid_skids('annotation:^ORN$', conn = conn))
  expect_warning(catmaid_skids('rhubarb', conn = conn), regexp = "complete annotation.*no matches")
}))
