context("treenode queries")

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
test_that("catmaid_get_treenode_table", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  tempval <- catmaid_skids('annotation:^ORN$',conn = conn)
  skid_1 <-tempval[[1]]
  expect_is(tnt<-catmaid_get_treenode_table(skid_1, conn= conn), 'data.frame')
  # check that we have appropriate columns
  expect_true(all(c("x","y","z") %in% colnames(tnt)))
}))

with_mock_api(
test_that("catmaid_get_treenodes_detail", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  tempval <- catmaid_skids('annotation:^ORN$',conn = conn)
  skid_1 <-tempval[[1]]
  skid_2 <-tempval[[2]]
  skid_3 <-tempval[[3]]
  expect_is(tn<-catmaid_get_treenodes_detail(c(skid_1, skid_2, skid_3), conn= conn),
            'data.frame')
  # check that we have appropriate columns
  expect_equal(nrow(tn), 3L)
  expect_equal(colnames(tn), c("treenode_id", "parent_id", "x", "y", "z", 
                              "confidence", "radius", "skid", "edition_time", 
                              "user_id"))
}))
