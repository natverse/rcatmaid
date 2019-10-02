context("catmaid metadata queries")

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
test_that("catmaid_get_neuronnames", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  baseline=c(`77`="KC001 DB", `446`="KC002 DB")
  expect_equal(catmaid_get_neuronnames(skids=c(77,446),conn=conn), baseline)
}))

with_mock_api(
test_that("catmaid_get_neuronnames with duplicates / NAs", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  baseline=c(`77`="KC001 DB", `446`="KC002 DB", `NA`=NA_character_)
  sk=c(77,446, NA)
  expect_equal(catmaid_get_neuronnames(skids=sk,conn=conn), baseline)
  expect_equal(catmaid_get_neuronnames(skids=rep(sk, 2),conn=conn)[1:3], baseline)
}))

with_mock_api(
test_that("catmaid_query_by_name", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_null(catmaid_query_by_name("wurgle",conn=conn))
  expect_is(rdf<-catmaid_query_by_name("ORN",conn=conn), 'data.frame')
  expect_equal(names(rdf), c("id", "name", "type", "skid"))
}))

with_mock_api(
test_that("catmaid_query_by_annotation", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_null(catmaid_query_by_annotation("wurgle",conn=conn))
  expect_is(rdf<-catmaid_query_by_annotation("^ORN$",conn=conn), "data.frame")
  expect_is(ornid<-catmaid_query_by_name("^ORN$",conn=conn)$id, 'integer')
  expect_equal(catmaid_query_by_annotation(ornid,conn=conn), rdf)
}))

context("catmaid neuron queries")

#with_mock_api <- force

with_mock_api(tempval <- catmaid_skids('annotation:^ORN$',conn = conn))

skid_1 <-tempval[[1]]
skid_2 <-tempval[[2]]

with_mock_api(
test_that("get neuron", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(skel<-catmaid_get_compact_skeleton(pid=1, skid=skid_1, conn=conn), 'list')
}))

with_mock_api(
test_that("read.neuron(s).catmaid and connectors", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(n<-read.neuron.catmaid(pid=1, skid=skid_1, conn=conn), 'neuron')
  # check that we also have this specialised class
  expect_is(n, 'catmaidneuron')
  expect_equal(n$InputFileName, as.character(skid_1))
  expect_is(nl<-read.neurons.catmaid(c(skid_2,skid_1), conn=conn), 'neuronlist')
  expect_is(df<-as.data.frame(nl), 'data.frame')
  expect_is(df$name, "character")
  
  expect_error(read.neurons.catmaid(1))
  
  expect_is(connectors(n), "data.frame")
  expect_is(connectors(nl), "data.frame")
  
  expect_equal((nl*2+0)/2, nl)
}))

test_that("read.neuron", {
  skip_if_offline()
  pubconn <- catmaid_connection(
    server="https://fafb.catmaid.virtualflybrain.org")
  n=try(read.neurons.catmaid('name:PN glomerulus DL4 23830 JMR', conn=pubconn)[[1]])
  skip_if_not(is.neuron(n))
  # exactly one soma recorded in Label column
  expect_equal(sum(n$d$Label==1L), 1L)
})
