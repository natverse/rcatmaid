context("catmaid metadata queries")

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("catmaid_get_neuronnames", {
  if(!inherits(conn, 'try-error')){
    baseline=c(`4453485`="IPC1", `10418394`="IPC10")
    expect_equal(catmaid_get_neuronnames(skids=c(4453485,10418394)), baseline)
  }
})

test_that("catmaid_query_by_name", {
  if(!inherits(conn, 'try-error')){
    expect_null(catmaid_query_by_name("wurgle"))
    expect_is(rdf<-catmaid_query_by_name("ORN"), 'data.frame')
    expect_equal(names(rdf), c("id", "name", "type", "skid"))
    expect_is(attr(rdf,'annotations'),'data.frame')
  }
})

test_that("catmaid_query_by_annotation", {
  if(!inherits(conn, 'try-error')){
    expect_null(catmaid_query_by_annotation("wurgle"))
    expect_is(rdf<-catmaid_query_by_annotation("^ORN$"), "data.frame")
    expect_is(ornid<-catmaid_query_by_name("^ORN$")$id, 'integer')
    expect_equal(catmaid_query_by_annotation(ornid), rdf)
  }
})

context("catmaid neuron queries")

test_that("get neuron", {
  if(!inherits(conn, 'try-error')){
    expect_is(skel<-catmaid_get_compact_skeleton(pid=1, skid=10418394, conn=conn), 'list')
  }
})

test_that("read.neuron(s).catmaid and connectors", {
  if(!inherits(conn, 'try-error')){
    expect_is(n<-read.neuron.catmaid(pid=1, skid=10418394, conn=conn), 'neuron')
    # check that we also have this specialised class
    expect_is(n, 'catmaidneuron')
    expect_is(nl<-read.neurons.catmaid(c(4453485,10418394), conn=conn), 'neuronlist')
    expect_is(df<-as.data.frame(nl), 'data.frame')
    expect_is(df$name, "character")
    
    expect_error(read.neurons.catmaid(1), "catmaid error:.*Skeleton")
    
    expect_is(connectors(n), "data.frame")
    expect_is(connectors(nl), "data.frame")
    
    expect_equal((((nl*1)+2)-2), nl)
  }
})
