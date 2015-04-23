context("catmaid metadata queries")

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("catmaid_get_neuronnames", {
  if(!inherits(conn, 'try-error')){
    baseline=c(`4453485`="IPC1", `10418394`="IPC10")
    expect_equal(catmaid_get_neuronnames(skids=c(4453485,10418394)), baseline)
  }
})

context("catmaid neuron queries")

test_that("get neuron", {
  if(!inherits(conn, 'try-error')){
    expect_is(skel<-catmaid_get_compact_skeleton(pid=1, skid=10418394, conn=conn), 'list')
  }
})

test_that("read.neuron.catmaid", {
  if(!inherits(conn, 'try-error')){
    expect_is(n<-read.neuron.catmaid(pid=1, skid=10418394, conn=conn), 'neuron')
  }
})
