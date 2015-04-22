context("catmaid metadata queries")

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("get neuron names", {
  if(!inherits(conn, 'try-error')){
    expect_equal(catmaid_get_neuronnames(pid=1, skids=c(10418394,4453485)),
                 structure(c("IPC10", "IPC1"), .Names = c("10418394", "4453485")))
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

test_that("catmaid_get_neuronnames", {
  if(!inherits(conn, 'try-error')){
    baseline=c(`4453485`="IPC1", `10418394`="IPC10")
    expect_equal(catmaid_get_neuronnames(skids=c(4453485,10418394)), baseline)
  }
})
