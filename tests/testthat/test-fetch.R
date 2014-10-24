context("catmaid metadata queries")

# login is handled by test-connections
conn=getOption('catmaid_temp_conn')

test_that("get neuron names", {
  if(!inherits(conn, 'try-error')){
    expect_equal(catmaid_get_neuronnames(pid=1, skids=c(10418394,4453485)),
                 structure(c("IPC10", "IPC1"), .Names = c("10418394", "4453485")))
  }
})
