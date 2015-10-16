context("catmaid login and get/post")

# we can only run real tests if we can log in with default parameters
conn=try(catmaid_connection_getenv(), silent = F)

test_that("can make a connection", {
  
  expect_is(catmaid_connection(server="http://somewhere.org"), 'catmaid_connection')
  conn<-catmaid_connection(server="https://somewhere.org", username = 'calvin', password = 'hobbes')
  expect_is(conn, "catmaid_connection")
  expect_is(conn$config, class(config()))
})

test_that("can login", {
  if(!inherits(conn, 'try-error')){
    expect_is(conn, 'catmaid_connection')
    expect_is(conn$authresponse, 'response')
    expect_equal(conn$authresponse$status, 200L)
  }
  
})

test_that("can get and post data", {
  if(!inherits(conn, 'try-error')){
    expect_is(skel<-catmaid_fetch("1/10418394/0/0/compact-skeleton", conn=conn, parse.json = FALSE),
              'response')
    expect_equivalent(neuronnames<-catmaid_fetch("/1/skeleton/neuronnames", conn=conn,
                                    body=list(pid=1, 'skids[1]'=10418394, 'skids[2]'=4453485)),
                      list(`10418394` = "IPC10", `4453485` = "IPC1"))
    expect_equal(names(attributes(neuronnames)), c("names", "url", "headers"))
  }  
})

test_that("can fetch cached connection", {
  conn1=catmaid_connection(server = "https://wurgle.com", 
                           username = 'rhubarb', password='crumble')
  
  # store connection manually
  .package_statevars$connections[['wurgle_cookie']]=conn1
  
  expect_equal(catmaid_cached_connection(conn = conn1), conn1)
  expect_equal(catmaid_cached_connection(conn = catmaid_connection(
    server="https://wurgle.com")), conn1)
  expect_equal(catmaid_cached_connection(conn = catmaid_connection(
    server="https://wurgle.com", username = "rhubarb")), conn1)
  expect_null(catmaid_cached_connection(conn = catmaid_connection(
    server="https://wurgle.com", username = "apple")))
  
  # remove fake cached connection
  .package_statevars$connections[['wurgle_cookie']]=NULL
})
