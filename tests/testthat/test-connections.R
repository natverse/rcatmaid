context("catmaid login and get/post")

# set any catmaid options from environment vars
# they could have been exported as follows:
# do.call(Sys.setenv, options()[grep('catmaid',names(options()))])
catmaid_opnames=paste("catmaid", c("server", "username", "password", "authname", 
                                   "authpassword", "authtype"),
                      sep=".")
catmaid_ops=Sys.getenv(catmaid_opnames)
op=options(as.list(catmaid_ops[nzchar(catmaid_ops)]))

# we can only run real tests if we can log in with default parameters
conn=try(catmaid_login(), silent = TRUE)

test_that("can make a connection", {
  
  expect_error(catmaid_connection(server="http://somewhere.org"))
  conn<-catmaid_connection(server="https://somewhere.org", username = 'calvin', password = 'hobbes')
  expect_is(conn, "catmaid_connection")
  expect_is(conn$config, "config")
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
  conn1=catmaid_connection(server = "https://wurgle.com", user='rhubarb', password='crumble')
  
  # store connection manually
  .package_statevars$connections[['wurgle_cookie']]=conn1
  
  expect_equal(catmaid_cached_connection(conn = conn1), conn1)
  expect_equal(catmaid_cached_connection(conn = catmaid_connection(
    server="https://wurgle.com")), conn1)
  expect_equal(catmaid_cached_connection(conn = catmaid_connection(
    server="https://wurgle.com", user='rhubarb')), conn1)
  expect_null(catmaid_cached_connection(conn = catmaid_connection(
    server="https://wurgle.com", user='apple')))
  
  # remove fake cached connection
  .package_statevars$connections[['wurgle_cookie']]=NULL
})
