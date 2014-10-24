context("catmaid login and get/post")

# set any catmaid options from environment vars
# they could have been exported as follows:
# do.call(Sys.setenv, options()[grep('catmaid',names(options()))])
catmaid_opnames=paste("catmaid", c("server", "username", "password", "authname", 
                                   "authpassword", "authtype"),
                      sep=".")
catmaid_ops=Sys.getenv(catmaid_opnames)
op=options(as.list(catmaid_ops[nzchar(catmaid_ops)]))

test_that("can make a connection", {
  
  expect_error(catmaid_connection(server="http://somewhere.org"))
  conn<-catmaid_connection(server="https://somewhere.org", username = 'calvin', password = 'hobbes')
  expect_is(conn, "catmaid_connection")
  expect_is(conn$config, "config")
})

# we can only run real tests if we can log in with default parameters
conn=try(catmaid_login(), silent = TRUE)

test_that("can login", {
  if(!inherits(conn, 'try-error')){
    expect_is(conn, 'catmaid_connection')
    expect_is(conn$authresponse, 'response')
    expect_equal(conn$authresponse$status, 200L)
  }
  
})

test_that("can get and post data", {
  if(!inherits(conn, 'try-error')){
    expect_is(skel<-catmaid_GET("1/10418394/0/0/compact-skeleton", conn=conn), 'response')
    expect_equivalent(neuronnames<-catmaid_POSTJ("/1/skeleton/neuronnames", conn=conn,
                                    body=list(pid=1, 'skids[1]'=10418394, 'skids[2]'=4453485)),
                      list(`10418394` = "IPC10", `4453485` = "IPC1"))
    expect_equal(names(attributes(neuronnames)), c("names", "url", "headers"))
  }  
})
