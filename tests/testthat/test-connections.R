context("catmaid_login")

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

test_that("can login", {
  
  conn=try(catmaid_login(), silent = TRUE)
  if(!inherits(conn, 'try-error')){
    # we can only run real tests if we can get a valid connection with default parameters
    expect_is(conn, 'catmaid_connection')
    expect_is(conn$authresponse, 'response')
    expect_equal(conn$authresponse$status, 200L)
  }
  
})
