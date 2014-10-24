context("catmaid_login")

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
