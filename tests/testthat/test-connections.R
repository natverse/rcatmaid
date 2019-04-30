context("catmaid login and get/post")

test_that("set and get the environmental variables responsible for connections", {
  #check the named object has all necessary fields..
  expect_named(catmaid_connection_getenv(), c("server", "authname", "authpassword","token"))
  #check if it returns a specific authname..
  expect_match(catmaid_connection_getenv()['authname'],'fly')
  #set a specific server name and check if it returns it back..
  
  
  #Set environmental variables through connection object and check them back again..
  conn <- catmaid_connection(server='http://somewhere.org', authname = "flyeee")
  catmaid_connection_setenv(conn = conn)
  expect_match(catmaid_connection_getenv()['server'],'http://somewhere.org')
  expect_match(catmaid_connection_getenv()['authname'],'flyeee')
  
  #just unset them so the other test cases can run..
  conn['authname'] <- "fly"
  catmaid_connection_setenv(conn = conn)
  expect_match(catmaid_connection_getenv()['authname'],'fly')

  
})

test_that("can connect to a server without logging in", {
  # No login is required to talk to this server
  expect_is(catmaid_login(server='http://hildebrand16.neurodata.io/catmaid/', Cache = FALSE),
            'catmaid_connection')
})

# we can only run real tests if we can log in with default parameters
conn=try(catmaid_login(Force = TRUE), silent = TRUE)

test_that("can make a connection", {
  
  expect_is(catmaid_connection(server="http://somewhere.org"), 'catmaid_connection')
  conn<-catmaid_connection(server="https://somewhere.org", username = 'calvin', password = 'hobbes')
  expect_is(conn, "catmaid_connection")
  expect_is(conn$config, class(config()))
})

test_that("can make a connection from list elements", {
  # see https://github.com/jefferis/rcatmaid/issues/91
  input = list(server = "https://wurgle.org", token = "YOURTOKEN")
  expect_is(
    catmaid_connection(server = input$server, token = input$token),
    'catmaid_connection'
  )
})

test_that("can login", {
  #Just restart the connection here to a public server..
  conn <- catmaid_login(server='http://hildebrand16.neurodata.io/catmaid/', Cache = FALSE)
  
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(conn, 'catmaid_connection')
  expect_is(conn$authresponse, 'response')
  expect_equal(conn$authresponse$status, 200L)
})

test_that("can get and post data", {
  
  #Just restart the connection here to a public server..
  conn <- catmaid_login(server='http://hildebrand16.neurodata.io/catmaid/', Cache = FALSE)
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
 
  #Fetch the skeletons first..
  expect_is(skel<-catmaid_fetch("2/skeletons", conn=conn, parse.json = FALSE),
            'response')
  
  #Get the neuronnames from some skids..
  skel_parsed<-catmaid_fetch("2/skeletons", conn=conn, parse.json = TRUE)
  expect_is(neuronnames<-catmaid_fetch("/2/skeleton/neuronnames", conn=conn,
                                       body=list(pid=2, 'skids[1]'=skel_parsed[[1]], 'skids[2]'=skel_parsed[[7]])),
            'list')
  # nb we can't rely on the returned order
  expect_equal(sort(names(neuronnames)), c(as.character(skel_parsed[[1]]),as.character(skel_parsed[[7]])))
  expect_equal(names(attributes(neuronnames)), c("names", "url", "headers"))
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
