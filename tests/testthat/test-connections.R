context("catmaid login and get/post")

# we can only run real tests if we can log in with default parameters
conn=try(catmaid_login(Force = TRUE), silent = TRUE)

# Sample server used in next block
publicserver <- "https://l1em.catmaid.virtualflybrain.org" #public server running a catmaid instance..

test_that("set and get the environmental variables responsible for connections", {

  #first check if any environment variable has the name of format catmaid. or catmaid_
  catmaid_msg <- try(catmaid_envstr())
  if (class(catmaid_msg)[[1]] == 'simpleMessage'){
    expect_message(message(catmaid_msg), regexp = "No usable environment")
    skip('No environmental variables found so skipping..')
  } else if (class(catmaid_msg) == "try-error"){
    #some environmental variables are missing here, so best to skip the tests again..
    skip('Some environmental variables are missing so skipping..')
  }

  # check the named object has valid field names
  expect_true(all(
    names(catmaid_connection_getenv()) %in%
      c(
        "server",
        "authname",
        "authpassword",
        "authtype",
        "token",
        "username",
        "password"
      )
  ))
  
  #Set environment variables through connection object and check them back again..
  pubconn <- catmaid_connection(server=publicserver, authname = "flyeee")
  catmaid_connection_setenv(conn = pubconn, Cache=FALSE)
  expect_match(catmaid_connection_getenv()['server'],publicserver)
  expect_match(catmaid_connection_getenv()['authname'],'flyeee')
  
  # reset environment vars so that other test cases can run
  # we reset them to those implied by the default connection if it is valid
  # otherwise we just unset them
  if(inherits(conn, 'try-error'))
    catmaid_connection_unsetenv()
  else
    catmaid_connection_setenv(conn = conn)
})

test_that("can connect to a server without logging in", {
  # No login is required to talk to this server
  expect_is(catmaid_login(server=publicserver, Cache = FALSE),
            'catmaid_connection')
})


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
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(conn, 'catmaid_connection')
  expect_is(conn$authresponse, 'response')
  expect_equal(conn$authresponse$status, 200L)
})

test_that("can get and post data", {
  pubconn <- catmaid_login(server=publicserver, Cache = FALSE)
  if(inherits(pubconn, 'try-error')) skip('No public catmaid connection')
  tempval <- catmaid_skids('annotation:^ORN$',conn = pubconn)
  skid_1 <-tempval[[1]]
  skid_2 <-tempval[[2]]
  expect_is(skel<-catmaid_fetch(paste0("1/", skid_1,"/0/0/compact-skeleton"), conn=pubconn, parse.json = FALSE),
            'response')
  expect_is(neuronnames<-catmaid_fetch("/1/skeleton/neuronnames", conn=pubconn,
                                       body=list(pid=1, 'skids[1]'=skid_1, 'skids[2]'=skid_2)),
            'list')
  # nb we can't rely on the returned order
  expect_equal(sort(names(neuronnames)), sort(as.character(c(skid_1,skid_2))))
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
