#This file contains details on how to cache http requests before using them in mock apis for offline testing
library(httptest)
library(testthat)

#Step 1: Set the folder locations..
testpath = paste0('./',test_path(),'/testdata')
.mockPaths(testpath)

#Step 2: Connect to a valid catmaid server..
conn <- catmaid_login()

#Step 2: Peform some capture requests to store data..
start_capturing()
skel<-catmaid_fetch("1/10418394/0/0/compact-skeleton", conn=conn, parse.json = FALSE)
stop_capturing()
