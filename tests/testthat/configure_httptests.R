#This file contains details on how to cache http requests before using them in mock apis for offline testing
library(httptest)
library(catmaid)

#Step 1: Set the folder locations..
testpath = paste0('./',test_path())
.mockPaths(testpath)


#Step 2: Start capturing http data from now..

start_capturing()

#Step 3: Connect to a valid catmaid server..
conn <- catmaid_login()

#Now add test cases for each file..

#For test file : test-annotation.R
      #For test case : "catmaid_get_annotations_for_skeletons"
tempval <- catmaid_skids('annotation:^ORN$')
skid_1 <- tempval[[1]]
skid_2 <- tempval[[2]]
pid <- 1

n1 <- catmaid_get_annotations_for_skeletons(skids = skid_1)
n2 <- catmaid_get_annotations_for_skeletons(skids = skid_2)
n12 <- catmaid_get_annotations_for_skeletons(skids = c(skid_1, skid_2))

pathvar = paste0(pid,"/",skid_1,"/0/0/compact-skeleton")
skel<-catmaid_fetch(pathvar, conn=conn, parse.json = FALSE)
neuronnames<-catmaid_fetch("/1/skeleton/neuronnames", conn=conn,body=list(pid=pid, 
                          'skids[1]'=skid_1, 'skids[2]'=skid_2))

#For test file : 
      #For test case :

#Step 4: Stop the capture
stop_capturing()

#Step 5: Rename the captured folder structure like below..
#old_folderstruct -- "neuropil.janelia.org/tracing/fafb/v14/"
#new_folderstruct -- "api/"



