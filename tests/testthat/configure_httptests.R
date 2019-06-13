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

#For test file : test-fetch.R
      #For test case : catmaid_get_neuronnames
catmaid_get_neuronnames(skids=c(77,446))
catmaid_get_neuronnames(skids=c(77,446, NA))
     #For test case : catmaid_query_by_name
catmaid_query_by_name("wurgle")
catmaid_query_by_name("ORN")
    #For test case : catmaid_query_by_annotation
catmaid_query_by_annotation("^ORN$")
catmaid_query_by_annotation("wurgle")
catmaid_query_by_name("^ORN$")$id
    #For test case : get neuron
catmaid_get_compact_skeleton(pid=1, skid=skid_1, conn=conn)
    #For test case : read.neuron(s).catmaid and connectors
read.neuron.catmaid(pid=1, skid=skid_1, conn=conn)
read.neurons.catmaid(c(skid_2,skid_1), conn=conn)

#Step 4: Stop the capture
stop_capturing()

#Step 5: Rename the captured folder structure like below..
unlink(paste0(testpath, "/api/", sep=""), recursive = TRUE) 
old_folderstruct <- paste0(testpath, "/neuropil.janelia.org/tracing/fafb/v14/.", sep="")
new_folderstruct <- paste0(testpath, "/api/", sep="")
dir.create(new_folderstruct)
file.copy(old_folderstruct, new_folderstruct, recursive=TRUE)
unlink(paste0(testpath, "/neuropil.janelia.org", sep=""), recursive = TRUE) 



