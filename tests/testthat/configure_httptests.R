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

#For test file : test-connections.R
      #For test case : 


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

#For test file : test-nat.R
      #For test case : 

#For test file : test-plot3d.R
      #For test case : 

#For test file : test-selections.R
      #For test case : 

#For test file : test-skeleton.R
      #For test case : skeleton-tests
sk <- catmaid_skids("^ORN$")[1:2]
nc <- catmaid_get_node_count(sk)

#For test file : test-skids.R
      #For test case : process skid queries
catmaid_skids("name:ORN")
catmaid_skids("name:rhubarb crumble")
catmaid_skids("annotation:^ORN$")
catmaid_skids("annotation:ORN", several.ok = FALSE)
catmaid_skids("annotation:rhubarb crumble")
catmaid_skids('ORN')
catmaid_skids('rhubarb')

#For test file : test-stats.R
      #For test case : catmaid users
ul=catmaid_get_user_list()
l1=ul[['login']][1]
i1=ul[['id']][1]
catmaid_userids(c(l1, 'rhubarbcrumble')) 
      #For test case : catmaid_get_time_invested
tempval <- catmaid_skids('annotation:^ORN$', conn = conn)
skid_1 <-tempval[[1]]
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM')
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='OVER_TIME')
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='ACTIONS')
catmaid_get_time_invested('annotation:^Putative Dm9', conn = conn,mode='SUM')

catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM', connectors = FALSE)
test_sk <- 7349300
catmaid_get_time_invested(skids=test_sk, conn = conn,mode='SUM')
catmaid_get_time_invested(skids=test_sk, conn = conn,mode='OVER_TIME')
catmaid_get_time_invested(skids=test_sk, conn = conn,mode='ACTIONS')
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM', treenodes=FALSE)

tempval <- catmaid_skids('annotation:single node no tag test Sri',conn = conn)
skid_1 <-tempval[[1]]
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM')
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='OVER_TIME')
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='ACTIONS')
catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM',connectors = FALSE)




#For test file : test-treenodes.R
      #For test case : catmaid_get_treenode_table
tempval <- catmaid_skids('annotation:^ORN$',conn = conn)
skid_1 <-tempval[[1]]
catmaid_get_treenode_table(skid_1, conn= conn)
      #For test case : catmaid_get_treenodes_detail
skid_2 <-tempval[[2]]
skid_3 <-tempval[[3]]
catmaid_get_treenodes_detail(c(skid_1, skid_2, skid_3), conn= conn)

#For test file : test-volume.R
      #For test case : get volume list
tmpvolume <- catmaid_get_volumelist(conn = conn)

#For test file : test-xform.R
      #For test case : xform scales connectors as well
tempval <- catmaid_skids('annotation:^ORN$',conn = conn)
skid_1 <-tempval[[1]]
read.neuron.catmaid(skid_1,conn = conn)


#Step 4: Stop the capture
stop_capturing()

#Step 5: Rename the captured folder structure like below..
unlink(paste0(testpath, "/api/", sep=""), recursive = TRUE) 
old_folderstruct <- paste0(testpath, "/neuropil.janelia.org/tracing/fafb/v14/.", sep="")
new_folderstruct <- paste0(testpath, "/api/", sep="")
dir.create(new_folderstruct)
file.copy(old_folderstruct, new_folderstruct, recursive=TRUE)
unlink(paste0(testpath, "/neuropil.janelia.org", sep=""), recursive = TRUE) 



