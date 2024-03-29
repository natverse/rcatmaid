context("catmaid annotation queries")

library(catmaid)
library(httptest)

##Set up the configuration server for mocks..
privateserver <- "https://neuropil.janelia.org/tracing/fafb/v14"
fakeconn <- structure( list( server = privateserver,
                             nologin = TRUE, authresponse = TRUE, config=httr::config() ),
                      .class = "catmaid_connection")

set_requester(function (request) {
  gsub_request(request, "https://neuropil.janelia.org/tracing/fafb/v14/", "api/")
})


##Set up the configuration server now for live public configurations.
publicserver="https://l1em.catmaid.virtualflybrain.org"
pubconn <- vfbcatmaid("l1em", Cache = FALSE)

print(paste0("Testing with public live server: ",publicserver, sep=""))
print(paste0("Testing with mock server: ",privateserver, sep=""))

#Set the environmental variable responsible for mocking
Sys.setenv(MOCK_BYPASS = "true") #"true" (public live server) , "capture" (mock server) 

test_that("Check mock connection",{
            expect_equal(fakeconn$server,privateserver) 
 })

test_that("Check live connection",{
  expect_equal(pubconn$server,publicserver) 
})
 
##Tests with mock connection first..
context("catmaid annotation queries-mock connection")
with_mock_api(tempval <- catmaid_skids('annotation:^ORN$',conn = fakeconn))
skid_1 <-tempval[[1]]
skid_2 <-tempval[[2]]

with_mock_api(
  test_that("catmaid_get_annotations_for_skeletons", {
    if (inherits(fakeconn, 'try-error')) skip('No catmaid connection')
    expect_is(n1 <- catmaid_get_annotations_for_skeletons(skids = skid_1,conn = fakeconn),'data.frame')
    n2 = catmaid_get_annotations_for_skeletons(skids = skid_2,conn = fakeconn)
    expect_is(n12 <- catmaid_get_annotations_for_skeletons(skids = c(skid_1, skid_2),conn = fakeconn),'data.frame')
    expect_equal(rbind(n1, n2), n12)
 
}))


##Tests with public live connection next..
context("catmaid annotation queries-public connection")
tempval <- catmaid_skids('annotation:^ORN$',conn = pubconn)
skid_1 <-tempval[[1]]
skid_2 <-tempval[[2]]

test_that("catmaid_get_annotations_for_skeletons", {
    if (inherits(pubconn, 'try-error')) skip('No catmaid connection')
    expect_is(n1 <- catmaid_get_annotations_for_skeletons(skids = skid_1,conn = pubconn),'data.frame')
    n2 = catmaid_get_annotations_for_skeletons(skids = skid_2,conn = pubconn)
    # NB the ordering is apparently not guaranteed, so need to fix this
    rn12 <- rbind(n1, n2)
    rn12 <- rn12[order(rn12$skid, rn12$id),]
    row.names(rn12) <- NULL
    
    expect_is(n12 <- catmaid_get_annotations_for_skeletons(skids = c(skid_1, skid_2),conn = pubconn),'data.frame')
    
    n12 <- n12[order(n12$skid, n12$id),]
    row.names(n12) <- NULL
    expect_equal(rn12, n12)
  })


test_that('model - entity conversion', {
  if (inherits(pubconn, 'try-error')) skip('No catmaid connection')
  
  ents <- c(`3991518` = 3570509L, `9747710` = 9747712L)
  skids=c(3991518,9747710)
  expect_equal(catmaid_entities_from_models(skids, conn = pubconn),
               ents)
  l=as.list(as.integer(names(ents)))
  names(l)=unname(ents)
  expect_equal(catmaid_models_from_entities(ents, conn=pubconn), l)
})
