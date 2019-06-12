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


##Set up the configuration server now for live public configurations..
publicserver <- "https://l1em.catmaid.virtualflybrain.org"
pubconn <- catmaid_login(server=publicserver, Cache = FALSE)


#Set the environmental variable responsible for mocking
Sys.setenv(MOCK_BYPASS = "true") #"true" (public live server) , "capture" (mock server) 

##Now check if the test is with real server or mock server
if (Sys.getenv("MOCK_BYPASS") == "true") {
    with_mock_api <- force
    conn <- pubconn
    print(paste0("Testing with public live server: ",publicserver, sep=""))
} else if (Sys.getenv("MOCK_BYPASS") == "capture") {
    conn <- fakeconn
    print("Testing with mock server")
}


test_that("Check mock connection",{
            expect_equal(fakeconn$server,privateserver) 
 })

test_that("Check live connection",{
  expect_equal(pubconn$server,publicserver) 
})
 

with_mock_api(tempval <- catmaid_skids('annotation:^ORN$',conn = conn))
skid_1 <-tempval[[1]]
skid_2 <-tempval[[2]]

with_mock_api(
  test_that("catmaid_get_annotations_for_skeletons", {
    if (inherits(conn, 'try-error')) skip('No catmaid connection')
    expect_is(n1 <- catmaid_get_annotations_for_skeletons(skids = skid_1,conn = conn),'data.frame')
    n2 = catmaid_get_annotations_for_skeletons(skids = skid_2,conn = conn)
    expect_is(n12 <- catmaid_get_annotations_for_skeletons(skids = c(skid_1, skid_2),conn = conn),'data.frame')
    expect_equal(rbind(n1, n2), n12)
 
}))
