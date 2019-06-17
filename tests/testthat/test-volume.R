context("volumes")
test_that("can pre-process meshes for catmaid", {
  mbcal = subset(nat::MBL.surf, "MB_CA_L")
  expect_equal(as.catmaidmesh(mbcal), as.catmaidmesh(as.mesh3d(mbcal)))
  
  eg = jsonlite::fromJSON(
    paste(
      '{"title": "Example mesh",',
      '"type": "trimesh",',
      '"mesh": [[[0,0,0], [1,0,0], [0,1,0], [1,1,0]],',
      '[[0,1,2], [1,3,2]]]}'
    )
  )
  class(eg)='catmaidmesh'
  expect_equal(as.catmaidmesh(
    list(matrix(c(0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0), ncol = 3L),
         matrix(c(0, 1, 1, 3, 2, 2), ncol = 3L)),
    title = "Example mesh"
  ), eg)
})

##Create dummy response object..
responseobj <- structure(list(url = "http://dummyresp.org/", status_code = 200L, 
                              cookies = structure(list(),row.names = integer(0), class = "data.frame")), 
                         class = 'response')

##Set up the configuration server for mocks..
privateserver <- "https://neuropil.janelia.org/tracing/fafb/v14"
fakeconn <- structure( list( server = privateserver,
                             nologin = TRUE, authresponse = responseobj, config=httr::config() ),
                       .class = "catmaid_connection")

set_requester(function (request) {
  gsub_request(request, "https://neuropil.janelia.org/tracing/fafb/v14/", "api/")
})

conn <- fakeconn

with_mock_api(
test_that("get volume list", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(catmaid_get_volumelist(conn = conn), 'data.frame')
}))
