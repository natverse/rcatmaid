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

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("get volume list", {
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  expect_is(catmaid_get_volumelist(), 'data.frame')
})
