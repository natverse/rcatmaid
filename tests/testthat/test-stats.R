context("catmaid stats")

test_that("catmaid_user_history", {
  # we actually just test the guts of catmaid_user_history by using 
  # process_one_user_history
  raw_uh_baseline <- list(
    `25` = list(
      `20161107` = list(),
      `20161106` = list(),
      `20161108` = list()
    ),
    `4` = list(
      `20161107` = list(
        new_treenodes = 45726,
        new_connectors = 3L,
        new_reviewed_nodes = 330L
      ),
      `20161106` = list(),
      `20161108` = list(new_treenodes = 4024, new_connectors = 1L)
    ),
    `89` = list(
      `20161107` = list(new_treenodes = 216750, new_connectors = 31L),
      `20161106` = list(new_treenodes = 761434, new_connectors = 39L),
      `20161108` = list(new_treenodes = 588695, new_connectors = 40L)
    )
  )
  
  # we get back a tibble
  res <- dplyr::bind_rows(lapply(raw_uh_baseline, process_one_user_history))
  res_df <- as.data.frame(res)
  
  baseline_df <- data.frame(
    new_treenodes = c(45726, 4024, 216750, 761434, 588695),
    new_connectors = c(3L, 1L, 31L, 39L, 40L),
    new_reviewed_nodes = c(330L, NA, NA, NA, NA),
    date = structure(c(17112, 17113, 17112, 17111, 17113), class = "Date")
  )
  
  expect_equal(as.data.frame(res), baseline_df)
})

##Set up the configuration server for mocks..
privateserver <- "https://neuropil.janelia.org/tracing/fafb/v14"
fakeconn <- structure( list( server = privateserver,
                             nologin = TRUE, authresponse = TRUE, config=httr::config() ),
                       .class = "catmaid_connection")

set_requester(function (request) {
  gsub_request(request, "https://neuropil.janelia.org/tracing/fafb/v14/", "api/")
})

conn <- fakeconn

with_mock_api(
test_that("catmaid users", {
  expect_equal(catmaid_userids(1, conn = conn), 1L)
  expect_equal(catmaid_userids(1.00000000001, conn = conn), 1L)
  expect_equal(catmaid_userids(1:2, conn = conn), 1:2)
  expect_error(catmaid_userids(1.1, conn = conn))
  
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  ul=catmaid_get_user_list(conn = conn)
  l1=ul[['login']][1]
  i1=ul[['id']][1]
  expect_equal(catmaid_userids(c(l1, 'rhubarbcrumble'), conn = conn), c(i1, NA_integer_))  
}))
