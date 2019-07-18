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

with_mock_api(
test_that("catmaid_get_time_invested", {
  tempval <- catmaid_skids('annotation:^ORN$',conn = conn)
  skid_1 <-tempval[[1]]
  test_summode <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM')
  test_overtimemode <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='OVER_TIME')
  test_actionsmode <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='ACTIONS')
  test_summodeannot <- catmaid_get_time_invested('annotation:^Putative Dm9', conn = conn,mode='SUM')
  
  #Test datastructures now..
  expect_is(test_summode,'data.frame')
  expect_is(test_overtimemode,'data.frame')
  expect_is(test_actionsmode,'data.frame')
  expect_is(test_summodeannot,'data.frame')
  
  
  #Test details now..
  expect_equal(63,test_summode[test_summode$login == 'heatha','total'])
  expect_equal(18,test_overtimemode[test_overtimemode$login == 'heatha','2016-07-12'])
  expect_equal(1147,test_actionsmode[test_actionsmode$login == 'all_users','2016-07-12'])
  expect_equal(24,test_summodeannot[test_summodeannot$login == 'kinde','total'])
  
  #test cases with no connectors
  test_noconnectors <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM', connectors = FALSE)
  test_sk <- 7349300
  test_noconnectors <- catmaid_get_time_invested(skids=test_sk, conn = conn,mode='SUM')
  expect_equal(42,test_noconnectors[test_noconnectors$login == 'engerts','total'])
  
  test_noconnectors <- catmaid_get_time_invested(skids=test_sk, conn = conn,mode='OVER_TIME')
  expect_equal(14,test_noconnectors[test_noconnectors$login == 'all_users','2018-02-26'])
  
  test_noconnectors <- catmaid_get_time_invested(skids=test_sk, conn = conn,mode='ACTIONS')
  expect_equal(1010,test_noconnectors[test_noconnectors$login == 'all_users','2018-02-26'])
  
  
  #test cases with no treenodes
  test_notreenodes <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM', treenodes=FALSE)
  expect_equal(9,test_notreenodes[test_notreenodes$login == 'robertsr','total'])
  
  #test cases with both no connectors and treenodes
  expect_error(catmaid_get_time_invested(skids=skid_1, conn = conn,
                                                          mode='SUM', treenodes=FALSE, connectors = FALSE))
  
  #test cases with no tags
  tempval <- catmaid_skids('annotation:single node no tag test Sri',conn = conn)
  skid_1 <-tempval[[1]]
  test_summode <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM')
  expect_equal(0,nrow(test_summode))
  
  test_overtimemode <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='OVER_TIME')
  expect_equal(0,nrow(test_overtimemode))
  
  test_actionsmode <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='ACTIONS')
  expect_equal(5,test_actionsmode[test_actionsmode$login == 'all_users','2018-02-12'])
  
  test_summode <- catmaid_get_time_invested(skids=skid_1, conn = conn,mode='SUM',connectors = FALSE)
  expect_equal(0,nrow(test_summode))
  
}))
