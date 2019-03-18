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

# nb will reuse cached connection made earlier
conn=try(catmaid_login(), silent = TRUE)

test_that("catmaid users", {
  expect_equal(catmaid_userids(1), 1L)
  expect_equal(catmaid_userids(1.00000000001), 1L)
  expect_equal(catmaid_userids(1:2), 1:2)
  expect_error(catmaid_userids(1.1))
  
  if(inherits(conn, 'try-error')) skip('No catmaid connection')
  ul=catmaid_get_user_list()
  l1=ul[['login']][1]
  i1=ul[['id']][1]
  expect_equal(catmaid_userids(c(l1, 'rhubarbcrumble')), c(i1, NA_integer_))  
})
