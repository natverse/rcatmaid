fakeconn <-
  structure(
    list(
      server = "https://fakecatmaid.org",
      nologin = TRUE,
      authresponse = TRUE,
      config=httr::config()
    ),
    .class = "catmaid_connection"
  )
