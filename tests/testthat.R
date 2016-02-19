library(testthat)
library(catmaid)

conn<-try(catmaid:::catmaid_connection_getenv())
if(inherits(conn,'try-error')) {
  message("Full tests depend on having a valid connection to a catmaid server.\n",
          "You must login in to catmaid (using catmaid_login() or by putting\n",
          "login details in your .Rprofile) and then export the connection details\n",
          "to environment variables using catmaid:::catmaid_connection_setenv().")
}

test_check("catmaid")
