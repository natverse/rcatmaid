library(testthat)
library(catmaid)

conn<-try(catmaid:::catmaid_login())
if(inherits(conn,'try-error')) {
  message("Full tests depend on having a valid connection to a catmaid server.\n",
          "You must put your login details in a .Renviron file (recommended) OR",
          "login in to catmaid (using catmaid_login() and then export the connection details\n",
          "to environment variables using catmaid:::catmaid_connection_setenv().")
}

test_check("catmaid")
