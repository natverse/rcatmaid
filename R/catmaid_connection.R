#' Connect/authenticate to a CATMAID server, returning a connection object
#' 
#' @description \code{catmaid_login} allows you to login to a CATMAID server 
#'   specified by a \code{catmaid_connection} object. If such an object is not 
#'   specified, then one is created, by default using \code{options} of the form
#'   "catmaid.*".
#'   
#'   The connection object returned by \code{catmaid_login} will then be used 
#'   for future requests to the CATMAID server.
#'   
#' @details After successful login, the \code{catmaid_connection} will contain a
#'   \code{cookie} field that includes a sessionid that is required for 
#'   subsequent GET/POST operations.
#'   
#' @param conn An optional \code{catmaid_connection} connection object.
#' @param ... Additional arguemnts passed to catmaid_connection
#' @param Force Whether to force a new login to the CATMAID server (default 
#'   \code{FALSE})
#'   
#' @return a \code{catmaid_connection} object that can be used to make 
#'   authenticated http requests to a CATMAID server.
#'   
#' @section Package options:
#'   
#'   You will very likely want to set some of the following package options in
#'   your .Rprofile file (see \code{\link{Startup}} for details)
#'   
#'   \itemize{
#'   
#'   \item{catmaid.server}
#'   
#'   \item{catmaid.username}
#'   
#'   \item{catmaid.password}
#'   
#'   \item{catmaid.authname}
#'   
#'   \item{catmaid.authpassword}
#'   
#'   } using code along these lines:
#'   
#'   \code{options(catmaid.server="https://mycatmaidserver.org/catmaidroot",
#'   catmaid.authname="Calvin",catmaid.authpassword="hobbes",
#'   catmaid.username="calvin", catmaid.password="hobbesagain")}
#'   
#' @seealso \code{\link{options}}, \code{\link{Startup}}
catmaid_login<-function(conn=NULL, ..., Force=FALSE){
  if(is.null(conn)) conn=catmaid_connection(...)
  else if(!is.null(conn$authresponse) && !Force) {
    # Bail if we have already logged in (and don't want to login afresh)
    return(conn)
  }
  
  conn$authresponse=if(is.null(conn$authname)){
    POST(url = paste0(conn$server,"/accounts/login"),
           body=list(name=conn$username,pwd=conn$password))
  } else {
    POST(url = paste0(conn$server,"/accounts/login"),
           body=list(name=conn$username,pwd=conn$password),
           authenticate(conn$authname,conn$authpassword))
  }
  conn$cookies=unlist(cookies(conn$authresponse))
  conn
}

#' @name catmaid_login
#' @param server url of CATMAID server
#' @param username,password Your CATMAID username and password.
#' @param authname,authpassword The http basicauth username/password that
#'   optionally secures the CATMAID server.
catmaid_connection<-function(server=getOption("catmaid.server"), 
                             username=getOption("catmaid.username"),
                             password=getOption("catmaid.password"),
                             authname=getOption("catmaid.authname"), 
                             authpassword=getOption("catmaid.authpassword") ) {
  if(is.null(server) || !grepl("^https", server)) stop("Must provide a valid https server")
  conn=list(server=server, authname=authname, authpassword=authpassword, username=username, password=password)
  class(conn)="catmaid_connection"
  conn
}

