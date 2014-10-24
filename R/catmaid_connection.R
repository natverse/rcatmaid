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
#'   authenticated http requests to a CATMAID server, specifically by making
#'   use of the \code{$config} field.
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
#' @examples
#' \dontrun{
#' ## example explicitly specifying connection options
#' conn = catmaid_login(server="https://mycatmaidserver.org/catmaidroot",
#'   authname="Calvin",authpassword="hobbes", 
#'   username="calvin", password="hobbesagain")
#' 
#' ## examples assuming that catmaid.* options have been set
#' conn = catmaid_login()
#' conn = catmaid_login(server='https://someotherserver.org/catmaidroot')
#' 
#' ## now do stuff with the connection like
#' skel=catmaid_GETJ("1/10418394/0/0/compact-skeleton", conn)
#' 
#' ## or for those who want to work at the lowest level
#' skel2=GET("https://mycatmaidserver.org/catmaidroot/1/10418394/0/0/compact-skeleton",
#'   config=conn$config)
#' }
#' @export
catmaid_login<-function(conn=NULL, ..., Force=FALSE){
  if(is.null(conn)) conn=catmaid_connection(...)
  else if(!is.null(conn$authresponse) && !Force) {
    # Bail if we have already logged in (and don't want to login afresh)
    return(conn)
  }
  # make a custom curl config that includes authentication information if necessary
  conn$config = if(is.null(conn$authname)) config() else authenticate(conn$authname, conn$authpassword)
  conn$authresponse = POST(url = paste0(conn$server,"/accounts/login"), 
                           body=list(name=conn$username,pwd=conn$password),
                           config = conn$config)
  # store the returned cookies for future use
  conn$cookies=unlist(cookies(conn$authresponse))
  conn$config=c(conn$config, set_cookies(conn$cookies))
  conn
}

#' @name catmaid_login
#' @param server url of CATMAID server
#' @param username,password Your CATMAID username and password.
#' @param authname,authpassword The http basicauth username/password that
#'   optionally secures the CATMAID server.
#' @export
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

#' Send http GET or POST request to a CATMAID server
#' 
#' @description \code{catmaid_GET/POST} returns the raw response, whereas 
#'   \code{catmaid_GET/POSTJ} parses JSON content into an R object.
#'   
#' @param path The path on the CATMAID server relative to the CATMAID root
#' @param body For \code{catmaid_POST(J)} the body of the post request, usually 
#'   in the form of a named list. See the \code{\link[httr]{POST}} documentation
#'   for full details.
#' @param conn A \code{catmaid_connection} objection returned by 
#'   \code{\link{catmaid_login}}. If \code{NULL} (the default) a new connection 
#'   object will be generated using the values of the \bold{catmaid.*} package 
#'   options as described in the help for \code{\link{catmaid_login}}.
#' @param include_headers Whether to include basic headers from the http request
#'   as attributes on the parsed JSON object (default \code{TRUE}).
#' @param ... Additional arguments passed to the \code{httr::GET} function
#' @return For \code{catmaid_GET} an object of class \code{response} or, for for
#'   \code{catmaid_GETJ}, the raw R object generated by calling 
#'   \code{jsonlite::\link[jsonlite]{fromJSON}} on the body of the response.
#' @examples
#' \dontrun{
#' ## Make a catmaid_connection object to use for these requests
#' conn=catmaid_login()
#' 
#' ## fetch a demo skeleton using a GET request
#' # raw response
#' skel.response=catmaid_GET("1/10418394/0/0/compact-skeleton", conn)
#' # list object
#' skel=catmaid_GETJ("1/10418394/0/0/compact-skeleton", conn)
#' 
#' ## Get the names of two skeletons using a POST request
#' # NB that the skids[n] elements are quoted when constructing the list since 
#' # they are not valid R names.
#' catmaid_POSTJ("/1/skeleton/neuronnames", conn=conn,
#'   body=list(pid=1, 'skids[1]'=10418394, 'skids[2]'=4453485))
#' }
#' @seealso \code{\link{catmaid_login}}, \code{\link[httr]{GET}}, 
#'   \code{\link[httr]{POST}}
catmaid_GET <- function(path, conn=NULL, ...) {
  conn=catmaid_login(conn)
  req <- GET(url=paste0(conn$server, path), 
             set_cookies(.cookies=conn$cookies),
             authenticate(conn$authname,conn$authpassword), ...)
  req
}

#' @rdname catmaid_GET
catmaid_GETJ<-function(path, conn=NULL, include_headers=TRUE, ...) {
  req=catmaid_GET(path, conn=conn, ...)
  parsed=catmaid_parse_json(req)
  if(include_headers) {
    fields_to_include=c("url", "headers")
    attributes(parsed) = c(attributes(parsed), req[fields_to_include])
  }
  parsed
}

catmaid_parse_json <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' @rdname catmaid_GET
#' @export
catmaid_POST <- function(path, body, conn=NULL, ...) {
  conn=catmaid_login(conn)
  req <- POST(url=paste0(conn$server, path), body=body,
              set_cookies(.cookies=conn$cookies),
              authenticate(conn$authname,conn$authpassword), ...)
  req
}

#' @rdname catmaid_GET
#' @export
catmaid_POSTJ<-function(path, body, conn=NULL, include_headers=TRUE, ...) {
  req=catmaid_POST(path, body=body, conn=conn, ...)
  parsed=catmaid_parse_json(req)
  if(include_headers) {
    fields_to_include=c("url", "headers")
    attributes(parsed) = c(attributes(parsed), req[fields_to_include])
  }
  parsed
}
