#' Connect/authenticate to a CATMAID server, returning a connection object
#' 
#' @description \code{catmaid_login} allows you to login to a CATMAID server 
#'   specified by a \code{catmaid_connection} object. If such an object is not 
#'   specified, then the last succesful connection in this R session is reused 
#'   if possible otherwise a new connection object is created using 
#'   \code{options} of the form "catmaid.*" (see details).
#'   
#'   The connection object returned by \code{catmaid_login} (or cached when 
#'   \code{Cache=TRUE}, the default) can then be used for future requests to the
#'   CATMAID server by get/query/fetch functions.
#'   
#' @details After successful login, the \code{catmaid_connection} object will 
#'   contain a \code{cookie} field that includes a sessionid that is required 
#'   for subsequent GET/POST operations.  When \code{Cache=TRUE} (the default) 
#'   the open connection object is cached and will be used when EITHER 
#'   catmaid_login is called with enough information to indicate that the same 
#'   server is desired OR (when no information about the server is passed to 
#'   catmaid_login) the last opened connection will be used.
#'   
#' @section Token based authentication: CATMAID now offers token based 
#'   authentication as an alternative to specifying you CATMAID username and 
#'   password. See \url{http://catmaid.github.io/dev/api.html#api-token} for how
#'   to get an API token. You can then set the catmaid.token package option. 
#'   Note that you must \bold{NOT} reveal this token e.g. by checking it into a 
#'   version control script.
#'   
#' @param conn An optional \code{catmaid_connection} connection object.
#' @param ... Additional arguments passed to catmaid_connection
#' @param Cache Whether to cache open connections at login so that they can be 
#'   reused automatically.
#' @param Force Whether to force a new login to the CATMAID server (default 
#'   \code{FALSE})
#'   
#' @return a \code{catmaid_connection} object that can be used to make 
#'   authenticated http requests to a CATMAID server, specifically by making use
#'   of the \code{$config} field.
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
#'   \item{catmaid.token} An alternative to using catmaid.username/password
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
#'   # or if you are using an API token # see 
#'   http://catmaid.github.io/dev/api.html#api-token 
#'   \code{options(catmaid.server="https://mycatmaidserver.org/catmaidroot", 
#'   catmaid.authname="Calvin",catmaid.authpassword="hobbes", catmaid.token = 
#'   "9944b09199c62bcf9418ad846dd0e4bbdfc6ee4b")}
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
#' skel=catmaid_fetch("1/10418394/0/0/compact-skeleton", conn=conn)
#' 
#' ## or for those who want to work at the lowest level
#' skel2=GET("https://mycatmaidserver.org/catmaidroot/1/10418394/0/0/compact-skeleton",
#'   config=conn$config)
#' }
#' @export
catmaid_login<-function(conn=NULL, ..., Cache=TRUE, Force=FALSE){
  if(is.null(conn)) {
    if(!length(pairlist(...))) {
      # try and get the last cached connection
      conn=catmaid_last_connection()
    }
    if(is.null(conn))
      conn=catmaid_connection(...)
  }
  
  # check if we can use this connection already or reuse a cached one
  if(!Force) {
    # already open if authresponse exists
    if(!is.null(conn$authresponse)) return(conn)
    cached_conn=catmaid_cached_connection(conn)
    if(!is.null(cached_conn)) return(cached_conn)
  }

  # otherwise login from scratch
  if(is.null(conn$token)){
    conn$authresponse = POST(url = paste0(conn$server,"/accounts/login"), 
                             body=list(name=conn$username,pwd=conn$password),
                             config = conn$config)
  } else {
    conn$authresponse = POST(url = paste0(conn$server,"/accounts/login"),
                             config = conn$config)
  }
  
  stop_for_status(conn$authresponse)
  
  # store the returned cookies for future use
  conn$cookies=unlist(cookies(conn$authresponse))
  conn$config=c(conn$config, set_cookies(conn$cookies))
  if(Cache)
    catmaid_cache_connection(conn)
  invisible(conn)
}

#' @name catmaid_login
#' @param server url of CATMAID server
#' @param username,password Your CATMAID username and password.
#' @param token An API token (A modern alternative to provifing your CATMAID username and password). See \bold{Token based authentication} for details.
#' @param authname,authpassword The http username/password that optionally
#'   secures the CATMAID server.
#' @param authtype The http authentication scheme, see
#'   \code{\link[httr]{authenticate}} for details.
#' @export
catmaid_connection<-function(server, username=NULL, password=NULL, authname=NULL, 
                             authpassword=NULL, token=NULL,
                             authtype=getOption("catmaid.authtype", default = "basic")) {
  
  defaultServer=getOption("catmaid.server")
  if(missing(server)) {
    server=defaultServer
  }
  if(is.null(server) || !grepl("^http[s]{0,1}", server))
    stop("Must provide a valid https server")
  
  if(isTRUE(server==defaultServer)){
    # we're using the default server specified by options
    # fill in the other options
    if(is.null(username)) username=getOption("catmaid.username")
    if(is.null(password)) password=getOption("catmaid.password")
    if(is.null(authname)) authname=getOption("catmaid.authname")
    if(is.null(authpassword)) authpassword=getOption("catmaid.authpassword")
    if(is.null(token)) token=getOption("catmaid.token")
  }
  
  conn=list(server=server, username=username, password=password, 
            authtype=authtype, authname=authname, authpassword=authpassword, token=token)
  # make a custom curl config that includes authentication information if necessary
  conn$config = if(is.null(authname)) config() else {
    authenticate(authname, authpassword, type = authtype)
  }
  if(!is.null(token))
    conn$config=c(conn$config, 
                  add_headers(`X-Authorization`=paste("Token", token)))

  class(conn)="catmaid_connection"
  conn
}

#' Send http GET or POST request to a CATMAID server
#' 
#' @description \code{catmaid_fetch} carries out a GET operation when 
#'   \code{body=NULL}, POST otherwise. The http status code of the response will
#'   be checked - if invalid an error will be thrown.
#'   
#' @param path The path on the CATMAID server relative to the CATMAID root
#' @param body The (optional) body of the post request, usually in the form of a
#'   named list. See the \code{\link[httr]{POST}} documentation for full 
#'   details.
#' @param conn A \code{catmaid_connection} objection returned by 
#'   \code{\link{catmaid_login}}. If \code{NULL} (the default) a new connection 
#'   object will be generated using the values of the \bold{catmaid.*} package 
#'   options as described in the help for \code{\link{catmaid_login}}.
#' @param parse.json Whether or not to parse a JSON response to an R object 
#'   (default \code{TRUE})
#' @param include_headers Whether to include basic headers from the http request
#'   as attributes on the parsed JSON object (default \code{TRUE}) when 
#'   \code{parse.json=TRUE}.
#' @param simplifyVector Wheter to use jsonlite::simplifyVector 
#' @param ... Additional arguments passed to the \code{httr::GET} or 
#'   \code{httr::POST} function
#' @return When \code{parse.json=FALSE} an object of class \code{response} 
#'   otherwise the raw R object generated by calling 
#'   \code{jsonlite::\link[jsonlite]{fromJSON}} on the body of the response.
#' @export
#' @seealso \code{\link{catmaid_login}}, \code{\link[httr]{GET}}, 
#'   \code{\link[httr]{POST}}
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' ## Make a catmaid_connection object to use for these requests
#' conn=catmaid_login()
#' 
#' ## fetch a demo skeleton using a GET request
#' # raw response
#' skel.response=catmaid_fetch("1/10418394/0/0/compact-skeleton", conn=conn)
#' # list object
#' skel=catmaid_fetch("1/10418394/0/0/compact-skeleton", conn=conn)
#' 
#' ## Get the names of two skeletons using a POST request
#' # NB that the skids[n] elements are quoted when constructing the list since 
#' # they are not valid R names.
#' catmaid_fetch("/1/skeleton/neuronnames", conn=conn,
#'   body=list(pid=1, 'skids[1]'=10418394, 'skids[2]'=4453485))
#' 
#' ## get all skeletons with more than 1000 nodes
#' skids=as.integer(catmaid_fetch("/1/skeletons/?nodecount_gt=1000"))
#' 
#' ## demonstrate that bad urls will result in an error
#' catmaid_fetch("/1/rhubarb/crumble")
#' }
catmaid_fetch<-function(path, body=NULL, conn=NULL, parse.json=TRUE, 
                        include_headers=TRUE, simplifyVector=FALSE, ...) {
  conn=catmaid_login(conn)
  req<-with_config(conn$config, {
    if(is.null(body)) {
      GET(url=file.path(conn$server, path, fsep="/"), ...)
    } else {
      POST(url=file.path(conn$server, path, fsep="/"), body=body, ...)
    }
  } )
  # error out if there was a problem
  stop_for_status(req)
  if(parse.json) {
    parsed=catmaid_parse_json(req, simplifyVector=simplifyVector)
    if(length(parsed)==2 && isTRUE(names(parsed)[2]=='error')) {
      stop("catmaid error: " , parsed$error)
    }
    if(include_headers) {
      fields_to_include=c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  } else req
}

catmaid_parse_json <- function(req, simplifyVector = FALSE, ...) {
  text <- content(req, as = "text", encoding = "UTF-8")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  fromJSON(text, simplifyVector = simplifyVector, ...)
}

# return the open cached connection object for an (unopened) connection
# returns NULL when passed NULL
# returns incoming connection if already opened
catmaid_cached_connection<-function(conn) {
  if(is.null(conn)) return(NULL)
  
  open_connections=names(.package_statevars$connections)
  if(!length(open_connections)) return(NULL)
  
  for(thisconn in open_connections) {
    thisconn=.package_statevars$connections[[thisconn]]
    checkfields=c("server","username","authname", "authtype")
    # drop any fields where the incoming connection does not contain info
    checkfields=checkfields[!sapply(conn[checkfields], is.null)]
    if(isTRUE(all.equal(thisconn[checkfields], conn[checkfields])))
      return(thisconn)
  }
  return(NULL)
}

catmaid_cache_connection<-function(conn) {
  .package_statevars$connections[[catmaid_connection_fingerprint(conn)]]=conn
}

catmaid_last_connection<-function() {
  conns=.package_statevars$connections
  num_conns=length(conns)
  if(num_conns) conns[[num_conns]] else NULL
}

# fingerprint of a connection consisting of server, username and cookies
catmaid_connection_fingerprint<-function(conn) {
  paste(c(conn$server, conn$username, cookies(conn$authresponse)), collapse = "")
}

#' Import/Export catmaid connection details to system variables (e.g. for tests)
#' 
#' @description
#' 
#' \code{catmaid_connection_setenv} sets environment variables based on a 
#' \code{catmaid_connection} object.
#' 
#' \code{catmaid_connection_getenv} fetches appropriately named environment 
#' variables and uses them to open a catmaid connection.
#' 
#' \code{catmaid_connection_unsetenv} unsets the environment variables.
#' @param conn A \code{catmaid_connection} object. The default value of NULL 
#'   implies that the most recent cached open connection will be used.
#' @param ... additional arguments passed to \code{catmaid_login}
#' @details \code{catmaid_connection_setenv} will attempt to login if this has 
#'   not already been done
#' @return \code{catmaid_connection_setenv} returns TRUE or FALSE depending on 
#'   whether variables were set successfully. \code{catmaid_connection_getenv} 
#'   returns a connection object created based on environment variables.
#' @seealso \code{\link{catmaid_login}}
#' @export
catmaid_connection_setenv<-function(conn=NULL, ...) {
  conn=catmaid_login(conn, ...)
  poss_vars_to_export=c("server", "username", "password", 
                        "authname", "authpassword", "authtype", "token")
  vars_to_export=intersect(poss_vars_to_export, names(conn))
  export_vector=unlist(conn[vars_to_export])
  names(export_vector)=paste0("catmaid.",  names(export_vector))
  all(do.call(Sys.setenv, as.list(export_vector)))
}

#' Fetch connection details from appropriate environment variables
#' @rdname catmaid_connection_setenv
#' @export
#' @importFrom stats na.omit
catmaid_connection_getenv<-function(...) {
  varnames=c("server", "username", "password", 
             "authname", "authpassword", "authtype", "token")
  catmaid_envnames=paste0("catmaid.", varnames)
  catmaid_envs=Sys.getenv(catmaid_envnames, unset = NA_character_)
  names(catmaid_envs)=varnames
  # drop any empty vars
  catmaid_envs=na.omit(catmaid_envs)
  do.call(catmaid_login, as.list(catmaid_envs, ...))
}

#' Unset catmaid connection environment variables
#' @rdname catmaid_connection_setenv
#' @export
catmaid_connection_unsetenv<-function(){
  vars=paste0("catmaid.",
              c("server", "username", "password", "authname", 
                "authpassword", "authtype", "token"))
  Sys.unsetenv(vars)
}


#' Return the CATMAID version running on the server
#' 
#' @details By default the version number for the current 
#'   \code{\link{catmaid_connection}} is stored on the first request after a new
#'   login and then the cached version number is reused. Setting 
#'   \code{cached=FALSE} will always force a request to the server. CATMAID 
#'   versions now appear to be named as YYYY.MM.DD and can therefore be 
#'   interpreted as a date or a tripartite version number (see examples).
#' @param conn A \code{catmaid_connection} object. The default value of NULL 
#'   implies that the most recent cached open connection will be used.
#' @param cached Whether to use the cached version number for this connection 
#'   (see details)
#' @param ... Additional arguments passed to \code{\link{catmaid_fetch}}
#' @export
#' @return A character vector containing the version
#' @examples
#' \dontrun{
#' # example of checking if server version is newer than required version
#' current_v=numeric_version(catmaid_version())
#' current_v
#' required_v=numeric_version("2016.1.1")
#' current_v >= required_v
#' }
catmaid_version <- function(conn=NULL, cached=TRUE, ...) {
  conn=catmaid_login(conn = conn)
  if(!isTRUE(cached) || is.null(conn$version)) {
    res=catmaid_fetch("/version", include_headers = F, simplifyVector = T, 
                      conn=conn, ...)
    # we just need the first return value
    conn$catmaid.version=res[[1]]
    catmaid_cache_connection(conn)
  }
  conn$catmaid.version
}
