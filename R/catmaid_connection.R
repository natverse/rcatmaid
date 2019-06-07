#' Connect/authenticate to a CATMAID server, returning a connection object
#' 
#' @description \code{catmaid_login} allows you to login to a CATMAID server 
#'   specified by a \code{catmaid_connection} object. If such an object is not 
#'   specified, then the last successful connection in this R session is reused 
#'   if possible otherwise a new connection object is created using 
#'   \code{options} of the form "catmaid.\*" or "catmaid_\*" (see details).
#'   
#'   The connection object returned by \code{catmaid_login} (or cached when 
#'   \code{Cache=TRUE}, the default) can then be used for future requests to the
#'   CATMAID server by get/query/fetch functions.
#'   
#' @details After successful login, the \code{catmaid_connection} object will 
#'   contain a \code{cookie} field that includes a sessionid that is required 
#'   for subsequent GET/POST operations.  When \code{Cache=TRUE} (the default) 
#'   the open connection object is cached and will be used when EITHER 
#'   \code{catmaid_login} is called with enough information to indicate that the
#'   same server is desired OR (when no information about the server is passed 
#'   to \code{catmaid_login}) the last opened connection will be used.
#'   
#' @section Token based authentication: CATMAID offers token based 
#'   authentication as the strongly preferred alternative to specifying you
#'   CATMAID username and password. See
#'   \url{http://catmaid.github.io/dev/api.html#api-token} for how to get an API
#'   token. You can then set the \code{catmaid.token} or \code{catmaid_token} package option, but no
#'   longer need to set the \code{catmaid.username} or \code{catmaid_username} and \code{catmaid.password} or \code{catmaid_password}
#'   options.
#'   
#'   Note that you must \bold{NOT} reveal this token e.g. by checking it into a 
#'   version controlled script as it gives complete access to your CATMAID 
#'   account.
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
#' @section Environment variables:
#'   
#'   You will very likely want to set the following environment variables in 
#'   your \code{.Renviron} file (see \code{\link{Startup}} for details). This 
#'   file is read by R on startup. In this way the catmaid package will 
#'   automatically login to your preferred CATMAID server. Note that environment
#'   variables will also be inherited by child R sessions. This means for
#'   example that they will be available when running knitr reports, tests or R
#'   CMD Check from Rstudio.
#'   
#'   \itemize{
#'   
#'   \item{\code{catmaid.server} or \code{catmaid_server}}
#'   
#'   \item{\code{catmaid.token} or \code{catmaid_token}} Preferred to using catmaid.username/password
#'   
#'   \item{\code{catmaid.authname} or \code{catmaid_authname}} Optional username for basic http website 
#'   authorisation
#'   
#'   \item{\code{catmaid.authpassword} or \code{catmaid_authpassword}} Optional password for basic http website
#'   authorisation
#'
#'   \item{\code{catmaid.username} or \code{catmaid_username}} Your catmaid username (deprecated in favour 
#'   of token)
#'   
#'   \item{\code{catmaid.password} or \code{catmaid_password}} Your catmaid password (deprecated in favour 
#'   of token)
#'   
#'   } An example \code{.Renviron} file might look like (the periods(.) in the environmental variable name can be also replaced
#'   with underscore(_) as mentioned above:
#'   
#'   \preformatted{
#'catmaid.server="https://mycatmaidserver.org/catmaidroot"
#'catmaid.token="9944b09199c62bcf9418ad846dd0e4bbdfc6ee4b"
#'catmaid.authname="Calvin"
#'catmaid.authpassword="hobbes"
#'   }
#'   
#'   and \bold{must} finish with a return at the end of the last line. See
#'   \url{http://catmaid.github.io/dev/api.html#api-token} for details of 
#'   obtaining an API token
#'   
#' @section Options: Although setting environment variables is now the recommended 
#'   approach, you can also set R startup options e.g. in your \code{.Rprofile}
#'   to specify default CATMAID login options including your personal access 
#'   token. The startup options have the same names as the environment variables
#'   listed above, so you can place code along the lines of:
#'   
#'   \code{options(catmaid.server="https://mycatmaidserver.org/catmaidroot", 
#'   catmaid.authname="Calvin",catmaid.authpassword="hobbes", catmaid.token = 
#'   "9944b09199c62bcf9418ad846dd0e4bbdfc6ee4b")}
#'   
#'   in your \code{.Rprofile} (see \code{\link{Startup}} for details). Note that
#'   it is important to have a final return at the end of your \code{.Rprofile}
#'   file.
#'   
#' @seealso \code{\link{options}}, \code{\link{Startup}}
#' @examples
#' \dontrun{
#' ## example explicitly specifying connection options
#' # using modern token based authentication
#' conn = catmaid_login(server="https://mycatmaidserver.org/catmaidroot",
#'   authname="Calvin",authpassword="hobbes", 
#'   token="9944b09199c62bcf9418ad846dd0e4bbdfc6ee4b")
#'   
#' # ... or using the old fashioned approach specifiy username/password
#' conn = catmaid_login(server="https://mycatmaidserver.org/catmaidroot",
#'   authname="Calvin",authpassword="hobbes", 
#'   username="calvin", password="hobbesagain")
#' 
#' ## examples assuming that catmaid.* environment variables/options are set
#' conn = catmaid_login()
#' conn = catmaid_login(server='https://someotherserver.org/catmaidroot')
#' 
#' ## set additional curl options/headers
#' # This example will bypass an SSL certificate verification error on the
#' # remote host e.g. if it has expired. Don't this regularly of course!
#' conn = catmaid_login(config=httr::config(ssl_verifypeer=0))
#' 
#' ## now do stuff with the connection like
#' skel=catmaid_fetch("1/10418394/0/0/compact-skeleton", conn=conn)
#' # you can also omit the connecttion because it will be cached and reused
#' skel=catmaid_fetch("1/10418394/0/0/compact-skeleton")
#' 
#' ## or for those who want to work at the lowest level
#' skel2=GET("https://mycatmaidserver.org/catmaidroot/1/10418394/0/0/compact-skeleton",
#'   config=conn$config)
#' }
#' @export
#' 
catmaid_login<-function(conn=NULL, ..., Cache=TRUE, Force=FALSE){
  if(is.character(conn) && grepl("^http", conn)) {
    # this looks like a server, probably because we are trying to connect to 
    # without other login details
    stop("To connect to : ", conn, ", you must name the server argument i.e.\n",
         sprintf('  catmaid_login(server="%s")', conn)) 
  }
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
    if(!is.null(conn$authresponse)) 
      return(invisible(conn))
    cached_conn=catmaid_cached_connection(conn)
    if(!is.null(cached_conn))
      return(invisible(cached_conn))
  }
  if(isTRUE(conn$nologin)) {
    # GET the CATMAID root for 2 reasons
    # 1. this checks that it actually exists
    # 2. we need to collect a CSRF token
    conn$authresponse=GET(url=conn$server)
    stop_for_status(conn$authresponse)
    
    # Now assuming that we could talk to the server we need to extract the CSRF
    # token. But it turns out that we need to pass the token twice
    # 1. once with the Cookie: header
    # 2. a second time we need to pass just the value with the X-CSRFToken header
    # Step 1 is handled generically for any kind of authresponse lower down
    # but Step 2 needs some special logic here
    res_cookies=cookies(conn$authresponse)
    csrf_row=grepl('csrf', res_cookies$name)
    if(any(csrf_row)) {
      token_value=res_cookies$value[csrf_row][1]
      conn$config=httr::add_headers('X-CSRFToken'=token_value, referer=conn$server)
    } else warning("I can't seem to find a CSRF token.",
              "You will not be able to POST to this site!")
  } else { 
    body <- if(is.null(conn$token))
      list(name=conn$username, pwd=conn$password) else NULL
    conn$authresponse = POST(url = paste0(conn$server, "/accounts/login"), 
                             body=body,
                             config = conn$config)
    stop_for_status(conn$authresponse)
  }
  
  # store the returned cookies for future use
  conn$cookies=unlist(cookies(conn$authresponse))
  conn$config=c(conn$config, set_cookies(conn$cookies))
  if(Cache)
    catmaid_cache_connection(conn)
  invisible(conn)
}

#' @name catmaid_login
#' @description \code{catmaid_connection} is a lower level function used by
#'   \code{catmaid_login} to create a connection object. End users should not
#'   need to call this directly, but it does document the arguments that can be
#'   used to specify a connection to a CATMAID server.
#' @param server url of CATMAID server
#' @param username,password Your CATMAID username and password.
#' @param token An API token (A modern alternative to providing your CATMAID
#'   username and password). See \bold{Token based authentication} for details.
#' @param authname,authpassword The http username/password that optionally
#'   secures the CATMAID website. These are not the same as your CATMAID login
#'   details.
#' @param authtype The http authentication scheme, see
#'   \code{\link[httr]{authenticate}} for details.
#' @param config Additional curl config options. See \code{\link[httr]{config}}
#'   for details and the examples section below.
#' @details Note the difference between \code{authname}/\code{authpassword} and
#'   \code{username}/\code{password}. The former are for generic web
#'   authentication, which is sometimes used to protect a private catmaid site
#'   from being accessible to general web traffic. The latter are used to
#'   authenticate to the CATMAID web application itself - for example the
#'   \code{username} is the one that will be associated with any tracing carried
#'   out by you in CATMAID.
#' @export
catmaid_connection<-function(server, username=NULL, password=NULL, authname=NULL, 
                             authpassword=NULL, token=NULL,
                             authtype=NULL, config=NULL) {
  # a bit of juggling to figure out which args could be passed and which
  # actually have been passed explicitly
  arglist=formals(fun = sys.function())
  argnames=names(arglist)
  m=match.call(definition = sys.function(), call = sys.call())
  passed_args=names(m[-1])
  
  # Set a default server if none specified
  defaultServer=unlist(getenvoroption("server"))
  if(missing(server)) {
    server=defaultServer
  }
  
  # construct basic connection list
  conn=list(server=server,
            username=username,
            password=password,
            authname=authname,
            authpassword=authpassword,
            token=token,
            authtype=authtype)[union("server", passed_args)]
  class(conn)="catmaid_connection"

  
  if(is.null(conn$server) || !grepl("^http[s]{0,1}", conn$server))
    stop("Must provide a valid https server")

  # Fill in the missing values using environment vars or options
  if(isTRUE(unname(conn$server==defaultServer))){
    missing_vars=setdiff(argnames, names(m))
    conn[missing_vars]=getenvoroption(missing_vars)
  }
  
  if(is.null(conn$username) && is.null(conn$token)) {
    conn$nologin=TRUE
    return(invisible(conn))
  }
  
  # make a curl config that includes any settings passed in 
  conn$config=c(config(), config)
  # then add authentication information if necessary
  if (!is.null(conn$authname)) {
    if (is.null(conn$authtype))
      conn$authtype = 'basic'
    conn$config = c(conn$config,
                    authenticate(conn$authname, conn$authpassword, type = conn$authtype))
  }
  if (!is.null(conn$token))
    conn$config = c(conn$config,
                    add_headers(`X-Authorization` = paste("Token", conn$token)))
  invisible(conn)
}

getenvoroption <- function(vars){
  fullvars=paste0(catmaid_envstr(), vars)
  res=Sys.getenv(fullvars, names = T, unset = NA)
  if(all(is.na(res))){
    # no env variables are set, let's try options
    res=do.call(options, as.list(fullvars))
  } else {
    # convert environment variables into options style list
    res=as.list(res)
    # replace missing values with NULL
    res=sapply(res, function(x) if(is.na(x)) NULL else x)
  }
  # give result the original variable names
  names(res)=vars
  res
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
#' @param simplifyVector Whether to use jsonlite::simplifyVector 
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
#' ## fetch user history since 1st April 2016
#' uh = catmaid_fetch('1/stats/history?start_date=2016-04-01', 
#'   simplifyVector = T, include_headers = F)
#' uh$date=as.Date(uh$date,"%Y%m%d")
#' library(dplyr)
#' # select top 10 users by nodes added
#' top10=uh %>% 
#'   group_by(name) %>% 
#'   summarise(total=sum(count)) %>% 
#'   arrange(desc(total)) %>%
#'   top_n(10)
#' # plot cumulative nodes traced
#' library(ggplot2)
#' uh %>% 
#'   group_by(name) %>%
#'   mutate(ccount = cumsum(count)) %>%
#'   filter(name %in% top10$name) %>% 
#'   ggplot(aes(date, ccount, col = name)) + geom_line()
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
#' @description \code{catmaid_connection_setenv} sets environment variables
#'   based on a \code{catmaid_connection} object.
#'
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
  names(export_vector)=paste0(catmaid_envstr(),  names(export_vector))
  all(do.call(Sys.setenv, as.list(export_vector)))
}

#' @description  #' \code{catmaid_connection_getenv} fetches appropriately named
#'   environment variables and returns them as named character vector.
#' @rdname catmaid_connection_setenv
#' @export
#' @importFrom stats na.omit
catmaid_connection_getenv<-function() {
  varnames=c("server", "username", "password", 
             "authname", "authpassword", "authtype", "token")
  catmaid_envnames=paste0(catmaid_envstr(), varnames)
  catmaid_envs=Sys.getenv(catmaid_envnames, unset = NA_character_)
  names(catmaid_envs)=varnames
  # drop any empty vars
  catmaid_envs=na.omit(catmaid_envs)
}

#' @description \code{catmaid_connection_unsetenv} unsets (or removes) the environment
#'   variables.
#' @rdname catmaid_connection_setenv
#' @export
catmaid_connection_unsetenv<-function(){
  vars=paste0(catmaid_envstr(),
              c("server", "username", "password", "authname", 
                "authpassword", "authtype", "token"))
  Sys.unsetenv(vars)
}


#' Return the CATMAID version running on the server
#' 
#' @details By default the version number for the current 
#'   \code{\link{catmaid_connection}} is stored on the first request after a new
#'   login and then the cached version number is reused. Setting 
#'   \code{cached=FALSE} will always force a request to the server.
#'   
#'   CATMAID versions are now produced by \bold{git describe} and look like 
#'   YYYY.MM.DD-XX-gaaaaaaa where aaaaaaa is a short SHA1 hash and XX is an 
#'   integer number of revisions since the last base version. Setting
#'   \code{numeric=TRUE} trims off the SHA1 hash leaving a string that can be
#'   interpreted as a four part version number (see examples).
#' @param conn A \code{catmaid_connection} object. The default value of NULL 
#'   implies that the most recent cached open connection will be used.
#' @param cached Whether to use the cached version number for this connection 
#'   (see details)
#' @param numeric Whether to parse the version string into an R 
#'   \code{numeric_version} object that can be used for comparisons.
#' @param ... Additional arguments passed to \code{\link{catmaid_fetch}}
#' @export
#' @return A character vector containing the version
#' @seealso \code{\link{numeric_version}}
#' @examples
#' \dontrun{
#' # example of checking if server version is newer than required version
#' catmaid_version(numeric=TRUE)>="2016.1.1-65"
#' }
catmaid_version <- function(conn=NULL, cached=TRUE, numeric=FALSE, ...) {
  conn=catmaid_login(conn = conn)
  version_regex="(-dev){0,1}-g[a-f0-9]{6,}$"
  if(!isTRUE(cached) || is.null(conn$catmaid.version)) {
    res=catmaid_fetch("/version", include_headers = F, simplifyVector = T, 
                      conn=conn, ...)
    # we just need the first return value
    conn$catmaid.version=res[[1]]
    nv <- try(numeric_version(sub(version_regex, "", conn$catmaid.version)),
              silent = TRUE)
    if(inherits(nv, 'try-error')){
      warning("CATMAID version: ", conn$catmaid.version, " could not be parsed!\n",
              "I will assume that you are running the latest version of CATMAID!")
      conn$catmaid.version <- "9999.12.31"
    }
    catmaid_cache_connection(conn)
  }
  if(!numeric){
    conn$catmaid.version
  } else {
    numeric_version(sub(version_regex, "", conn$catmaid.version))
  }
}

set_catmaid_version <- function(version, conn=NULL, ...) {
  catmaid_version(conn=conn)
  conn=catmaid_login(conn)
  conn$catmaid.version=version
  catmaid_cache_connection(conn)
}

#This local function returns the string to be used in matching the environment variable..
# either catmaid_ or catmaid.
catmaid_envstr <- function(){
  tempstr <- paste0("^", "catmaid", "(\\.|_)") #searching for strings beginning with catmaid_ or catmaid.
  matchvalues <- grep(pattern = tempstr, x = names(Sys.getenv()), value = TRUE)
  
  if (length(grep(pattern = paste0("^", "catmaid", "\\.") , x= matchvalues)) >= 1) {
    return("catmaid.")
  } else if (length(grep(pattern = paste0("^", "catmaid", "_") , x= matchvalues)) >= 1) {
    return("catmaid_")
  }else if (length(grep(pattern = paste0("^", "catmaid", "\\.") , x= matchvalues)) == 0 && 
            length(grep(pattern = paste0("^", "catmaid", "_") , x= matchvalues)) == 0) {
    simpleMessage("catmaid message: No usable environment variables found")
  } 
  else stop(paste("\ncatmaid error: Only found the environment variable -- ", matchvalues))
}


#' Get the CATMAID server in use
#'
#' @description shows the URL for a connection object in use (see \code{catmaid_login})
#' @param conn CATMAID connection object, see ?catmaid::catmaid_login for details
#' @param ... methods passed to catmaid::catmaid_login
#' @seealso \code{\link{catmaid_login}}
#' @export
#' @rdname catmaid_get_server
catmaid_get_server<-function(conn=NULL,...){
  if(is.null(conn)){
    conn = catmaid::catmaid_login()
  }
  conn$server
}
