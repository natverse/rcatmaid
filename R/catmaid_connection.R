catmaid_connection<-function(server=getOption("catmaid.server"), 
                             authname=getOption("catmaid.authname"), 
                             authpassword=getOption("catmaid.authpassword"), 
                             username=getOption("catmaid.username"),
                             password=getOption("catmaid.password")) {
  if(is.null(server) || !grepl("^https", server)) stop("Must provide a valid https server")
  conn=list(server=server, authname=authname, authpassword=authpassword, username=username, password=password)
  class(conn)="catmaid_connection"
  conn
}

catmaid_login<-function(conn, ..., Force=FALSE){
  if(missing(conn)) conn=catmaid_connection(...)
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
  conn$cookies=cookies(conn$authresponse)
  conn
}
