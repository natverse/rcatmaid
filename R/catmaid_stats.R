#' Get contributor statistics for neurons from CATMAID
#' 
#' @inheritParams read.neuron.catmaid
#' @return a list containing different statistics including construction and 
#'   review times (aggregated across all the specified input neurons). There 
#'   will also be 3 data.frames containing statistics for number of nodes and 
#'   pre/post-synaptic connectors broken down per user.
#'   
#'   \itemize{
#'   
#'   \item pre_contributors number of pre-synaptic connectors contributed per
#'   user.
#'   
#'   \item node_contributors number of skeleton nodes contributed per user.
#'   
#'   \item post_contributors number of post-synaptic connectors contributed per
#'   user.
#'   
#'   }
#'   
#' @export
#' @examples
#' \dontrun{
#' cs=catmaid_get_contributor_stats(skids=c(10418394,4453485))
#' # fetch user list
#' ul=catmaid_get_user_list()
#' # merge with list of node contributors and sort in descending order of
#' # contributions
#' library(dplyr)
#' left_join(cs$node_contributors, ul) %>%
#'   select(id,n, full_name) %>%
#'   arrange(desc(n))
#' }
#' @seealso \code{\link{catmaid_get_review_status}}
catmaid_get_contributor_stats<-function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  post_data=list()
  post_data[sprintf("skids[%d]", seq_along(skids)-1)]=as.list(skids)
  path=sprintf("/%d/skeleton/contributor_statistics_multiple", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, conn=conn, ...)
  # convert lists to data.frames
  fix_list<-function(l) {
    if(!is.list(l)) return(l)
    df=data.frame(id=as.integer(names(l)), n=unlist(l, use.names = FALSE))
  }
  lapply(res, fix_list)
}

#' Fetch or translate catmaid users for current/specified connection/project
#' 
#' @inheritParams read.neuron.catmaid
#' @export
#' @return For \code{catmaid_get_user_list} a \code{data.frame} with 
#' @examples 
#' \donttest{
#' head(catmaid_get_user_list())
#' }
#' @seealso \code{\link{catmaid_get_review_status}, 
#'   \link{catmaid_get_contributor_stats}}
catmaid_get_user_list<-function(pid=1, conn=NULL, ...){
  catmaid_fetch('user-list', simplifyVector = T, pid=pid, conn=conn, ...)
}

#' @description \code{catmaid_userids} convert login names to numeric ids
#' @param x A character vector of login names. If \code{x} contains valid
#'   integer (or numeric) ids then they will be returned as is.
#' @return For \code{catmaid_userids} an integer vector of numeric ids
#' @rdname catmaid_get_user_list
#' @importFrom checkmate check_integerish asInteger
#' @export
#' @examples 
#' \donttest{
#' catmaid_userids(1)
#' catmaid_userids('greg')
#' }
catmaid_userids <- function(x, pid=1, conn=NULL, ...) {
  if(is.numeric(x)) {
    checkmsg=check_integerish(x)
    
    if(!isTRUE(checkmsg))
      stop("You have given me numeric input but some elements are not integers!",
         "\n  user ids must always be integers! Details:\n", checkmsg)
    return(asInteger(x))
  }
  # otherwise translate login names to ids
  ul=catmaid_get_user_list(pid=pid, conn=conn)
  mx=match(x, ul[['login']])
  ul[['id']][mx]
}

#' Fetch user contribution history
#' 
#' @param from Starting date for history
#' @param to End date for history (defaults to today's date)
#' @inheritParams read.neuron.catmaid
#'   
#' @return A data.frame with columns \itemize{
#'   
#'   \item full_name
#'   
#'   \item login
#'   
#'   \item id
#'   
#'   \item new_cable (in nm)
#'   
#'   \item new_connectors
#'   
#'   \item new_reviewed_nodes
#'   
#'   \item date
#'   
#'   }
#' @export
#' @importFrom dplyr bind_rows right_join as_data_frame
#' @examples
#' \dontrun{
#' catmaid_user_history(from="2016-01-01")
#' # last 2 days
#' catmaid_user_history(from = Sys.Date()-2)
#' }
catmaid_user_history <- function(from, to=Sys.Date(), pid=1L, conn=NULL, ...) {
  fromd=as.Date(from)
  if(fromd<as.Date("2001-01-01")) 
    stop("Invalid date: ",from, ". See ?Date for valid formats")
  tod=as.Date(to)
  if(tod<as.Date("2001-01-01")) 
    stop("Invalid date: ",to, ". See ?Date for valid formats")
  u=sprintf("%d/stats/user-history?pid=1&start_date=%s&end_date=%s", pid, fromd, tod)
  cf=catmaid_fetch(u, conn=conn, simplifyVector = T, ...)
  
  ul=catmaid_get_user_list(pid=pid, conn=conn, ...)
  ll=lapply(cf$stats_table, process_one_user_history)
  df=bind_rows(ll)
  # comes in with name new_treenodes but this is not correct
  names(df)[1]="new_cable"
  df$uid=rep(as.integer(names(cf$stats_table)), sapply(ll, nrow))
  df=right_join(ul[c("full_name","login","id")], df, by=c(id="uid"))
  as.data.frame(df)
}


process_one_user_history <- function(x) {
  slx=sum(sapply(x, length))
  if(slx<1) {
    empytydf=dplyr::tibble(new_treenodes = numeric(0), new_connectors = integer(0), 
           new_reviewed_nodes = integer(0), date = structure(numeric(0), class = "Date"))
    return(empytydf)
  }
  df=bind_rows(lapply(x, as_data_frame))
  dates=as.Date(names(x), format="%Y%m%d")
  df$date=rep(dates, sapply(x, function(x) length(x)>0))
  df
}



#' Get meta information on nodes connected to a CATMAID connector node
#'
#' @description Get information on a CATMAID connector node
#' @param connector_id the ID for a connector of interest
#' @param node whether the connector is pre- or post-synaptic to the node of interest
#' @param pid project id. Defaults to 1
#' @param conn CATMAID connection object, see ?catmaid_login for details
#' @param ... methods passed to \code{catmaid_fetch} and \code{catmaid_get_treenode_detail}
#' @export
#' @rdname catmaid_connector_nodes
catmaid_connector_nodes <- function(connector_id, node = c("presynaptic","postsynaptic"),
                                    pid=1, conn = conn, ...){
  node = match.arg(node)
  connector_id = as.numeric(connector_id)
  if(length(connector_id)!=1){
    stop("connector_id must be a single connector_id")
  }
  post_data = list()
  post_data["connector_ids[0]"] = connector_id
  path = sprintf("/%d/connector/skeletons", pid)
  res = catmaid_fetch(path, body = post_data, include_headers = F,
                               simplifyVector = T, conn = conn,...)
  tnids = res[[1]][[2]][[paste0(node,"_to_node")]]
  detail = catmaid_get_treenodes_detail (tnids=tnids, pid = pid, conn = conn, ...)
  detail$connector_id = connector_id
  detail
}


### Added to rcatmaid ###
#' Get the UTC creation / edit time for a CATMAID node
#'
#' @description Get the UTC creation / edit time for a CATMAID treenode or connector. 
#' Useful for making 'state' arguments to be passed to other functions that edit data on a CATMAID server.
#' @param id a treenode or connector ID
#' @param time whether to return the creation_time or edition_time
#' @param pid project id. Defaults to 1
#' @param conn CATMAID connection object, see ?catmaid_login for details
#' @param ... methods passed to catmaid_set_labels
#' @export
#' @rdname catmaid_node_time
catmaid_node_time <- function(id, time = c("creation_time", "edition_time"), pid = 1, conn = NULL, ...){
  time = match.arg(time)
  id = as.numeric(id)
  post_data = list()
  post_data["node_ids"] = id
  path = sprintf("/%d/node/user-info", pid)
  res = catmaid_fetch(path, body = post_data, include_headers = F,
                               simplifyVector = T, conn = conn, ...)
  if(!is.null(res$error)){
    stop(res$error)
  }else{
    res[[1]][[time]]
  }
}

# A helper function, not exported
catmaid_convert_time <- function(utc){
  t = format(as.POSIXlt(utc,tz="GMT",origin="1970-01-01"), "%Y-%m-%d %H:%M:%OS3")
  s = unlist(strsplit(t," "))
  t = paste0(s[1],"T",s[2],"Z")
}

