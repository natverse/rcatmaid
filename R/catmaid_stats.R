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
  skids=catmaid_skids(skids, conn = conn)
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

#' Fetch list of catmaid users for current/specified connection/project
#' 
#' @inheritParams read.neuron.catmaid
#' @export
#' @examples 
#' \dontrun{
#' catmaid_get_user_list()
#' }
#' @seealso \code{\link{catmaid_get_review_status}, 
#'   \link{catmaid_get_contributor_stats}}
catmaid_get_user_list<-function(pid=1, conn=NULL, ...){
  catmaid_fetch('user-list', simplifyVector = T, pid=pid, conn=conn, ...)
}
