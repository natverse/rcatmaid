#' return the raw data for a CATMAID neuronal skeleton
#' 
#' @param pid,skid project and skeleton ids
#' @param conn the \code{\link{catmaid_connection}} object
#' @param connectors Whether to fetch connector information
#' @param tags Whether to fetch tag information
#' @param raw Whether to return completely unprocessed data (when \code{TRUE}) 
#'   or to convert the nodes and connectors lists into processed data.frames 
#'   (when \code{FALSE}, the default)
#' @param ... Additional arguments passed to the \code{\link{catmaid_fetch}} 
#'   function.
#' @seealso \code{link{catmaid_fetch}}
#' @export
#' @examples
#' \dontrun{
#' ## using an existing connection object
#' conn=catmaid_login()
#' skel=catmaid_get_compact_skeleton(1, 10418394, conn)
#' # no connector (i.e. synapse) information
#' skel=catmaid_get_compact_skeleton(1, 10418394, conn, connectors = FALSE)
#' 
#' ## using throwaway connection object
#' skel=catmaid_get_compact_skeleton(1, 10418394)
#' }
catmaid_get_compact_skeleton<-function(pid, skid, conn=NULL, connectors = TRUE, tags = TRUE, raw=FALSE, ...) {
  path=file.path("", pid, skid, ifelse(connectors, 1L, 0L), ifelse(tags, 1L, 0L), "compact-skeleton")
  skel=catmaid_fetch(path, conn=conn, ...)
  names(skel)=c("nodes", "connectors", "tags")
  
  if(raw) return(skel)
  # else process the skeleton
  if(length(skel$nodes))
    skel$nodes=list2df(skel$nodes, 
                     cols=c("id", "parent_id", "user_id", "location.x",
                            "location.y", "location.z", "radius", "confidence"))
  
  if(length(skel$connectors))
    skel$connectors=list2df(skel$connectors, 
                            cols=c("XXX", "connector_id", "prepost", "location.x",
                                   "location.y", "location.z"))
  skel
}

list2df<-function(x, cols, use.col.names=F, ...) {
  if(!length(x)) return(NULL)
  l=list()
  for(i in seq_along(cols)) {
    colidx=if(use.col.names) cols[i] else i
    raw_col = sapply(x, "[[", colidx)
    if(is.list(raw_col)) {
      raw_col[sapply(raw_col, is.null)]=NA
      raw_col=unlist(raw_col)
    }
    l[[cols[i]]]=raw_col
  }
  as.data.frame(l, ...)
}

#' Return skeleton ids for pre/postsynaptic partners of a set of connector_ids
#' 
#' @param connector_ids Numeric ids for each connection
#' @inheritParams catmaid_get_compact_skeleton
#' @return A data.frame with columns \itemize{
#'   
#'   \item connector_id
#'   
#'   \item pre
#'   
#'   \item post
#'   
#'   }
#' @export
catmaid_get_connectors<-function(connector_ids, pid=1, conn=NULL, raw=FALSE, ...) {
  path=paste("", pid, "connector","skeletons",sep="/")
  post_data=as.list(connector_ids)
  names(post_data)=sprintf("connector_ids[%d]", seq_along(connector_ids))
  conns=catmaid_fetch(path, body=post_data, conn=conn, ...)
  
  if(raw) return(conns)
  # else process the connector information
  if(!length(conns)) return(NULL)

  # connector_ids
  ids=as.integer(sapply(conns, "[[", 1))
  # make indiviudal data.frames of synapse info in long form
  syns=lapply(conns, function(y) expand.grid(pre=unlist(y[[2]]['presynaptic_to'], use.names = F),
                                             post=unlist(y[[2]]['postsynaptic_to'], use.names = F)))
  # now assemble that all together
  df=data.frame(connector_id=rep(ids, sapply(syns, nrow)))
  cbind(df, do.call(rbind, syns))
}
