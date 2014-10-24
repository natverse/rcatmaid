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
  skel$nodes=list2df(skel$nodes, 
                     cols=c("id", "parent_id", "user_id", "location.x",
                            "location.y", "location.z", "radius", "confidence"))
  
  if(length(skel$connectors))
    skel$connectors=list2df(skel$connectors, 
                            cols=c("XXX", "connector_id", "prepost", "location.x",
                                   "location.y", "location.z"))
  skel
}

list2df<-function(x, cols) {  
  l=list()
  for(i in seq_along(cols)) {
    raw_col=sapply(x, "[[", i)
    if(is.list(raw_col)) {
      raw_col[sapply(raw_col, is.null)]=NA
      raw_col=unlist(raw_col)
    }
    l[[cols[i]]]=raw_col
  }
  as.data.frame(l)
}
