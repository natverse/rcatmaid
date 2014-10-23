#' return the raw data for a CATMAID neuron skeleton
#' 
catmaid_get_compact_skeleton<-function(pid, skid, connectors = TRUE, tags = TRUE, conn=NULL, raw=FALSE, ...) {
  path=file.path("", pid, skid, ifelse(connectors, 1L, 0L), ifelse(tags, 1L, 0L), "compact-skeleton")
  skel=catmaid_GETJ(path, conn=conn, ...)
  names(skel)=c("nodes", "connectors", "tags")
  
  if(raw) return(skel)
  # else process the skeleton
  skel$nodes=list2df(skel$nodes, 
                     cols=c("id", "parent_id", "user_id", "location.x",
                            "location.y", "location.z", "radius", "confidence"))
  
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
