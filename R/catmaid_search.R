# Searching for CATMAID neurons

#' Search for CATMAID skeletons within a volume
#'
#' @description  Programmatically search for skeleton IDs pertaining to neurons within a search volume defined by a bounding box.
#' @param bbx the bounding box (a matrix of 2 rows and 3 columns) describing a search volume
#' @param min_nodes the minimum number of nodes a neuron in the search area must have (includes nodes outside search area)
#' @param pid project id. Defaults to 1
#' @param conn CATMAID connection object, see ?catmaid::catmaid_login for details
#' @param ... methods passed to catmaid::catmaid_fetch
#' @export
#' @rdname catmaid_skeletons_in_bbx
catmaid_skeletons_in_bbx <- function(bbx, min_nodes = 2, pid = 1, conn = NULL, ...){
  post_data = list()
  post_data["minx"] = bbx[1,1]
  post_data["miny"] = bbx[1,2]
  post_data["minz"] = bbx[1,3]
  post_data["maxx"] = bbx[2,1]
  post_data["maxy"] = bbx[2,2]
  post_data["maxz"] = bbx[2,3]
  post_data["min_nodes"] = min_nodes
  path = sprintf("/%d/skeletons/in-bounding-box", pid)
  res = catmaid::catmaid_fetch(path, body = post_data, include_headers = F,
                               simplifyVector = T, conn = conn, ...)
  res
}


#' Get the CATMAID neuron ID that corresponds to the skeleton ID
#'
#' @description Retrieve the neuron IDs for given skeleton IDs. This is typically the skeleton ID + 1, and is often, but not always accurately, kept by CATMAID tracers in the name of a neuron.
#' @param skids a vector of skeleton IDs or argument applicable to \code{catmaid_get_neuronid}
#' @param pid project id. Defaults to 1
#' @param conn CATMAID connection object, see \code{catmaid::catmaid_login} for details
#' @param ... methods passed to  \code{catmaid_fetch}
#' @export
#' @rdname catmaid_get_neuronid
catmaid_get_neuronid <- function(skids, pid = 1, conn = NULL, ...){
  skids = catmaid_skids(skids, conn = conn, pid = pid, ...)
  if (any(duplicated(skids))) {
    uskids = unique(skids)
    unids = catmaid_get_neuronid(uskids, pid = pid, conn = conn,...)
    res = unids[match(skids, uskids)]
    return(res)
  }
  skids[is.na(skids)] = -1L
  res = lapply(skids,function(skid)
    catmaid::catmaid_fetch(sprintf("/%d/skeleton/%s/neuronname", pid, skid), body = NULL, include_headers = F,
                           conn = conn, ...)$neuronid)
  res = sapply(res,function(r) ifelse(is.null(r),NA,r))
  names(res) = skids
  res
}
