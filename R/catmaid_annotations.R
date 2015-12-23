#' Get or set annotations for neurons from CATMAID
#' 
#' @inheritParams read.neuron.catmaid
#' @return For \code{catmaid_get_annotations_for_skeletons} a data.frame 
#'   containing the following columns \itemize{
#'   
#'   \item skid The skeleton id
#'   
#'   \item annotation The annotation string
#'   
#'   \item id The annotation id
#'   
#'   \item uid The user id for the annotation
#'   
#'   }
#'   
#'   For \code{catmaid_set_annotations_for_skeletons} a list containing 
#'   information about the annotations that have just been added.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_annotations_for_skeletons(skids=c(10418394,4453485))
#' catmaid_get_annotations_for_skeletons("name:ORN (left|right)")
#' catmaid_get_annotations_for_skeletons("annotation:ORN PNs$")
#' }
#' @seealso \code{\link{catmaid_fetch}}, \code{\link{catmaid_skids}}
catmaid_get_annotations_for_skeletons<-function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn)
  post_data=list()
  post_data[sprintf("skeleton_ids[%d]", seq_along(skids))]=as.list(skids)
  path=sprintf("/%d/annotations/forskeletons", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, 
                    simplifyVector = T, ...)
  
  # make a simple character vector with the annotation names
  al=unlist(res$annotations)
  aldf=data.frame(id=as.integer(names(al)), annotation=al)
  
  # rbind all the per skelenton data.frames into a single data.frame
  skdf=do.call(rbind, res$skeletons)
  # nb handle situation where neuron has no annotations
  nrows=sapply(res$skeletons, function(x) {nr=nrow(x); if(is.null(nr)) 0 else nr})
  skdf$skid=rep(as.integer(names(res$skeletons)), nrows)
  skdf=merge(skdf, aldf, by='id')
  skdf=skdf[c("skid", "annotation", "id", "uid")]
}

#' @export
#' @param annotations Character vector of one or more named annotations to add 
#'   to the specified neurons.
#' @details Note that annotations will be created on the catmaid server if they 
#'   do not already exist (and will not be re-created if they do already exist.)
#' @examples
#' \dontrun{
#' catmaid_set_annotations_for_skeletons(skids=c(10418394,4453485), 'myselection')
#' catmaid_set_annotations_for_skeletons(skids="annotation:ORN PNs$", 'my pns')
#' }
#' @rdname catmaid_get_annotations_for_skeletons
catmaid_set_annotations_for_skeletons<-function(skids, annotations, pid=1, 
                                                conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn)
  post_data=list()
  post_data[sprintf("skeleton_ids[%d]", seq_along(skids))]=as.list(skids)
  post_data[sprintf("annotations[%d]", seq_along(annotations))]=as.list(annotations)
  path=sprintf("/%d/annotations/add", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, 
                    simplifyVector = T, ...)
  invisible(res)
}
