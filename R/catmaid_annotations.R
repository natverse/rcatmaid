#' Get or set annotations for neurons from CATMAID
#' 
#' @description Pretty obviously: \code{catmaid_get_annotations_for_skeletons}
#'   gets annotations from one or more neurons.
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

#'   For \code{catmaid_remove_annotations_for_skeletons} a list containing 
#'   information about the annotations that have just been removed.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_annotations_for_skeletons(skids=c(10418394,4453485))
#' catmaid_get_annotations_for_skeletons("name:ORN (left|right)")
#' catmaid_get_annotations_for_skeletons("annotation:ORN PNs$")
#' }
#' @seealso \code{\link{catmaid_fetch}}, \code{\link{catmaid_skids}}
catmaid_get_annotations_for_skeletons<-function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  post_data=list()
  post_data[sprintf("skeleton_ids[%d]", seq_along(skids))]=as.list(skids)
  path=sprintf("/%d/annotations/forskeletons", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, 
                    simplifyVector = T, conn=conn, ...)
  
  # make a simple character vector with the annotation names
  al=unlist(res$annotations)
  # reorder according to our skid list (not the order returned by catmaid_fetch)
  res$skeletons=res$skeletons[as.character(skids)]
  
  # rbind all the per skeleton data.frames into a single data.frame
  skdf=do.call(rbind, res$skeletons)
  # nb handle situation where neuron has no annotations
  nrows=sapply(res$skeletons, function(x) {nr=nrow(x); if(is.null(nr)) 0 else nr})
  skdf$skid=rep(skids, nrows)
  # add in annotation names
  skdf$annotation=al[as.character(skdf$id)]
  # reorder columns
  skdf[c("skid", "annotation", "id", "uid")]
}

#' @export
#' @description Pretty obviously: \code{catmaid_set_annotations_for_skeletons} 
#'   sets annotations for one or more neurons. Although adding annotations is 
#'   non-destructive, \bold{please use carefully} since this will be making
#'   changes on the server!
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
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  post_data=list()
  post_data[sprintf("skeleton_ids[%d]", seq_along(skids))]=as.list(skids)
  post_data[sprintf("annotations[%d]", seq_along(annotations))]=as.list(annotations)
  path=sprintf("/%d/annotations/add", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, 
                    simplifyVector = T, conn=conn, ...)
  invisible(catmaid_error_check(res))
}

#' @rdname catmaid_get_annotations_for_skeletons
#' @export
#' @param force Whether to force the catmaid server to remove multiple 
#'   annotations (default \code{FALSE}) to provide some protection against 
#'   accidents.
#' @examples 
#' \dontrun{
#' nn=c(10418394,4453485)
#' catmaid_set_annotations_for_skeletons(skids=nn, 'mytest')
#' catmaid_remove_annotations_for_skeletons(nn, 'mytest')
#' }
catmaid_remove_annotations_for_skeletons<-function(skids, annotations,
                                                   force=FALSE, pid=1,
                                                   conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  eids=catmaid_entities_from_models(skids, conn=conn)
  if(!length(eids))
    stop("No entity ids founds from skids!")
  post_data=list()
  post_data[sprintf("entity_ids[%d]", seq_along(eids))]=as.list(eids)
  annotations=catmaid_aids(annotations)
  if(length(annotations)>1 && !force)
    stop("You must set force=TRUE when removing multiple annotations")
  post_data[sprintf("annotation_ids[%d]", seq_along(annotations))]=as.list(annotations)
  path=sprintf("/%d/annotations/remove", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, 
                    simplifyVector = T, ...)
  invisible(catmaid_error_check(res))
}

# internal function to check for error state in return values
# stops on error, otherwise returns input
catmaid_error_check <- function(x, stoponerror=TRUE){
  err_fields=c("error","type", "detail")
  if(length(x)>=3 & all(err_fields %in% names(x))){
    # looks like an error response, let's get name of calling function
    caller=sys.call(-1)[[1]]
    if(stoponerror)
      stop(caller, " of type: ", x$type, "\n  ", x$error, call. = FALSE)
    else
      return(TRUE)
  }
  if(stoponerror) x else FALSE
}

#' Return the entity ids for one or more model ids
#' @details This will normally be used to turn skeleton ids into neuron ids 
#'   which are used e.g. for annotation purposes. This is probably not something
#'   that many end users will need but is required e.g. by 
#'   \code{catmaid_remove_annotations_for_skeletons}.
#' @inheritParams read.neuron.catmaid
#' @export
#' @return An integer vector of \bold{entity ids} each named by the 
#'   corresponding \bold{model id} (usually a skeleton id).
#' @seealso Used by \code{\link{catmaid_remove_annotations_for_skeletons}}
catmaid_entities_from_models <- function(skids, pid = 1, conn = NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  post_data=list()
  post_data[sprintf("model_ids[%d]", seq_along(skids))]=as.list(skids)
  path=sprintf("/%d/neurons/from-models", pid)
  unlist(catmaid_fetch(path, body=post_data, include_headers = F, 
                    simplifyVector = T, ...))
}
