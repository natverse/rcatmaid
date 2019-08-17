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

#' For \code{catmaid_remove_annotations_for_skeletons} a list containing
#' information about the annotations that have just been removed.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_annotations_for_skeletons(skids=c(10418394,4453485))
#' catmaid_get_annotations_for_skeletons("name:ORN (left|right)")
#' catmaid_get_annotations_for_skeletons("annotation:ORN PNs$")
#' }
#' @seealso \code{\link{catmaid_query_by_annotation}},
#'   \code{\link{catmaid_get_meta_annotations}} \code{\link{catmaid_fetch}},
#'   \code{\link{catmaid_skids}}
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

#' Get, query, set and remove CATMAID meta-annotations for annotations
#'
#' @description \code{catmaid_get_meta_annotations} Query by annotations to find
#'   the meta-annotations that label them.
#' @description \code{catmaid_query_meta_annotations} Query by meta-annotations
#'   to find the annotations that they label.
#' @description \code{catmaid_set_meta_annotations} Meta-annotate a group of
#'   CATMAID annotations
#' @description \code{catmaid_remove_meta_annotations} Remove meta-annotations
#'   from annotations.
#'
#' @param annotations annotation ids designating which annotations to
#'   meta-annotate. IDs can be found by calling
#'   \code{catmaid_get_annotationlist}. If a character string is given, then IDs
#'   will be found by calling \code{catmaid_get_annotationlist}.
#' @param meta_annotations meta-annotation to add to query. Either a vector of
#'   IDs or a character sting of meta-annotations can be given.
#' @param with_annotations whether or not to return the other meta-annotations
#'   of an annotation, when using \code{catmaid_query_meta_annotations}.
#' @param force Whether to force the catmaid server to remove multiple
#'   annotations (default \code{FALSE}, to provide some protection against
#'   accidents).
#' @inheritParams read.neuron.catmaid
#' @seealso \code{\link{catmaid_query_by_annotation}} which has some overlap in
#'   functionality, \code{\link{catmaid_get_annotations_for_skeletons}},
#'   \code{\link{catmaid_skids}}, \code{\link{catmaid_get_annotationlist}}
#' @export
#' @name catmaid_meta_annotations
#' @aliases catmaid_get_meta_annotations
#' @examples
#' \dontrun{
#' ## Against FAFB CATMAID server
#' catmaid_query_meta_annotations("ItoLee_Lineage")
#'
#' # note that this is similar to:
#' catmaid_query_by_annotation("^ItoLee_Lineage$", type = 'annotation')
#' }
catmaid_get_meta_annotations <-function(annotations, pid=1, conn=NULL,...){
  if(!possibly.numeric(annotations)){
    a <- catmaid_get_annotationlist(pid=pid, conn=conn, ...)
    annotations <- a$annotations[a$annotations$name%in%annotations,"id"]
  }
  if(!length(annotations)){
    stop("Please give at least one valid annotation or annotation ID for your chosen CATMAID instance.")
  }
  post_data <-  list()
  post_data[sprintf("object_ids[%d]", seq_along(annotations))] <- as.list(annotations)
  path <- sprintf("/%d/annotations/query", pid)
  res <- catmaid_fetch(path, body = post_data, include_headers = F,
                       simplifyVector = T, conn = conn, ...)
  invisible(catmaid_error_check(res))
  res
}

#' @export
#' @rdname catmaid_meta_annotations
catmaid_query_meta_annotations <-function(meta_annotations, 
                                      with_annotations = FALSE,
                                      pid=1, conn=NULL,...){
  if(!possibly.numeric(meta_annotations)){
    a <- catmaid_get_annotationlist(pid=pid, conn=conn, ...)
    meta_annotations <- a$annotations[a$annotations$name%in%meta_annotations,"id"]
  }
  if(!length(meta_annotations)){
    stop("Please give at least one valid meta annotation or meta annotation ID for your chosen CATMAID instance.")
  }
  post_data <- list()
  post_data[sprintf("annotated_with[%d]", seq_along(meta_annotations))] <- as.list(meta_annotations)
  post_data["with_annotations"] <- with_annotations
  post_data["types"] <- 'annotation'
  path <- sprintf("/%d/annotations/query-targets", pid)
  res <- catmaid_fetch(path, body = post_data, include_headers = F,
                      simplifyVector = T, conn = conn, ...)
  invisible(catmaid_error_check(res))
  res$entities
}

#' @export
#' @rdname catmaid_meta_annotations
catmaid_set_meta_annotations<-function(meta_annotations,annotations,pid=1,conn=NULL,...){
  if(!possibly.numeric(annotations)){
    a <- catmaid_get_annotationlist(pid=pid, conn=conn, ...)
    annotations <- a$annotations[a$annotations$name%in%annotations,"id"]
  }
  if(!length(annotations)){
    stop("Please give at least one valid annotation or annotation ID for your chosen CATMAID instance.")
  }
  post_data <- list()
  post_data[sprintf("entity_ids[%d]", seq_along(annotations))] <- as.list(annotations)
  path <- sprintf("/%d/annotations/add", pid)
  post_data[sprintf("annotations[%d]", seq_along(meta_annotations))] <- as.list(meta_annotations)
  res <- catmaid_fetch(path, body = post_data, include_headers = F,
                       simplifyVector = T, conn = conn, ...)
  invisible(catmaid_error_check(res))
}

#' @export
#' @rdname catmaid_meta_annotations
catmaid_remove_meta_annotations <-function(annotations, 
                                           meta_annotations,
                                           force=FALSE, 
                                           pid=1,
                                           conn=NULL, ...) {
  a <- 0
  if(!possibly.numeric(annotations)){
    a <- catmaid_get_annotationlist(pid=pid, conn=conn, ...)
    annotations <- a$annotations[a$annotations$name%in%annotations,"id"]
  }
  if(!possibly.numeric(meta_annotations)){
    if(a == 0){
      a <- catmaid_get_annotationlist(pid=pid, conn=conn, ...) 
    }
    meta_annotations <- a$annotations[a$annotations$name%in%meta_annotations,"id"]
  }
  if(!length(meta_annotations) | !length(annotations)){
    stop("Please give valid annotations or annotation IDs for your chosen CATMAID instance.")
  }
  post_data <-  list()
  post_data[sprintf("entity_ids[%d]", seq_along(annotations))]=as.list(annotations)
  if(length(meta_annotations)>1 && !force)
    stop("You must set force=TRUE when removing multiple meta annotations")
  post_data[sprintf("annotation_ids[%d]", seq_along(meta_annotations))]=as.list(meta_annotations)
  path=sprintf("/%d/annotations/remove", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, 
                    simplifyVector = T, ...)
  invisible(catmaid_error_check(res))
}

# hidden
possibly.numeric <- function(x) {
  stopifnot(is.atomic(x) || is.list(x))
  nNA <- sum(is.na(x))
  nNA.new <- suppressWarnings(sum(is.na(as.numeric(x))))
  nNA.new == nNA
}

#' Lock or unlock a CATMAID neuron reconstruction
#'
#' @description  Lock or unlock a CATMAID neuron reconstruction by adding or removing a 'locked' annotation to a set of skeleton IDs (skids). A locked neuron cannot be edited until it is unlocked.
#' @inheritParams read.neuron.catmaid
#' @export
#' @rdname catmaid_lock_neurons
catmaid_lock_neurons <- function(skids, pid = 1, conn = NULL, ...){
  skids = catmaid_skids(skids, pid=pid,conn=conn,...)
  catmaid_set_annotations_for_skeletons(skids, annotations = "locked", pid = pid,
                                                 conn = conn, ...)
}
#' @export
#' @rdname catmaid_lock_neurons
catmaid_unlock_neurons <- function(skids, pid = 1, conn = NULL, ...){
  skids = catmaid_skids(skids, pid=pid,conn=conn,...)
  catmaid_remove_annotations_for_skeletons(skids, annotations = "locked", pid = pid,
                                                    conn = conn, ...)
}
