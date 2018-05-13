#' Get statistics for all labels in a project from a CATMAID server
#' 
#' @inheritParams read.neuron.catmaid
#' 
#' @export
#' @examples
#' \donttest{
#' label_stats=catmaid_get_label_stats()
#' library(dplyr)
#' # select soma labels
#' soma_labels=label_stats %>%
#'   filter(labelName=='soma') %>%
#'   group_by(skeletonID)
#' 
#' # select skeleton ids for neurons with multiple cell bodies
#' multiple_soma=soma_labels %>%
#'   count(skeletonID) %>%
#'   filter(n>1) %>%
#'   arrange(desc(n))
#' 
#' multiple_soma_info = soma_labels %>% 
#'   filter(skeletonID%in% multiple_soma$skeletonID)
#' }
#' @seealso \code{\link{catmaid_get_review_status}}
#' @family labels
catmaid_get_label_stats<-function(pid=1, conn=NULL, ...) {
  # see https://github.com/catmaid/CATMAID/blob/eec50d6c3532ad92ce1f511f39e5aed66c6297e5/django/applications/catmaid/control/label.py#L99
  res=catmaid_fetch(paste0(pid, '/labels/stats'), include_headers = F, conn=conn, ...)
  list2df(res, c("labelID", "labelName", "skeletonID", "treenodeID"))
}


#' Get/Set/Remove CATMAID treenode labels (aka tags)
#'
#' @description \code{catmaid_get_labels} gets labels (tags) for specified
#'   nodes.
#'
#' @param treenodes,connectors Integer ids of nodes to be queried
#' @inheritParams read.neuron.catmaid
#'
#' @export
#' @return a data.frame with columns \itemize{
#'
#'   \item id (integer)
#'
#'   \item type (treenode or connector)
#'
#'   \item label (the tag)
#'
#'   }
#'
#'   A zero row data.frame will be returned if there are no results.
#' @family labels
catmaid_get_labels <- function(treenodes=NULL, connectors=NULL,
                               pid=1, conn=NULL, ...) {
  path=file.path(pid, "labels-for-nodes", fsep="/")
  body=list()
  if(length(treenodes)) body[['treenode_ids']]=paste(treenodes, collapse=",")
  if(length(connectors)) body[['connector_ids']]=paste(connectors, collapse=",")
  
  res=catmaid_fetch(path, body = body, conn = conn, ...)
  nres=length(res)
  nelements=if(nres) sapply(res, length) else 0
  types=if(nres) ifelse(names(res) %in% treenodes, 'treenode', 'connector') else character()
  data.frame(id=rep(as.integer(names(res)), nelements),
             type=rep(types, nelements),
             label=if(nres) unlist(res, use.names = F) else character())
}

# @description \code{catmaid_set_labels} sets labels (tags) for specified
#   nodes.
# @export
catmaid_set_labels <- function(node, labels, type=c("treenode", "connector"),
                               delete_existing=FALSE, pid=1, conn=NULL, ...) {
  type=match.arg(type)
  path=file.path(pid, "label", type, node, "update", fsep="/")
  if(any(grepl(",", labels, fixed = T)))
    stop("CATMAID cannot accept labels containing commas")
  body=list(ntype=type, tags=paste(labels, collapse = ","))
  body[['delete_existing']] <- if(isTRUE(delete_existing)) 'true' else 'false'
  res=catmaid_fetch(path, body = body, conn = conn, ..., include_headers = F)
  invisible(res)
}
