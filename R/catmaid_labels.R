#' Get statistics for all labels in a project from a CATMAID server
#'
#' @inheritParams read.neuron.catmaid
#'
#' @description \code{catmaid_get_label_stats} returns a data.frame with one row
#'   for every node-tag pair.
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

#' @description \code{catmaid_get_all_labels} returns a character vector of labels
#' 
#' @export
#' @rdname catmaid_get_label_stats
#' @examples
#' \donttest{
#' head(catmaid_get_all_labels())
#' }
catmaid_get_all_labels<-function(pid=1, conn=NULL, ...) {
  # see https://github.com/catmaid/CATMAID/blob/eec50d6c3532ad92ce1f511f39e5aed66c6297e5/django/applications/catmaid/control/label.py#L99
  res=catmaid_fetch(paste0(pid, '/labels/'), include_headers = F, conn=conn, ..., simplifyVector = T)
  c(catmaid_error_check(res))
  
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
#' @return \code{catmaid_get_label_stats} returns a data.frame with columns
#'   \itemize{
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

#' @description \code{catmaid_set_labels} sets labels (tags) for a specified
#'   node.
#' @param node A single tree or connector node to modify
#' @param labels A character vector specifying the labels to add or remove
#' @param type A character vector specifying the type of node to modify
#' @param delete_existing For \code{catmaid_set_labels} whether to remove all
#'   existing labels before setting the new ones.
#' @export
#' @rdname catmaid_get_labels
catmaid_set_labels <- function(node, labels, type=c("treenode", "connector"),
                               delete_existing=FALSE, pid=1, conn=NULL, ...) {
  type=match.arg(type)
  path=file.path(pid, "label", type, node, "update", fsep="/")
  if(any(grepl(",", labels, fixed = T)))
    stop("CATMAID cannot accept labels containing commas")
  body=list(ntype=type, tags=paste(labels, collapse = ","))
  body[['delete_existing']] <- if(isTRUE(delete_existing)) 'true' else 'false'
  res=catmaid_fetch(path, body = body, conn = conn, ..., include_headers = F)
  invisible(catmaid_error_check(res))
}

#' @description \code{catmaid_remove_labels} removes labels (tags) for a
#'   specified node.
#' @export
#' @rdname catmaid_get_labels
#' @return \code{catmaid_remove_labels} and \code{catmaid_set_labels} throw an
#'   error on failure or invisibly return a list containing status information.
catmaid_remove_labels <- function(node, labels, type=c("treenode", "connector"),
                               pid=1, conn=NULL, ...) {
  type=match.arg(type)
  path=file.path(pid, "label", type, node, "remove", fsep="/")
  if(any(grepl(",", labels, fixed = T)))
    stop("CATMAID cannot accept labels containing commas")
  if(length(labels)>1)
    stop("CATMAID API can only remove one label at a time!")
  body=list(ntype=type, tag=paste(labels, collapse = ","))
  res=catmaid_fetch(path, body = body, conn = conn, ..., include_headers = F)
  invisible(catmaid_error_check(res))
}
