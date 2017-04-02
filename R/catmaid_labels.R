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
catmaid_get_label_stats<-function(pid=1, conn=NULL, ...) {
  # see https://github.com/catmaid/CATMAID/blob/eec50d6c3532ad92ce1f511f39e5aed66c6297e5/django/applications/catmaid/control/label.py#L99
  res=catmaid_fetch(paste0(pid, '/labels/stats'), include_headers = F, conn=conn, ...)
  list2df(res, c("labelID", "labelName", "skeletonID", "treenodeID"))
}
