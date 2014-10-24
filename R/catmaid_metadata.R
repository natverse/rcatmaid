#' Get names of neurons from CATMAID
#' 
#' @inheritParams read.neurons.catmaid
#' @return a named character vector of neuron names. Missing values will be
#'   represented by a \code{NA_character} value.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_neuronnames(pid=1, skids=c(10418394,4453485))
#' }
#' @seealso \code{\link{catmaid_POSTJ}}
catmaid_get_neuronnames<-function(pid, skids, ...) {
  post_data=list(pid=pid)
  post_data[sprintf("skids[%d]", seq_along(skids))]=as.list(skids)
  path=sprintf("/%d/skeleton/neuronnames", pid)
  res=catmaid_POSTJ(path, post_data, include_headers = F, ...)
  res=unlist(res)
  # handle any missing return values
  missing_names=setdiff(as.character(skids), names(res))
  if(length(missing_names))
    res[missing_names]=NA_character_
  res
}
