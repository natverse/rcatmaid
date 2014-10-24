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
#' @seealso \code{\link{catmaid_fetch}}
catmaid_get_neuronnames<-function(pid, skids, ...) {
  post_data=list(pid=pid)
  post_data[sprintf("skids[%d]", seq_along(skids))]=as.list(skids)
  path=sprintf("/%d/skeleton/neuronnames", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, ...)
  res=unlist(res)
  # handle any missing return values
  missing_names=setdiff(as.character(skids), names(res))
  if(length(missing_names))
    res[missing_names]=NA_character_
  res
}

#' Get list of annotated neurons (including user information) from CATMAID
#' 
#' @inheritParams catmaid_get_compact_skeleton
#' @return A list containing two data.frames, neurons and users. The users
#'   data.frame describes the users associated with each neuron, with one row
#'   for each valid neuron/user pair.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_neuronnames(pid=1)
#' }
catmaid_get_annotationlist<-function(pid, conn=NULL, raw=FALSE, ...){
  res=catmaid_fetch("/1/annotations/list", conn=conn, parse.json = TRUE, ...)
  if(raw) return(res)
  ni=sapply(res[[1]],function(y) unlist(y[c('id','name')]))
  nidf=data.frame(id=as.integer(ni['id',]), name=ni['name',], stringsAsFactors = F)
  
  ui=sapply(res[[1]],function(y) y[!names(y)%in%c('id','name')])
  
  num_users_neuron=sapply(ui, function(x) length(unlist(x, use.names = F)), USE.NAMES = F)
  uim=matrix(unlist(ui, use.names = F), ncol=2, byrow = T)
  uidf=data.frame(neuron.id=rep(nidf$id, num_users_neuron), id=as.integer(uim[,1]), name=uim[,2])
  
  res$neurons=nidf
  res$users=uidf
  res[['annotations']]=NULL
  res
}
