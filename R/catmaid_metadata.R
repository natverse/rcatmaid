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

#' Get list of annotations (including user information) from CATMAID
#' 
#' @inheritParams catmaid_get_compact_skeleton
#' @return A list containing two data.frames, annotations and users. The users
#'   data.frame describes the users associated with each annotations, with one row
#'   for each valid annotations/user pair.
#' @export
#' @examples
#' \dontrun{
#' al=catmaid_get_annotationlist(pid=1)
#' # table of the number of users who have contributed to each annotation
#' table(al$neurons$num_users_annotation)
#' }
catmaid_get_annotationlist<-function(pid, conn=NULL, raw=FALSE, ...){
  res=catmaid_fetch("/1/annotations/list", conn=conn, parse.json = TRUE, ...)
  if(raw) return(res)
  # reformat annotation information
  ni=sapply(res[[1]],function(y) unlist(y[c('id','name')]))
  nidf=data.frame(id=as.integer(ni['id',]), name=ni['name',], stringsAsFactors = F)
  
  ui=sapply(res[[1]],function(y) y[!names(y)%in%c('id','name')])
  
  # calculate number of users per annotation (and store it)
  num_users_annotation=sapply(ui, function(x) length(unlist(x, use.names = F))/2, USE.NAMES = F)
  nidf$num_users_annotation=num_users_annotation
  
  uim=matrix(unlist(ui, use.names = F), ncol=2, byrow = T)
  uidf=data.frame(annotation.id=rep(nidf$id, num_users_annotation), id=as.integer(uim[,1]), name=uim[,2])
  
  res$annotations=nidf
  res$users=uidf
  res
}
