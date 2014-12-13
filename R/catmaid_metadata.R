#' Get names of neurons from CATMAID
#' 
#' @inheritParams read.neurons.catmaid
#' @return a named character vector of neuron names. Missing values will be
#'   represented by a \code{NA_character} value.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_neuronnames(skids=c(10418394,4453485))
#' }
#' @seealso \code{\link{catmaid_fetch}}
catmaid_get_neuronnames<-function(skids, pid=1, ...) {
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
#' table(al$annotations$num_users_annotation)
#' }
catmaid_get_annotationlist<-function(pid=1, conn=NULL, raw=FALSE, ...){
  res=catmaid_fetch(paste0("/",pid,"/annotations/list"), conn=conn, 
                    parse.json = TRUE, ...)
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

#' Get list of neurons/annotations querying by neuron/annotation name
#' 
#' @param query A query string (NB this can be a regular expression)
#' @inheritParams catmaid_get_compact_skeleton
#' @param maxresults The maximum number of results to return
#' @return a data.frame containing the results with an attribute "annotations" 
#'   containing the annotations as a raw list
#' @export
#' @examples
#' \dontrun{
#' catmaid_query_by_neuronname("ORN")
#' # using regex functionality
#' catmaid_query_by_neuronname("ORN (left|right)")
#' # fancier regex
#' catmaid_query_by_neuronname("^[0-9a-f &]+ PN (left|right)")
#' }
catmaid_query_by_neuronname<-function(query, pid=1, maxresults=500, raw=FALSE, ...){
  res=catmaid_fetch('1/neuron/query-by-annotations', ..., include_headers = F,
                body=list(neuron_query_by_name=query, display_start=0, display_length=sprintf("%d",maxresults)))
  if(raw) return(res)
  # key fields name type, id
  res2=list2df(res$entities, c("id", "name", "type", "skeleton_ids"), use.col.names = T)
  attr(res2,'annotations')=lapply(res$entities, "[[", "annotations")
  res2
}

#' Find neurons connected to a starting neuron
#' 
#' @param minimum_synapses Must be at least this number of synapses between 
#'   starter neuron and returned partners
#' @inheritParams catmaid_get_compact_skeleton
#' @return A list containing two data.frames (incoming, outgoing) when
#'   \code{raw=FALSE}
#' @examples
#' \dontrun{
#' orn13a=catmaid_query_by_neuronname("13a ORN left")$skeleton_ids
#' catmaid_query_connected(orn13a)
#' }
#' @export
catmaid_query_connected<-function(skid, minimum_synapses=1, pid=1, raw=FALSE, ...){
  path=paste0("/",pid,"/skeleton/connectivity")
  connectivity_post = list('source[0]'=skid, threshold=minimum_synapses, boolean_op='logic_OR')
  res=catmaid_fetch(path, connectivity_post, include_headers = F, ...)
  if(raw) return(res)
    
  res$outgoing=list2df(res$outgoing, 
                       cols=c("union_reviewed", "skids", "name", "num_nodes"),
                       use.col.names = T, stringsAsFactors=FALSE)
  res$incoming=list2df(res$incoming, 
                       cols=c("union_reviewed", "skids", "name", "num_nodes"),
                       use.col.names = T, stringsAsFactors=FALSE)
  
  # pretty up the output data.frames
  fixresdf<-function(df, minimum_synapses) {
    if(is.null(df)) return(NULL)
    # not sure why this element ends up named like this in the json
    names(df)[names(df)=='skids']='syn.count'
    # actual skeleton ids come back as rownames
    df$skid=as.integer(rownames(df))
    rownames(df)=NULL
    colsforleft<-c('skid','name','syn.count')
    df=df[c(colsforleft,setdiff(names(df), colsforleft))]
    df=df[df$syn.count>=minimum_synapses, ]
    df[order(df$syn.count, decreasing = TRUE),]
  }

  lapply(res, fixresdf, minimum_synapses)
}
