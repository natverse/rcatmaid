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
  # ensure that return values are in order that was passed in
  res[as.character(skids)]
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
#' @description These functions try to match a \code{query} against the 
#'   \code{name} of a neuron or annotation (\code{catmaid_query_by_name}) 
#'   \emph{or} against an annotation (tag) that has been applied to a neuron or 
#'   annotation (\code{catmaid_query_by_annotation}). Note that 1) objects can
#'   be tagged with multiple annotations and 2) annotation tags are recursive so
#'   one annotation can be tagged with a second annotation.
#'   
#'   The most common use will be to get a data.frames of neurons when you should
#'   use \code{type="neuron"}. You can also return both neurons and annotations 
#'   (the default) and just annotations. See \code{type} argument for details.
#'   
#' @param query A query string (NB this is a regular expression)
#' @inheritParams catmaid_get_compact_skeleton
#' @param maxresults The maximum number of results to return
#' @param type Type of results to return. Defaults to \code{c("neuron", 
#'   "annotation")}. Only relevant when \code{raw=FALSE}.
#' @return For \code{catmaid_query_by_name} a data.frame containing the 
#'   results with an attribute "annotations" containing the annotations as a 
#'   separate data.frame. For both functions the main data.frame has the 
#'   following columns \itemize{
#'   
#'   \item id
#'   
#'   \item name
#'   
#'   \item type (neuron or annotation)
#'   
#'   \item skid (the main identfier for the neuron skeleton, catmaid often calls
#'   this skeleton_id)
#'   
#'   }
#' @export
#' @examples
#' \dontrun{
#' catmaid_query_by_name("ORN")
#' # using regex functionality
#' catmaid_query_by_name("ORN (left|right)")
#' # fancier regex
#' catmaid_query_by_name("^[0-9a-f &]+ PN (left|right)")
#' }
catmaid_query_by_name<-function(query, pid=1, maxresults=500, 
                                      type=c("neuron","annotation"), raw=FALSE, 
                                      ...){
  query_by_neuron_or_annotation(path='1/neuron/query-by-annotations',
                                body=list(neuron_query_by_name=query,
                                          display_start=0,
                                          display_length=sprintf("%d",maxresults)),
                                pid=pid, maxresults=maxresults, type=type, 
                                raw=raw, ...=...)
}

query_by_neuron_or_annotation<-function(path, body, pid=1, maxresults=500, 
                                        type=c("neuron","annotation"), 
                                        raw=FALSE, ...){
  return_type=match.arg(type, c("neuron","annotation"), several.ok = T)
  res=catmaid_fetch(path=path, body=body, ..., include_headers = F)
  if(raw) return(res)
  # key fields name type, id
  res2=list2df(res$entities, c("id", "name", "type", "skeleton_ids"), use.col.names = T)
  if(is.null(res2)) return(res2)
  names(res2)[names(res2)=="skeleton_ids"]="skid"
  al=lapply(res$entities, "[[", "annotations")
  ldf=lapply(al, list2df, c("uid","id", "name"), use.col.names=T, return_empty_df=T)
  adf=rbind.pages(ldf)
  names(adf)[names(adf)=='id']='annotation_id'
  adf$id=rep(res2$id, sapply(ldf, nrow))
  if(length(return_type)==1) {
    # subset
    res2=res2[res2$type%in%return_type,]
    adf=adf[adf$id%in%res2$id,]
  }
  attr(res2,'annotations')=adf
  res2
}


#' @rdname catmaid_query_by_name
#' @return For \code{catmaid_query_by_annotation} a data.frame containing the 
#'   results.
#' @seealso \code{\link{catmaid_get_annotationlist}}
#' @export
#' @examples 
#' \dontrun{
#' # query matches 3 specific annotations
#' catmaid_query_by_annotation("ORN PNs")
#' 
#' # what are those 3 annotations?
#' al=catmaid_get_annotationlist()
#' subset(al$annotations, grepl("ORN PNs", name))
#' 
#' # Insist on specific annotation by using regex start/finish symbols
#' catmaid_query_by_annotation("^ORN PNs$")
#' }
catmaid_query_by_annotation<-function(query, pid=1, maxresults=500, 
                                      type=c("neuron","annotation"), raw=FALSE, 
                                      ...){
  return_type=match.arg(type, several.ok = T)
  if(is.character(query)) {
    al=catmaid_get_annotationlist()
    matches=grepl(query, al$annotations$name)
    nmatches=sum(matches)
    if(nmatches==0) return(NULL)
    query=al$annotations$id[matches]
    if(nmatches>1) {
      warning(nmatches," matching annotations!")
      return(lapply(query, catmaid_query_by_annotation))
    }
  }
  query_by_neuron_or_annotation('1/neuron/query-by-annotations',
                                body=list(neuron_query_by_annotation=query, 
                                          display_start=0, 
                                          display_length=sprintf("%d",maxresults)),
                                pid=pid, maxresults=maxresults, type=type, 
                                raw=raw, ...=...)
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
#' orn13a=catmaid_query_by_name("13a ORN left")$skid
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

