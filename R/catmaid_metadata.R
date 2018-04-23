#' Get names of neurons from CATMAID
#'
#' @details Note that \code{catmaid_get_neuronnames} only queries the server for
#'   the unique set of input \code{skids} to save time.
#'
#' @inheritParams read.neuron.catmaid
#' @return a character vector of neuron names, with a names attribute specifying
#'   the skeleton ids (skids). Missing values will be represented by a
#'   \code{NA_character} value.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_neuronnames(skids=c(10418394,4453485))
#' catmaid_get_neuronnames("name:ORN (left|right)")
#' catmaid_get_neuronnames("annotation:ORN PNs$")
#' }
#' @seealso \code{\link{catmaid_fetch}}, \code{\link{catmaid_skids}}
catmaid_get_neuronnames<-function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  if(any(duplicated(skids))) {
    uskids=unique(skids)
    unames=catmaid_get_neuronnames(uskids, pid=pid, conn=conn, ...)
    res=unames[match(skids, uskids)]
    return(res)
  }
  # -1 indicates a bad skid but does not trigger an error like an NA value
  skids[is.na(skids)]=-1L
  post_data=list(pid=pid)
  post_data[sprintf("skids[%d]", seq_along(skids))]=as.list(skids)
  path=sprintf("/%d/skeleton/neuronnames", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, conn=conn, ...)
  res=unlist(res)
  # handle any missing return values
  missing_names=setdiff(as.character(skids), names(res))
  if(length(missing_names))
    res[missing_names]=NA_character_
  # ensure that return values are in order that was passed in
  res=res[as.character(skids)]
  names(res)[names(res)=="-1"]="NA"
  res
}


#' Rename (presently) a single neuron or other entity (e.g. annotation)
#' 
#' @details Each neuron consists of both a generic entity and an associated 
#'   skeleton (\code{skid}). If you supply the \code{skid} the function will 
#'   internally look up the corresponding neuron entity id. Note that you 
#'   actually wished to change the name of the skeleton you could pass the skid 
#'   into the \code{entityids} argument.
#' @inheritParams read.neuron.catmaid
#' @param entityids Generic entity ids
#' @param names New names
#'   
#' @return \code{logical} value indicating success or failure with attributes 
#'   indicating the previous name.
#' @export
#' 
#' @examples
#' \dontrun{
#' # Warning this will change data!
#' n=get_neuron_name(27296)
#' catmaid_rename_neuron(skids=27296, names=n)
#' }
catmaid_rename_neuron <- function(skids=NULL, entityids=NULL, names, pid=1, conn=NULL, ...) {
  if(!is.null(skids)){
    if(!is.null(entityids))
      stop("You can only supply one of neuronids or entityids!")
    skids=catmaid_skids(skids, conn = conn, pid=pid)
    if(length(skids)>1) stop("I can only work with one neuron at the moment!")
    # now find entityids
    entityids=c(get_neuronid_for_skid(skids))
  }
  if(length(entityids)>1) stop("I can only work with one name at the moment!")
  post_data=list(pid=pid)
  if(length(names)!=length(entityids)) 
    stop("Must supply same number of new names as ids")
  post_data['name']=names
  path=sprintf("/%d/neurons/%d/rename", pid, entityids)
  res=catmaid_fetch(path, body=post_data, include_headers = F, conn=conn, ...)
  if(!is.list(res) || length(res)<3) invisible(FALSE)
  restf=res$success
  attributes(restf)=res[names(res)!="success"]
  invisible(restf)
}

get_neuronid_for_skid <- function(skid, pid=1, conn=NULL, ...){
  path=sprintf("/%d/skeleton/%d/neuronname", pid, skid)
  res=catmaid_fetch(path, include_headers = F, conn=conn, ...)
  if(!is.null(res$error)) {
    rval=NA_integer_
    attr(rval,"error")=res$error
  } else {
    rval=res$neuronid
    attr(rval,"neuronname")=res$neuronname
  }
  rval
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
  res=catmaid_fetch(paste0("/",pid,"/annotations/"), conn=conn, 
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
  query_by_neuron_or_annotation(path=paste0(pid, '/annotations/query-targets'),
                                body=list(name=query),
                                maxresults=maxresults, type=type, 
                                raw=raw, ...=...)
}

query_by_neuron_or_annotation<-function(path, body, maxresults=500, 
                                        type=c("neuron","annotation"), 
                                        raw=FALSE, ...){
  return_type=match.arg(type, c("neuron","annotation"), several.ok = T)
  body$rangey_start=0
  body$range_length=sprintf("%d",maxresults)
  res=catmaid_fetch(path=path, body=body, ..., include_headers = F)
  if(raw) return(res)
  # key fields name type, id
  res2=list2df(res$entities, c("id", "name", "type", "skeleton_ids"), use.col.names = T)
  if(is.null(res2)) return(res2)
  names(res2)[names(res2)=="skeleton_ids"]="skid"
  # this column makes no sense as a factor since names should be unique
  res2$name=as.character(res2$name)
  if(length(return_type)==1) {
    # subset
    res2=res2[res2$type%in%return_type,]
  }
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
                                      conn=NULL, ...){
  return_type=match.arg(type, several.ok = T)
  if(is.character(query)) {
    querydf=catmaid_aids(query, conn=conn, rval = 'data.frame')
    query=querydf$id
    if(length(query)>1) {
      warning(length(query)," matching annotations!")
      res=lapply(query, catmaid_query_by_annotation, conn=conn, 
                 type=return_type, raw=raw, pid=pid, maxresults=maxresults, ...)
      names(res)=query
      attr(res, 'annotations')=querydf
      return(res)
    }
  }
  if(!length(query)) return(NULL)
  query_by_neuron_or_annotation(paste0(pid, '/annotations/query-targets'),
                                body=list(annotated_with=query),
                                maxresults=maxresults, type=return_type, 
                                raw=raw, conn=conn, ...=...)
}

catmaid_aids<-function(x, several.ok=TRUE, conn=NULL, pid=1, fixed=FALSE, 
  rval=c('ids', 'data.frame'), ...) {
  if(is.numeric(x)) return(x)
  al=catmaid_get_annotationlist(conn=conn, pid = pid)
  
  process_match <- function(x, al, several.ok, fixed) {
    matches=grepl(x, al$annotations$name, fixed = fixed)
    nmatches=sum(matches)
    if(nmatches==0) return(NULL)
    ids=al$annotations$id[matches]
    if(!several.ok && nmatches>1) 
      stop("There are ", nmatches, " matches but only one is allowed!")
    ids
  }
  
  ids=unlist(lapply(x, process_match, al=al, several.ok=several.ok, fixed=fixed))
  rval=match.arg(rval)
  if(rval=='ids') return(ids)
  al$annotations[match(ids, al$annotations$id),]
}

#' Find neurons connected to a starting neuron
#' 
#' @param minimum_synapses Must be at least this number of synapses between 
#'   starter neuron and returned partners
#' @param boolean_op Whether returned neurons can be connected to \emph{any} 
#'   character vector (\code{boolean_op="OR"}, the default) or must be connected
#'   to all the specified neurons (\code{boolean_op="AND"}).
#' @inheritParams read.neuron.catmaid
#' @inheritParams catmaid_get_compact_skeleton
#' @return A list containing two data.frames (incoming, outgoing), each 
#'   data.frame having one row for each partner neuron and the following columns
#'   \itemize{
#'   
#'   \item skid skeleton id of the query neuron
#'   
#'   \item partner the skeleton id of the partner neuron
#'   
#'   \item syn.count the number of synapses made with partner
#'   
#'   \item num_nodes number of nodes in the partner neuron
#'   
#'   }
#' @examples
#' \dontrun{
#' orn13a=catmaid_query_by_name("13a ORN left")$skid
#' catmaid_query_connected(orn13a)
#' 
#' # connected to either left OR right 13a ORNs
#' orn13as=catmaid_query_by_name("13a ORN")
#' catmaid_query_connected(orn13as)
#' # connected to both left AND right 13a ORNs
#' catmaid_query_connected(orn13as, boolean_op = 'AND')
#' }
#' @export
#' @seealso \code{\link{catmaid_skids}}. See 
#'   \code{\link{catmaid_get_review_status}} to get information about the review
#'   status of partners (as shown in the equivalent CATMAID report).
#' @family connectors
catmaid_query_connected<-function(skids, minimum_synapses=1, 
                                  boolean_op=c("OR","AND"), 
                                  pid=1, raw=FALSE, conn=NULL, ...){
  boolean_op=match.arg(boolean_op)
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  names(skids)=sprintf('source_skeleton_ids[%d]',seq_along(skids))
  connectivity_post = c(as.list(skids), threshold=minimum_synapses, 
                        boolean_op=boolean_op)
  path=paste0("/",pid,"/skeletons/connectivity")
  rawres=catmaid_fetch(path, connectivity_post, include_headers = F, 
                       simplifyVector = TRUE, conn=conn)
  if(raw) return(rawres)
  
  # pretty up the output data.frames
  makeresdf<-function(x, minimum_synapses) {
    if(is.null(x)) return(NULL)
    # nb I do not know what the first 4 elements alongside the 5th element
    skids=sapply(x,function(x) names(x$skids), simplify = FALSE)
    npartners=sapply(skids, length)
    df=data.frame(skid=as.integer(unlist(skids, use.names = FALSE)),
                  partner=rep(as.integer(names(x)), npartners),
                  syn.count=unlist(lapply(x, function(x) lapply(x$skids,"[",5)), use.names = FALSE),
                  num_nodes=rep(sapply(x,"[[", "num_nodes", USE.NAMES = F), npartners),
                  stringsAsFactors = FALSE)
    rownames(df)=NULL
    colsforleft<-c('skid','name','syn.count')
    df=df[df$syn.count>=minimum_synapses, ]
    df[order(df$syn.count, decreasing = TRUE),]
  }
  
  lapply(rawres[c("outgoing","incoming")], makeresdf, minimum_synapses)
}

#' Get review status of neurons from CATMAID
#' 
#' @inheritParams read.neuron.catmaid
#' @return a data.frame consisting of total number of nodes and number of
#'   reviewed nodes. Note that should any neurons be invalid a warning will be
#'   generated and a row with NA values will be returned.
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_review_status(skids=c(10418394,4453485))
#' }
#' @seealso \code{\link{catmaid_fetch}}
catmaid_get_review_status<-function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  post_data=list()
  post_data[sprintf("skeleton_ids[%d]", seq_along(skids)-1)]=as.list(skids)
  path=sprintf("/%d/skeletons/review-status", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, conn=conn, ...)
  res=list2df(res, cols=c('total','reviewed'))
  # handle any missing return values
  missing_names=setdiff(as.character(skids), rownames(res))
  if(length(missing_names)){
    warning("unable to identify ", length(missing_names), " neuron(s).")
    res[missing_names,]=NA
  }
  # ensure that return values are in order that was passed in
  res[as.character(skids),]
}
