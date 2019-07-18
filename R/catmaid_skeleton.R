#' Return the raw data for a CATMAID neuronal skeleton
#' 
#' @details Note that by default this fetches both skeleton and connector 
#'   (synapse) information.
#' @param skid single skeleton id
#' @param pid project id (default 1)
#' @param conn the \code{\link{catmaid_connection}} object
#' @param connectors Whether to fetch connector information
#' @param tags Whether to fetch tag information
#' @param raw Whether to return completely unprocessed data (when \code{TRUE}) 
#'   or to convert the nodes and connectors lists into processed data.frames 
#'   (when \code{FALSE}, the default)
#' @param ... Additional arguments passed to the \code{\link{catmaid_fetch}} 
#'   function.
#' @return An R list object with three elements \itemize{
#'   
#'   \item nodes A data frame containing XYZ location, node identifiers etc for 
#'   each point in the neuron.
#'   
#'   \item connectors A data frame containing the position and tree node 
#'   identifiers for the synaptic partners.
#'   
#'   \item tags A list containing one vector for each named tag; the vectors 
#'   contain node ids that are also present in the \code{nodes} element.
#'   
#'   }
#' @seealso \code{\link{read.neuron.catmaid}} to read as neuroanatomy toolbox 
#'   neuron that can be plotted directly. \code{\link{catmaid_fetch}}.
#' @export
#' @examples
#' \dontrun{
#' ## ensure that you have done something like
#' # conn=catmaid_login()
#' # at least once this session to connect to the server
#' skel=catmaid_get_compact_skeleton(10418394)
#' # no connector (i.e. synapse) information
#' skel=catmaid_get_compact_skeleton(10418394, connectors = FALSE)
#' 
#' }
catmaid_get_compact_skeleton<-function(skid, pid=1L, conn=NULL, connectors = TRUE, tags = TRUE, raw=FALSE, ...) {
  path=file.path("", pid, skid, ifelse(connectors, 1L, 0L), ifelse(tags, 1L, 0L), "compact-skeleton")
  skel=catmaid_fetch(path, conn=conn, ...)
  if(is.character(skel[[1]]) && isTRUE(skel[[1]]=="Exception"))
    stop("No valid neuron returned for skid: ",skid)
  names(skel)=c("nodes", "connectors", "tags")
  
  if(raw) return(skel)
  # else process the skeleton
  if(length(skel$nodes))
    skel$nodes=list2df(skel$nodes, 
                     cols=c("id", "parent_id", "user_id", "x","y", "z", "radius", "confidence"))
  
  if(length(skel$connectors))
    skel$connectors=list2df(skel$connectors, 
                            cols=c("treenode_id", "connector_id", "prepost", "x", "y", "z"))
  # change tags from list of lists to list of vectors
  skel$tags=sapply(skel$tags, function(x) sort(unlist(x)), simplify = FALSE)
  skel
}

list2df<-function(x, cols, use.col.names=F, return_empty_df=FALSE, ...) {
  if(!length(x)) {
    return(if(return_empty_df){
      as.data.frame(structure(replicate(length(cols), logical(0)), .Names=cols))
    } else NULL)
  }
  l=list()
  for(i in seq_along(cols)) {
    colidx=if(use.col.names) cols[i] else i
    raw_col = sapply(x, "[[", colidx)
    if(is.list(raw_col)) {
      raw_col[sapply(raw_col, is.null)]=NA
      sublens=sapply(raw_col, length)
      if(all(sublens==1))
        raw_col=unlist(raw_col)
      else raw_col=sapply(raw_col, paste, collapse=',')
    }
    l[[cols[i]]]=raw_col
  }
  as.data.frame(l, ...)
}

#' Return skeleton ids for pre/postsynaptic partners of a set of connector_ids
#' 
#' @details Note that this returns pairwise connections between neurons. A 
#'   single synapse (i.e. connector) may have multiple connections; most 
#'   commonly a single presynaptic cell connects to multiple post-synaptic
#'   cells but many variations are possible
#' @param connector_ids Numeric ids for each connector (synapse).
#' @inheritParams catmaid_get_compact_skeleton
#' @return A data.frame with columns \itemize{
#'   
#'   \item connector_id
#'   
#'   \item pre
#'   
#'   \item post
#'   
#'   }
#' @export
#' @family connectors
catmaid_get_connectors<-function(connector_ids, pid=1, conn=NULL, raw=FALSE, ...) {
  path=paste("", pid, "connector","skeletons",sep="/")
  post_data=as.list(connector_ids)
  names(post_data)=sprintf("connector_ids[%d]", seq_along(connector_ids))
  conns=catmaid_fetch(path, body=post_data, conn=conn, ...)
  
  if(raw) return(conns)
  # else process the connector information
  if(!length(conns)) return(NULL)

  # connector_ids
  ids=as.integer(sapply(conns, "[[", 1))
  # make indiviudal data.frames of synapse info in long form
  syns=lapply(conns, function(y) expand.grid(pre=unlist(y[[2]]['presynaptic_to'], use.names = F),
                                             post=unlist(y[[2]]['postsynaptic_to'], use.names = F)))
  # now assemble that all together
  df=data.frame(connector_id=rep(ids, sapply(syns, nrow)))
  cbind(df, do.call(rbind, syns))
}


#' Return connector table for a given neuron
#'
#' @param skids Numeric skeleton ids
#' @param direction whether to find incoming or outgoing connections
#' @param partner.skids Whether to include information about the skid of each
#'   partner neuron (NB there may be multiple partners per connector)
#' @param get_partner_names,get_partner_nodes Whether to fetch the names and/or
#'   number of nodes for the partner neurons.
#' @inheritParams read.neuron.catmaid
#' @inheritParams catmaid_get_compact_skeleton
#' @return As of CATMAID v2016.10.18 this returns a data.frame with columns
#'   \itemize{
#'
#'   \item skid
#'
#'   \item connector_id
#'
#'   \item x
#'
#'   \item y
#'
#'   \item z
#'
#'   \item confidence
#'
#'   \item user_id
#'
#'   \item treenode_id (NB this is always the treenode id of the query skeleton
#'   whether or not incoming or outgoing connections are requested)
#'
#'   \item last_modified
#'
#'   \item partner_skid
#'
#'   }
#'
#'   Prior to this it returned a data.frame with columns \itemize{
#'
#'   \item connector_id
#'
#'   \item partner_skid
#'
#'   \item x
#'
#'   \item y
#'
#'   \item z
#'
#'   \item s
#'
#'   \item confidence
#'
#'   \item tags
#'
#'   \item nodes_in_partner
#'
#'   \item username
#'
#'   \item treenode_id
#'
#'   \item last_modified
#'
#'   }
#' @export
#' @examples
#' \dontrun{
#' # fetch connector table for neuron 10418394
#' ct=catmaid_get_connector_table(10418394)
#' # compare number of incoming and outgoing synapses
#' table(ct$direction)
#'
#' ## Look at synapse location in 3d
#' # plot the neuron skeleton in grey for context
#' library(nat)
#' nopen3d()
#' plot3d(read.neurons.catmaid(10418394), col='grey')
#' # note use of nat::xyzmatrix to get xyz positions from the ct data.frame
#' # colour synapses by direction
#' points3d(xyzmatrix(ct), col=as.integer(ct$direction))
#'
#' ## plot connected neurons in context of brain
#' nopen3d()
#' # fetch and plot brain model
#' models=catmaid_fetch("1/stack/5/models")
#' vs=matrix(as.numeric(models$cns$vertices), ncol=3, byrow = TRUE)
#' points3d(vs, col='grey', size=1.5)
#'
#' # fetch and plot neurons
#' plot3d(read.neurons.catmaid(10418394), col='black', lwd=3)
#' points3d(xyzmatrix(ct), col=as.integer(ct$direction))
#'
#' partner_neuron_ids=unique(na.omit(as.integer(ct$partner_skid)))
#' partner_neurons=read.neurons.catmaid(partner_neuron_ids, .progress='text', OmitFailures = TRUE)
#' plot3d(partner_neurons)
#' }
#' @family connectors
catmaid_get_connector_table<-function(skids, 
                                      direction=c("both", "incoming", "outgoing"),
                                      partner.skids=TRUE,
                                      get_partner_names=FALSE, get_partner_nodes=FALSE,
                                      pid=1, conn=NULL, raw=FALSE, ...) {
  direction=match.arg(direction)
  if(get_partner_names || get_partner_nodes) {
    if(!partner.skids) 
      stop("Must fetch partner skids to find names/partner nodes")
  }
  
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  if(direction[1]=='both') {
    dfin =catmaid_get_connector_table(skids, direction='incoming', pid=pid, conn=conn, raw=raw, ...)
    dfout=catmaid_get_connector_table(skids, direction='outgoing', pid=pid, conn=conn, raw=raw, ...)
    dfin$direction="incoming"
    dfout$direction="outgoing"
    df=rbind(dfin,dfout)
    df$direction=factor(df$direction)
    return(df)
  }
  if(catmaid_version(numeric = TRUE)>="2016.09.01-77"){
    body=NULL
    paramsv=sprintf("skeleton_ids[%s]=%d",seq_len(length(skids)), skids)
    paramsv=c(paramsv, paste0("relation_type=", ifelse(direction=="incoming","postsynaptic_to","presynaptic_to")))
    params=paste(paramsv, collapse = "&")
    if(catmaid_version(numeric = TRUE)>="2017.10.02-128"){
      # see https://github.com/catmaid/CATMAID/commit/9d029cbfaa92a9f14bcf99ebffcb89c3da786ef0
      relpath=paste0("/", pid, "/connectors/links/?",params)
    } else {
      relpath=paste0("/", pid, "/connectors/?",params)
    }
  } else {
    relpath=paste0("/", pid, "/connector/table/list")
    body=list(skeleton_id=skids)
    # relation_type 0 => incoming
    if(catmaid_version(numeric = TRUE)>="2016.09.01-65"){
      body$relation_type=ifelse(direction=="incoming","postsynaptic_to","presynaptic_to")
    } else {
      body$relation_type=ifelse(direction=="incoming",0L, 1L)
    }
  }
  ctl=catmaid_fetch(path=relpath, body=body, conn=conn, ...)
  catmaid_error_check(ctl)
  if(raw) return(ctl)
  # else process the connector information
  dfcolnames <- if(catmaid_version(numeric = TRUE)>="2016.09.01-77") {
    c("skid", "connector_id", "x", "y", "z", "confidence", 
      "user_id", "treenode_id", "last_modified")
  } else {
    c("connector_id", "partner_skid", "x", "y", "z", "s", "confidence", 
      "tags", "nodes_in_partner", "username", "treenode_id", 
      "last_modified")
  }
  df=list2df(ctl[[1]], cols = dfcolnames, return_empty_df = T, stringsAsFactors=FALSE)
  if("username"%in%names(df))
    df$username=factor(df$username)
  if(is.character(df$partner_skid))
    df$partner_skid=as.integer(df$partner_skid)
  if(partner.skids && !"partner_skid"%in%names(df)){
    # find the skids for the partners
    cdf=catmaid_get_connectors(df$connector_id, pid = pid, conn=conn, ...)
  
    if(direction=="outgoing") {
      names(cdf)[2:3]=c("skid","partner_skid")
    } else {
      names(cdf)[2:3]=c("partner_skid","skid")
    }
    # remove any duplicates e.g. where the a single connector 
    # has multiple connections for the same pair of partner neurons
    cdf=cdf[!duplicated(cdf), , drop=FALSE]
    
    df=merge(df, cdf, by=c('connector_id', 'skid'), all.x=TRUE)
  }
  if(get_partner_names)
    df$partner_name <- catmaid_get_neuronnames(df$partner_skid)
  
  if(get_partner_nodes)
    df$partner_nodes <- catmaid_get_node_count(df$partner_skid)
  
  df
}

#' Return tree node table for a given neuron
#' 
#' @param skid Numeric skeleton id
#' @inheritParams catmaid_get_compact_skeleton
#' @return A data.frame with columns \itemize{
#'   
#'   \item id
#'   
#'   \item parent_id
#'   
#'   \item confidence
#'   
#'   \item x
#'   
#'   \item y
#'   
#'   \item z
#'   
#'   \item r
#'   
#'   \item user_id
#'   
#'   \item last_modified
#'   
#'   \item reviewer_id (character vector with comma separated reviewer ids)
#'   
#'   }
#'   
#'   In addition two data.frames will be included as attributes: \code{reviews},
#'   \code{tags}.
#'   
#' @export
#' @examples 
#' \dontrun{
#' # get tree node table for neuron 10418394
#' tnt=catmaid_get_treenode_table(10418394)
#' 
#' # look at tags data
#' str(attr(tnt, 'tags'))
#' # merge with main node table to get xyz position
#' tags=merge(attr(tnt, 'tags'), tnt, by='id')
#' # label up a 3d neuron plot
#' n=read.neuron.catmaid(10418394)
#' plot3d(n, WithNodes=F)
#' text3d(xyzmatrix(tags), texts = tags$tag, cex=.7)
#' }
#' @seealso \code{\link{catmaid_get_compact_skeleton}}, 
#'   \code{\link{read.neuron.catmaid}} and \code{\link{catmaid_get_user_list}} 
#'   to translate user ids into names.
catmaid_get_treenode_table<-function(skid, pid=1, conn=NULL, raw=FALSE, ...) {
  # relation_type 0 => incoming
  tnl=catmaid_fetch(path=paste0("/", pid, "/skeletons/",skid,"/node-overview"),
                    conn=conn, simplifyVector = TRUE, ...)
  
  if(raw) return(tnl)
  # else process the tree node information
  # this comes in 3 separate structures:
  # treenodes, reviews, tags
  if(length(tnl)!=3)
    stop("I don't understand the raw treenode structure returned by catmaid")
  if(!length(tnl[[1]]))
    stop("There are no tree nodes for this skeleton id")
  names(tnl)=c("treenodes", "reviews", "tags")
  tnl=lapply(tnl, as.data.frame, stringsAsFactors=FALSE)
  
  colnames(tnl$treenodes)=c("id", "parent_id", "confidence", "x", "y", "z", "r",
                            "user_id", "last_modified")
  idcols=grepl("id", colnames(tnl$treenodes), fixed = TRUE)
  tnl$treenodes[idcols]=lapply(tnl$treenodes[idcols], as.integer)
  
  if(length(tnl$reviews)) {
    colnames(tnl$reviews)=c("id", "reviewer_id")
    # collapse reviewer ids into single item so that we can add one 
    # well-behaved column to the data.frame
    b=by(tnl$reviews$reviewer_id, tnl$reviews$id, paste, collapse=",")
    merged_reviews=data.frame(id=as.integer(names(b)), 
                           reviewer_id=unname(sapply(b,c)), 
                           stringsAsFactors = F)
  } else {
    merged_reviews=data.frame(id=integer(),reviewer_id=character())
    tnl$reviews=data.frame(id=integer(),reviewer_id=integer())
  }
  
  colnames(tnl$tags)=c("id", "tag")
  tnl$tags=as.data.frame(tnl$tags, stringsAsFactors = FALSE)
  tnl$tags$id=as.integer(tnl$tags$id)
  
  tndf=merge(tnl$treenodes, merged_reviews, by='id', all.x=TRUE)
  attr(tndf, 'tags')=tnl$tags
  attr(tndf, 'reviews')=tnl$reviews
  tndf
}

#' Return information about connectors joining sets of pre/postsynaptic skids
#'
#' @details If either the \code{pre_skids} or \code{post_skids} arguments are
#'   not specified (taking the default \code{NULL} value) then this implies
#'   there is no restriction on the pre- (or post-) synaptic partners.
#'
#'   Each row is a unique set of pre_synaptic node, post_synaptic node,
#'   connector_id. A rare (and usually erroneous) scenario is if the same
#'   pre_node and post_node are present with two different connector_ids - this
#'   would create two rows.
#' @param pre_skids,post_skids Skeleton ids in any form understood by
#'   \code{\link{catmaid_skids}} or \code{NULL} meaning no restriction.
#' @param get_names Whether to fetch the neuron name for each pre- and
#'   post-synaptic skid (default \code{FALSE}).
#' @return A data.frame with columns \itemize{
#'
#'   \item pre_skid
#'
#'   \item post_skid
#'
#'   \item connector_id
#'
#'   \item pre_node_id
#'
#'   \item post_node_id
#'
#'   \item connector_x
#'
#'   \item connector_y
#'
#'   \item connector_z
#'
#'   \item pre_node_x
#'
#'   \item pre_node_y
#'
#'   \item pre_node_z
#'
#'   \item post_node_x
#'
#'   \item post_node_y
#'
#'   \item post_node_z
#'
#'   \item pre_confidence
#'
#'   \item pre_user
#'
#'   \item post_confidence
#'
#'   \item post_user
#'
#'   \item pre_name (optional - the name of the presynaptic neuron)
#'
#'   \item post_name (optional - the name of the postsynaptic neuron)
#'
#'   }
#' @export
#' @inheritParams catmaid_get_compact_skeleton
#' @family connectors
catmaid_get_connectors_between <- function(pre_skids=NULL, post_skids=NULL, 
                                           get_names=FALSE,
                                           pid=1, conn=NULL, raw=FALSE, ...) {
  post_data=list()
  if(is.null(post_skids) && is.null(pre_skids))
    stop("pre_skids and post_skids cannot both be null!")
  
  if(!is.null(pre_skids)){
    pre_skids=catmaid_skids(pre_skids, conn = conn, pid=pid)
    post_data[sprintf("pre[%d]", seq(from=0, along.with=pre_skids))]=as.list(pre_skids)
  } else {
    cvn = catmaid_version(numeric = TRUE)
    if (cvn < "2017.04.20" && cvn >= "2016.08.09")
      stop(
        "catmaid_get_connectors_between is buggy for CATMAID server version",
        cvn,
        "when no pre_skids are specified"
      )
  }
  if(!is.null(post_skids)){
    post_skids=catmaid_skids(post_skids, conn = conn, pid=pid)
    post_data[sprintf("post[%d]", seq(from=0, along.with=post_skids))]=as.list(post_skids)
  }
  path=paste("", pid, "connector", "info", sep="/")
  conns=catmaid_fetch(path, body=post_data, conn=conn, ...)
  
  if(raw) return(conns)
  # else process the connector information
  if(!length(conns)) return(NULL)
  
  df=do.call(rbind, conns)
  colnames(df)=c("connector_id", "connector_xyz", "pre_node_id", "pre_skid", "pre_confidence", "pre_user", "pre_node_xyz", 
                 "post_node_id", "post_skid", "post_confidence", "post_user", "post_node_xyz")
  ddf=as.data.frame(df)
   xyzcols=grep("xyz",colnames(ddf), value = T)
  for(col in rev(xyzcols)){
    xyz=data.frame(t(sapply(ddf[[col]], as.numeric)))
    colnames(xyz)=paste0(sub("xyz","",col), c("x","y","z"))
    ddf=cbind(xyz, ddf)
  }
  # drop those columns
  ddf=ddf[!colnames(ddf)%in%xyzcols]
  
  # fix any columns that are still lists
  list_cols=sapply(ddf, is.list)
  ddf[list_cols]=lapply(ddf[list_cols], unlist, use.names=F)
  
  # deal with neuron names
  if(get_names){
    # make this a single call for efficiency
    allnames <- catmaid_get_neuronnames(c(ddf$pre_skid, ddf$post_skid), conn = conn, pid=pid, ...)
    ddf$pre_name <- allnames[seq_along(ddf$pre_skid)]
    ddf$post_name <- allnames[seq_along(ddf$post_skid)+length(ddf$pre_skid)]
  }
  
  # move some columns to front
  first_cols=c("pre_skid", "post_skid", "connector_id", "pre_node_id", "post_node_id")
  ddf[c(first_cols, setdiff(colnames(ddf), first_cols))]
}

#' Fetch position and other information for arbitrary treenode ids
#'
#' @details The key feature of this function is that allows you to fetch
#'   information for arbitrary tree nodes that do not need to be from the same
#'   skeleton. Furthermore the nodes can be defined by the presence of labels
#'   (tags) or by a skeleton id. \code{labels} and \code{skids} specifiers can
#'   be combined in order e.g. to find details for the somata for a given set of
#'   skeleton ids. However these queries are slow for more than a few hundred
#'   skids, at which point it is better to fetch using the label and then filter
#'   by skid post hoc in R.
#'
#' @param tnids One or more (integer) treenode ids
#' @param labels One or more (character) labels with which nodes must be tagged
#' @param skids One or more skeleton ids or an expression compatible with
#'   \code{\link{catmaid_skids}} (see Details for advice re many skids)
#' @seealso \code{\link{catmaid_get_treenode_table}},
#'   \code{\link{catmaid_get_connectors}},
#'   \code{\link{catmaid_get_compact_skeleton}}
#' @inheritParams catmaid_get_compact_skeleton
#'
#' @return \itemize{
#'
#'   \item treenode_id,
#'
#'   \item parent_id,
#'
#'   \item x,
#'
#'   \item y,
#'
#'   \item z,
#'
#'   \item confidence,
#'
#'   \item radius,
#'
#'   \item skid,
#'
#'   \item edition_time,
#'
#'   \item user_id
#'
#'   }
#' @export
#'
#' @examples
#' \donttest{
#' # details for 3 nodes from two different skeletons
#' catmaid_get_treenodes_detail(c(9943214L, 25069047L, 12829015L))
#' 
#' # example label search
#' tosoma=catmaid_get_treenodes_detail("to soma")
#' }
#' 
#' \dontrun{
#' # If you have a lot of skids to query you will need to break up your queries
#' # into smaller chunks. You could do this like so:
#' catmaid_get_treenodes_detail_chunked <- function(skids, chunksize=300, chunkstoread=NULL, ...) {
#'   nchunks=ceiling(length(skids)/chunksize)
#'   chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_along(skids)]
#'   
#'   l=list()
#'   if(is.null(chunkstoread)) chunkstoread=seq_len(nchunks)
#'   pb <- progress::progress_bar$new(total = length(skids),
#'                                    format = "  :current/:total [:bar]  eta: :eta",
#'                                    show_after=1)
#'   
#'   for(i in chunkstoread) {
#'     pb$tick(len = sum(chunks==i))
#'     l[[length(l)+1]]=catmaid_get_treenodes_detail(skids=skids[chunks==i], ...)
#'   }
#'   dplyr::bind_rows(l)
#' }
#' 
#' }
catmaid_get_treenodes_detail<-function(tnids=NULL, labels=NULL, skids=NULL, 
                                       pid=1, conn=NULL, raw=FALSE, ...) {
  path=paste("", pid, "treenodes","compact-detail",sep="/")
  
  params=!sapply(list(tnids, labels, skids), is.null)
  if (sum(params) == 0)
    stop("You must specify at least one of tree node ids, node labels",
         " or skeleton id args!")
  if(any(params[2:3])) {
    min.version="2018.07.19"
    if((cv <- catmaid_version(conn = conn, numeric = TRUE)) < min.version)
      stop("Your CATMAID server is running version ", cv, " but version >=",
           min.version, " is required to support label or skid queries!")
  }
  body=list()
  if(!is.null(tnids)){
    body=as.list(tnids)
    names(body)=sprintf("treenode_ids[%d]", seq_along(tnids))
  }
  if(!is.null(labels)){
    body=c(body, label_names=labels)
  }
  if(!is.null(skids)) {
    skids=catmaid_skids(skids)
    skidlist=as.list(skids)
    names(skidlist)=sprintf("skeleton_ids[%d]", seq_along(skids)-1L)
    body=c(body, skidlist)
  }
  
  nodeinfo=catmaid_fetch(path, body=body, conn=conn, simplifyVector = T, ...)
  
  if(raw) return(nodeinfo)
  # else process the connector information
  if(!length(nodeinfo)) return(NULL)
  
  catmaid_error_check(nodeinfo)
  if(!(is.matrix(nodeinfo) && ncol(nodeinfo)==10)){
    stop("Unexpected return format catmaid_get_treenodes_detail!")
  }
  
  coltypes = c(
    "treenode_id"="integer",
    "parent_id"="integer",
    "x"="numeric",
    "y"="numeric",
    "z"="numeric",
    "confidence"="integer",
    "radius"="numeric",
    "skid"="integer",
    "edition_time"="numeric",
    "user_id"="integer"
  )
  colnames(nodeinfo)=names(coltypes)
  nodeinfo=as.data.frame(nodeinfo)
  curcoltypes=sapply(nodeinfo,mode)
  cols_to_change=names(which(coltypes!=curcoltypes))
  for(col in cols_to_change) {
    mode(nodeinfo[[col]]) <- coltypes[col]
  }
  nodeinfo
}


#' Get the number of nodes per skeleton
#'
#' @details This is actually a thin wrapper around the
#'   \code{\link{catmaid_get_review_status}} function, which returns the number
#'   of reviewed and total nodes for one or more neurons. However this function
#'   can efficiently return the number of nodes when there are duplicates in the
#'   input list.
#'
#' @inheritParams catmaid_get_review_status
#' @export
#' @return An integer vector of node counts
#' @examples 
#' \dontrun{
#' catmaid_get_node_count("glomerulus DA2")
#' 
#' # NB handles repeated input efficiently
#' skids=catmaid_skids("glomerulus DA2 right")
#' catmaid_get_node_count(rep(skids,20))
#' }
catmaid_get_node_count <- function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  good_skids=unique(na.omit(skids))
  res=catmaid_get_review_status(good_skids, pid=pid, conn=conn, raw=FALSE, ...)
  res$total[match(skids, good_skids)]
}
