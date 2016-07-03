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
  if(identical(skel[[1]],"Exception"))
    stop("Failed to read skeleton: ", skid, "\ncatmaid error: ", skel[['error']])
  names(skel)=c("nodes", "connectors", "tags")
  
  if(raw) return(skel)
  # else process the skeleton
  if(length(skel$nodes))
    skel$nodes=list2df(skel$nodes, 
                     cols=c("id", "parent_id", "user_id", "x","y", "z", "radius", "confidence"))
  
  if(length(skel$connectors))
    skel$connectors=list2df(skel$connectors, 
                            cols=c("treenode_id", "connector_id", "prepost", "x", "y", "z"))
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
      raw_col=unlist(raw_col)
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
#' @seealso \code{\link{catmaid_get_connector_table}}
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
#' @param skid Numeric skeleton id
#' @param direction whether to find incoming or outgoing connections
#' @inheritParams catmaid_get_compact_skeleton
#' @return A data.frame with columns \itemize{
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
#'   \item partner_treenode_id
#'   
#'   \item last_modified
#'   
#'   }
#' @export
#' @seealso \code{\link{catmaid_get_connectors}}
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
catmaid_get_connector_table<-function(skid, 
                                      direction=c("both", "incoming", "outgoing"),
                                      pid=1, conn=NULL, raw=FALSE, ...) {
  direction=match.arg(direction)
  if(direction[1]=='both') {
    dfin =catmaid_get_connector_table(skid, direction='incoming', pid=pid, conn=conn, raw=raw, ...)
    dfout=catmaid_get_connector_table(skid, direction='outgoing', pid=pid, conn=conn, raw=raw, ...)
    dfin$direction="incoming"
    dfout$direction="outgoing"
    df=rbind(dfin,dfout)
    df$direction=factor(df$direction)
    return(df)
  }
  # relation_type 0 => incoming
  ctl=catmaid_fetch(path=paste0("/", pid, "/connector/table/list"),
                   body=list(skeleton_id=skid, 
                             relation_type=ifelse(direction=="incoming",0,1)), 
                   conn=conn, ...)
  
  if(raw) return(ctl)
  # else process the connector information
  dfcolnames=c("connector_id", "partner_skid", "x", "y", "z", "s", "confidence", 
    "tags", "nodes_in_partner", "username", "partner_treenode_id", 
    "last_modified")
  df=list2df(ctl[[1]], cols = dfcolnames, return_empty_df = T, stringsAsFactors=FALSE)
  df$username=factor(df$username)
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
#'   \item type
#'   
#'   \item tags
#'   
#'   \item confidence
#'   
#'   \item x
#'   
#'   \item y
#'   
#'   \item z
#'   
#'   \item s
#'   
#'   \item r
#'   
#'   \item user
#'   
#'   \item last_modified
#'   
#'   \item reviewer
#'   
#'   }
#' @export
#' @examples 
#' \dontrun{
#' # get tree node table for neuron 10418394
#' tnt=catmaid_get_treenode_table(10418394)
#' # show all leaf nodes
#' subset(tnt, type=="L")
#' # table of node types
#' table(tnt$type)
#' }
#' @seealso \code{\link{catmaid_get_compact_skeleton}}. \code{\link{read.neuron.catmaid}}
catmaid_get_treenode_table<-function(skid, pid=1, conn=NULL, raw=FALSE, ...) {
  # relation_type 0 => incoming
  tnl=catmaid_fetch(path=paste0("/", pid, "/treenode/table/",skid,"/content"),
                    conn=conn, simplifyVector = TRUE, ...)
  
  if(raw) return(tnl)
  # else process the tree node information
  # this comes in 3 separate structures:
  # treenodes, reviews, tags
  if(length(tnl)!=3)
    stop("I don't understand the raw treenode structure returned by catmaid")
  names(tnl)=c("treenodes", "reviews", "tags")
  tnl=lapply(tnl, as.data.frame, stringsAsFactors=FALSE)
  
  colnames(tnl$treenodes)=c("id", "parent_id", "confidence", "x", "y", "z", "r",
                            "user_id", "last_modified")
  idcols=grepl("id", colnames(tnl$treenodes), fixed = TRUE)
  tnl$treenodes[idcols]=lapply(tnl$treenodes[idcols], as.integer)
  
  colnames(tnl$reviews)=c("id", "reviewer_id")
  
  colnames(tnl$tags)=c("id", "tag")
  tnl$tags=as.data.frame(tnl$tags, stringsAsFactors = FALSE)
  tnl$tags$id=as.integer(tnl$tags$id)
  
  tndf=merge(tnl$treenodes, tnl$reviews, by='id')
  attr(tndf, 'tags')=tnl$tags
  tndf
}

