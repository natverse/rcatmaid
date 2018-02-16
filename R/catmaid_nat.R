#' Read neuron(s) from CATMAID server into NeuroAnatomy Toolbox (nat) format
#' 
#' \code{read.neuron.catmaid} reads a single neuron, while 
#' \code{read.neurons.catmaid} generates a \code{\link[nat]{neuronlist}} object 
#' including some metadata information.
#' 
#' @details These functions provide a bridge between CATMAID and the 
#'   neuronanatomy toolbox R package (\url{https://github.com/jefferis/nat}), 
#'   which provides extensive functionality for analysing and plotting neurons 
#'   within the context of temaplate brains.
#'   
#'   Note that the soma is set by inspecting CATMAID tags that 
#'   (case-insensitively) match the regex \code{"(cell body|soma)"}. Where >1 
#'   tag exists the one that tags an endpoint is preferred.
#' @param skid A numeric skeleton id
#' @param pid Project id (default 1)
#' @param conn A \code{catmaid_connection} objection returned by 
#'   \code{\link{catmaid_login}}. If \code{NULL} (the default) a new connection 
#'   object will be generated using the values of the \bold{catmaid.*} package 
#'   options as described in the help for \code{\link{catmaid_login}}.
#' @param ... Additional arguments passed to the \code{\link{catmaid_fetch}} 
#'   function
#' @return a \code{\link[nat]{neuron}} or \code{\link[nat]{neuronlist}} object 
#'   containing one or more neurons. These neurons will have an additional class
#'   \code{catmaidneuron} which provides for some extra functionality in certain
#'   methods.
#' @seealso \code{\link{plot3d.catmaidneuron}}, \code{\link[nat]{read.neuron}}, 
#'   \code{\link{connectors}} to extract connector information from a 
#'   \code{catmaid.neuron}
#' @export
read.neuron.catmaid<-function(skid, pid=1L, conn=NULL, ...) {
  res=catmaid_get_compact_skeleton(pid=pid, skid=skid, conn=conn, ...)
  if(!length(res$nodes)) stop("no valid nodes for skid:", skid)
  swc=with(res$nodes, 
           data.frame(PointNo=id, Label=0, X=x, Y=y, Z=z, W=radius*2, Parent=parent_id)
  )
  swc$Parent[is.na(swc$Parent)]=-1L
  sp=somapos.catmaidneuron(swc=swc, tags=res$tags)
  soma_id_in_neuron = if(nrow(sp)==0) NULL else sp$PointNo
  n=nat::as.neuron(swc, origin=soma_id_in_neuron, skid=skid)
  
  # add all fields from input list except for nodes themselves
  n[names(res[-1])]=res[-1]
  # we expect connectors field to be null when there are no connectors
  if(length(n$connectors)<1) n$connectors=NULL
  fields_to_include=c("url", "headers")
  n[fields_to_include]=attributes(res)[fields_to_include]
  class(n)=c('catmaidneuron', 'neuron')
  n
}

somapos.catmaidneuron <- function(x, swc=x$d, tags=x$tags, skid=NULL, ...) {
  # Find soma position, based on plausible tags
  soma_tags<-grep("(cell body|soma)", ignore.case = T, names(tags), value = T)
  # soma is the preferred tag - use this for preference if it exists
  if(any(soma_tags=="soma")) soma_tags="soma"
  soma_id=unlist(unique(tags[soma_tags]))
  soma_id_in_neuron=intersect(soma_id, swc$PointNo)
  
  soma_d=swc[match(soma_id_in_neuron,swc$PointNo),,drop=FALSE]
  if(length(soma_id_in_neuron)>1) {
    if(sum(soma_d$Parent<0) == 1 ) {
      # just one end point is tagged as soma, so go with that
      soma_d[soma_d$Parent<0,, drop=FALSE]
    } else {
      warning("Ambiguous points tagged as soma in neuron: ",skid,". Using first")
      soma_d[1,, drop=FALSE]
    }
  } else soma_d
}

#' @rdname read.neuron.catmaid
#' @param skids One or more numeric skeleton ids or a character vector defining
#'   a query (see \code{\link{catmaid_skids}} or examples for the syntax).
#' @param OmitFailures Whether to omit neurons for which \code{FUN} gives an 
#'   error. The default value (\code{NA}) will result in nlapply stopping with 
#'   an error message the moment there is an eror. For other values, see 
#'   details.
#' @param df Optional data frame containing information about each neuron
#'   
#' @details When \code{OmitFailures} is not \code{NA}, \code{FUN} will be 
#'   wrapped in a call to \code{try} to ensure that failure for any single 
#'   neuron does not abort the nlapply/nmapply call. When 
#'   \code{OmitFailures=TRUE} the resultant neuronlist will be subsetted down to
#'   return values for which \code{FUN} evaluated successfully. When 
#'   \code{OmitFailures=FALSE}, "try-error" objects will be left in place. In 
#'   either of the last 2 cases error messages will not be printed because the 
#'   call is wrapped as \code{try(expr, silent=TRUE)}.
#'   
#'   The optional dataframe (\code{df}) detailing each neuron should have 
#'   \code{rownames} that match the names of each neuron. It would also make 
#'   sense if the same key was present in a column of the data frame. If the 
#'   dataframe contains more rows than neurons, the superfluous rows are dropped
#'   with a warning. If the dataframe is missing rows for some neurons an error 
#'   is generated. If SortOnUpdate is TRUE then updating an existing neuronlist 
#'   should result in a new neuronlist with ordering identical to reading all 
#'   neurons from scratch.
#' @export
#' @seealso \code{\link{catmaid_skids}}
#' @examples
#' \dontrun{
#' library(nat)
#' nl=read.neurons.catmaid(c(10418394,4453485))
#' plot3d(nl)
#' 
#' ## Full worked example looking at Olfactory Receptor Neurons
#' # read in ORNs (using exact match to ORN annotation)
#' # note that use a progress bar drop any failures
#' orns=read.neurons.catmaid("ORN", OmitFailures = T, .progress='text')
#' 
#' # Add two extra columns to the attached data.frame
#' # for the Odorant receptor genes and the side of brain
#' orns[,'side']=factor(ifelse(grepl("left", orns[,'name']), "L", "R"))
#' orns[,'Or']= factor(sub(" ORN.*", "", orns[,'name']))
#'    
#' # check what we have
#' # see ?head.neuronlist and ?with.neuronlist for details of how this works
#' head(orns)
#' with(orns, ftable(side~Or))
#' 
#' # now some plots
#' open3d()
#' # colour by side of brain
#' plot3d(orns, col=side)
#' clear3d()
#' # colour by Odorant Receptor
#' # note similar position of axon terminals for same ORN class on left and right
#' plot3d(orns, col=Or)
#' 
#' ## Additional example using Olfactory Projection Neurons
#' pns=read.neurons.catmaid("annotation:ORN PNs$", .progress='text')
#' pns[,'side']=factor(ifelse(grepl("left", pns[,'name']), "L", "R"))
#' pns[,'Or']= factor(sub(" PN.*", "", pns[,'name']))
#' 
#' # check that we have the same levels for the Odorant Receptor (Or) factor
#' # for ORNs and PNs
#' stopifnot(levels(pns[,'Or'])==levels(orns[,'Or']))
#' 
#' # Ok, let's plot the PNs - they will be in matching colours
#' plot3d(pns, col=Or)
#' 
#' }
read.neurons.catmaid<-function(skids, pid=1L, conn=NULL, OmitFailures=NA, df=NULL, ... ) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  if(is.null(df)) {
    names(skids)=as.character(skids)
    df=data.frame(pid=pid, skid=skids, 
                  name=catmaid_get_neuronnames(skids, pid, conn=conn),
                  stringsAsFactors = F)
    rownames(df)=names(skids)
  } else {
    names(skids)=rownames(df)
  }
  fakenl=nat::as.neuronlist(as.list(skids), df=df)
  nat::nlapply(fakenl, read.neuron.catmaid, pid=pid, conn=conn, OmitFailures=OmitFailures, ...)
}

#' Get data.frame of connector (synapse) information from a neuron or neuronlist
#' 
#' In contrast to \code{\link{catmaid_get_connector_table}} this assumes that
#' you have already read the neurons into an R structure of class 
#' \code{\link[nat]{neuron}} or \code{\link[nat]{neuronlist}}.
#' 
#' @param x Neuron or neuronlist
#' @param ... Additional arguments passed to methods (and to \code{nlapply} in 
#'   the case of connectors)
#' @return A data.frame with columns \itemize{
#'   
#'   \item treenode_id (NB this is the treenode id for the \emph{current} skeleton)
#'   
#'   \item connector_skid
#'   
#'   \item prepost integer indicating whether connection is pre-(\code{0}) or 
#'   post(\code{1})-synaptic with respect to the current neuron. In other words 
#'   this field will be \code{0} (pre) for the output synapses of this neuron.
#'   
#'   \item x Spatial Location
#'   
#'   \item y
#'   
#'   \item z
#'   
#'   \item skid For \code{connectors.neuronlist}, the skid of the skeleton from
#'   which connector information was retrieved.
#'   
#'   }
#'   
#' @export
#' @examples
#' \dontrun{
#' ornsl=read.neurons.catmaid("name:ORN left", OmitFailures = T, .progress='text')
#' conndf=connectors(ornsl)
#' summary(connectors(ornsl))
#' 
#' # plot points in 3d
#' library(nat)
#' nopen3d()
#' points3d(xyzmatrix(conndf), col=c(pre='red', post='cyan')[conndf$prepost+1])
#' }
#' @family connectors
connectors<-function(x, ...) UseMethod('connectors')

#' @rdname connectors
#' @export
connectors.catmaidneuron<-function(x, ...) {
  x[['connectors']]
}

connectors.neuron<-function(x, ...) {
  stop("This neuron does not have class 'catmaid.neuron' and therefore does",
       " not have connector information!")
}

#' @rdname connectors
#' @param subset, optional subset of neurons to keep (see 
#'   \code{\link[nat]{nlapply}} for details)
#' @export
#' @seealso \code{\link[nat]{nlapply}}
#' @importFrom plyr rbind.fill
connectors.neuronlist<-function(x, subset=NULL, ...) {
  dfs=nat::nlapply(x, FUN=connectors, ..., subset=subset)
  # drop any null return values
  dfs=dfs[!sapply(dfs,is.null)]
  df=rbind.fill(dfs)
  df$skid=as.integer(rep(names(dfs), sapply(dfs, nrow)))
  df
}

#' Plot skeleton and connectors for neuron retrieved from CATMAID
#' 
#' @export
#' @method plot3d catmaidneuron
#' @param x A neuron to plot
#' @param WithConnectors logical indicating whether or not to plot connectors
#'   (defaults to \code{FALSE}).
#' @param WithNodes logical indicating whether to plot branch/end points 
#'   (defaults to \code{FALSE} since they will obscure the synapses).
#' @param ... additional arguments passed to \code{\link[nat]{plot3d.neuron}} 
#'   (synapses). Default: \code{TRUE}.
#' @seealso \code{\link[nat]{plot3d.neuron}}
#' @importFrom rgl plot3d points3d
#' @inheritParams nat::plot3d.neuron
#' @examples 
#' \dontrun{
#' nl=read.neurons.catmaid(c(10418394,4453485))
#' plot3d(nl)
#' 
#' # now with connectors (i.e. synapses)
#' plot3d(nl, WithConnectors=TRUE)
#' }
#' @aliases plot3d
plot3d.catmaidneuron<-function(x, WithConnectors=FALSE, WithNodes=FALSE, soma=FALSE, ...) {
  soma=plot3d_somarad(x, soma)
  rglreturnlist=NextMethod(WithNodes=WithNodes, soma=soma)
  if (WithConnectors) {
    conndf = connectors(x)
    # only try to plot if the neuron has connectors
    if (length(conndf)){
      rglreturnlist[['synapses']] = 
        points3d(xyzmatrix(conndf), col = c(pre ='red', post = 'cyan')[conndf$prepost + 1])
    }
  }
  invisible(rglreturnlist)
}

# private function to choose plotting radius for a neuron
plot3d_somarad <- function(x, soma=FALSE){
  if(is.logical(soma) && !soma) {
    # do nothing, soma is FALSE
  } else if(is.numeric(soma) && soma<=0){
    # this signals that we really want to plot a sphere at the origin
    # regardless of whether it is tagged as a soma or not
    soma=-soma
  } else {
    # check if this neuron has a soma and whether it has a sensible radius
    sp=somapos.catmaidneuron(x)
    # no soma defined so we don't want to plot
    if(nrow(sp)==0) soma=FALSE
    else if(is.numeric(soma) && is.finite(soma)) {
      # if we got passed a numeric value for soma then accept that
    } else {
      # we didn't get passed a numeric value for soma, so let's 
      # see if we can come up with a sensible number
      if(sp[,'W']<=0) soma=TRUE else {
        soma=sp[,'W']/2
        # check that we don't have a stupid value e.g. because diameter
        # was not transformed
        if(soma>max(x$d[,c("X","Y","Z")], na.rm = TRUE))
          soma=T
      }
    }
  }
  soma
}

#' @export
`Ops.catmaidneuron` <- function(e1, e2) {
  n2=NextMethod()
  conndf=connectors(e1)
  if(!is.null(conndf)) {
    # multiply connectors as well
    lx=length(e2)
    if(lx==1) xyzmatrix(conndf)=xyzmatrix(conndf)*e2
    else {
      xyzmatrix(conndf)=t(t(xyzmatrix(conndf)*e2[1:3]))
    }
    n2[['connectors']]=conndf
  }
  n2
}


nsoma <- function(x) length(x[['tags']][['soma']])

#' @export
summary.catmaidneuron<-function(object, ...) {
  df=NextMethod()
  nc=nrow(connectors(object))
  if(is.null(nc)) nc=0L
  df$connectors=nc
  df$nsoma=nsoma(object)
  df
}

#' Copy the tag / connector info from a catmaid neuron to a new neuron
#' @description This function is intended primarily for developer use as a way 
#'   to copy over connector/tag information typically contained in CATMAID
#'   neurons but lost when nat functions are applied to transform a neuron.
#' @details Both connectors and tag information contain node ids. These may need
#'   to be mapped onto new node ids if these are not consistent between old and 
#'   new neurons.
#' @export
#' @param new,old information will be copied from old -> new
#' @param update_node_ids whether to update the node ids (default \code{TRUE})
#' @return A new neuron containing tag and connector information from the old 
#'   neuron and having the same class as the old neuron.
#' @family catmaidneuron
#' @importFrom nabor knn
copy_tags_connectors <- function(new, old, update_node_ids=TRUE) {
  ## connectors
  c = connectors(old)
  if(update_node_ids) {
  nnres = nabor::knn(
    data = nat::xyzmatrix(new),
    query = nat::xyzmatrix(c),
    k = 1
  )
  # note that we map indices into the point array onto PointNo
  c$treenode_id=new$d$PointNo[nnres$nn.idx]
  }
  new[['connectors']] = c

  ## tags
  # replace the tag ids using a similar strategy
  old_tag_ids = unlist(old$tags, use.names = F)
  if (!update_node_ids) {
    new[['tags']] = old[['tags']]
  } else {
    nnres = nabor::knn(
      data = nat::xyzmatrix(new),
      query = nat::xyzmatrix(old)[match(old_tag_ids, old$d$PointNo), ],
      k = 1
    )
    new_tag_ids = new$d$PointNo[nnres$nn.idx]
    new$tags = old$tags
    k = 0
    for (i in seq_along(old$tags)) {
      for (j in seq_along(old$tags[[i]])) {
        k = k + 1
        new$tags[[i]][[j]] = new_tag_ids[k]
      }
      names(new$tags[[i]]) = names(old$tags[[i]])
    }
    names(new$tags) = names(old$tags)
  }
  class(new)=class(old)
  new
}
