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
#' @param ... Additional arguments passed to the catmaid_fetch function
#' @seealso \code{\link{catmaid_fetch}}
#' @export
read.neuron.catmaid<-function(skid, pid=1L, conn=NULL, ...) {
  res=catmaid_get_compact_skeleton(pid=pid, skid=skid, conn=conn, ...)
  if(!length(res$nodes)) stop("no valid nodes for skid:", skid)
  swc=with(res$nodes, 
           data.frame(PointNo=id, Label=0, X=x, Y=y, Z=z, W=radius*2, Parent=parent_id)
  )
  swc$Parent[is.na(swc$Parent)]=-1L
  
  # Find soma position, based on plausible tags
  soma_tags<-grep("(cell body|soma)", ignore.case = T, names(res$tags), value = T)
  soma_id=unlist(unique(res$tags[soma_tags]))
  soma_id_in_neuron=intersect(soma_id, swc$PointNo)
  
  if(length(soma_id_in_neuron)>1) {
    soma_d=swc[match(soma_id_in_neuron,swc$PointNo),]
    if(sum(soma_d$Parent<0) == 1 ) {
      # just one end point is tagged as soma, so go with that
      soma_id_in_neuron=soma_d$PointNo[soma_d$Parent<0]
    } else {
      warning("Ambiguous points tagged as soma in neuron: ",skid,". Using first")
      soma_id_in_neuron=soma_id_in_neuron[1]
    }
  } else if(length(soma_id_in_neuron)==0) {
    soma_id_in_neuron=NULL
  }
  n=nat::as.neuron(swc, origin=soma_id_in_neuron)
  
  # add all fields from input list except for nodes themselves
  n[names(res[-1])]=res[-1]
  # we expect connectors field to be null when there are no connectors
  if(length(n$connectors)<1) n$connectors=NULL
  fields_to_include=c("url", "headers")
  n[fields_to_include]=attributes(res)[fields_to_include]
  class(n)=c('catmaidneuron', 'neuron')
  n
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
#' # read in ORNs (using a regex to match their name)
#' # note that use a progress bar drop any failures
#' orns=read.neurons.catmaid("name:ORN (left|right)", OmitFailures = T, .progress='text')
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
  skids=catmaid_skids(skids, conn = conn)
  if(is.null(df)) {
    names(skids)=as.character(skids)
    tagdf=catmaid_get_annotations_for_skeletons(skids, pid=pid, conn=conn)
    taglist=unclass(by(tagdf$annotation, tagdf$skid, as.character))
    
    df=data.frame(pid=pid, skid=skids, 
                  name=catmaid_get_neuronnames(skids, pid=pid, conn=conn),
                  tags=taglist[names(skids)],
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
#' @seealso \code{\link{catmaid_get_connector_table}}, 
#'   \code{\link{catmaid_get_connectors}}
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
connectors<-function(x, ...) UseMethod('connectors')

#' @rdname connectors
#' @export
connectors.catmaidneuron<-function(x, ...) {
  x[['connectors']]
}

connectors.neuron<-function(x, ...) {
  stop("This neuron does not have class 'catmaid.neuronlist' and therefore does",
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
#' @param WithNodes logical indicating whether to plot branch/end points 
#'   (default to \code{FALSE} since they will obscure the synapses).
#' @param ... additional arguments passed to \code{\link[nat]{plot3d.neuron}} 
#'   (synapses). Default: \code{TRUE}.
#' @seealso \code{\link[nat]{plot3d.neuron}}
#' @importFrom rgl plot3d points3d
#' @examples 
#' \dontrun{
#' nl=read.neurons.catmaid(c(10418394,4453485))
#' plot3d(nl)
#' 
#' # now with connectors (i.e. synapses)
#' plot3d(nl, WithConnectors=TRUE)
#' }
#' @aliases plot3d
plot3d.catmaidneuron<-function(x, WithConnectors=FALSE, WithNodes=FALSE, ...) {
  rglreturnlist=NextMethod()
  if(WithConnectors) {
    conndf=connectors(x)
    rglreturnlist[['synapses']]=points3d(
      xyzmatrix(conndf),col=c(pre='red', post='cyan')[conndf$prepost+1])
  }
  rglreturnlist
}

#' @export
`*.catmaidneuron` <- function(n,x) {
  n2=NextMethod()
  conndf=connectors(n)
  if(!is.null(conndf)) {
    # multiply connectors as well
    lx=length(x)
    if(lx==1) xyzmatrix(conndf)=xyzmatrix(conndf)*x
    else {
      xyzmatrix(conndf)=t(t(xyzmatrix(conndf)*x[1:3]))
    }
    n2[['connectors']]=conndf
  }
  n2
}

#' @export
`+.catmaidneuron` <- function(n,x) {
  n2=NextMethod()
  conndf=connectors(n)
  if(!is.null(conndf)) {
    # multiply connectors as well
    lx=length(x)
    if(lx==1) xyzmatrix(conndf)=xyzmatrix(conndf)+x
    else {
      xyzmatrix(conndf)=t(t(xyzmatrix(conndf)+x[1:3]))
    }
    n2[['connectors']]=conndf
  }
  n2
}

#' @export
summary.catmaidneuron<-function(object, ...) {
  nc=nrow(connectors(object))
  if(is.null(nc)) nc=0L
  data.frame(nodes=nrow(object$d), 
             cable.length=sum(seglengths(object)),
             connectors=nc)
}
