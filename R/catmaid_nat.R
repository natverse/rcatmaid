#' Read one more neurons in the NeuroAnatomy Toolbox (nat) format
#' 
#' \code{read.neuron.catmaid} reads a single neuron, while 
#' \code{read.neurons.catmaid} generates a neuronlist object including some 
#' metadata information.
#' 
#' @details These functions provide a bridge between CATMAID and the 
#'   neuronanatomy toolbox R package (\url{https://github.com/jefferis/nat}), 
#'   which provides extensive functionality for analysing and plotting neurons
#'   within the context of temaplate brains.
#' @param skid,skids One or more skeleton ids
#' @param pid Project id
#' @param ... Additional arguments passed to the catmaid_fetch function
#' @inheritParams catmaid_fetch
#' @seealso \code{\link{catmaid_fetch}}
#' @export
read.neuron.catmaid<-function(skid, pid, conn=NULL, ...) {
  res=catmaid_get_compact_skeleton(pid=pid, skid=skid, conn=conn, ...)
  if(!length(res$nodes)) stop("no valid nodes for skid:", skid)
  swc=with(res$nodes, 
           data.frame(PointNo=id, Label=0, X=location.x, Y=location.y, Z=location.z, W=radius*2, Parent=parent_id)
  )
  swc$Parent[is.na(swc$Parent)]=-1L
  n=nat::as.neuron(swc)
  n[names(res[-1])]=res[-1]
  fields_to_include=c("url", "headers")
  n[fields_to_include]=attributes(res)[fields_to_include]
  n
}

#' @rdname read.neuron.catmaid
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
#' @examples
#' \dontrun{
#' library(nat)
#' nl=read.neurons.catmaid(c(10418394,4453485), pid=1)
#' plot3d(nl)
#' 
#' ## Full worked example looking at Olfactory Receptor Neurons
#' # Find ORNs
#' conn=catmaid_login()
#' # note use of regex in query
#' orn_query=catmaid_query_by_neuronname("ORN (left|right)", conn=conn)
#' 
#' # Tidy up result data.frame keeping only neurons
#' orn_query=subset(orn_query, type=='neuron')
#' # find the Odorant receptor genes and the side of brain
#' orn_query=transform(orn_query, 
#'    side=factor(ifelse(grepl("left",name), "L", "R")),
#'    Or= factor(sub(" ORN.*", "", name)))
#' # a table of odorant receptor / side of brain
#' ftable(side~Or,data=orn_query)
#' 
#' # set rownames for data.frame so that we can plot neurons by skeleton id
#' rownames(orn_query)=orn_query$skeleton_ids
#' 
#' # now fetch those neurons with progress bar, dropping any failures
#' orns=read.neurons.catmaid(orn_query$skeleton_ids, pid=1, df=orn_query,
#'   OmitFailures = T, .progress='text', conn=conn)
#'   
#' # now some plots
#' open3d()
#' # colour by side of brain
#' plot3d(orns, col=side)
#' # colour by Odorant Receptor
#' # note similar position of axon terminals for same ORN class on left and right
#' plot3d(orns, col=Or)
#' }
read.neurons.catmaid<-function(skids, pid, conn=NULL, OmitFailures=NA, df=NULL, ... ) {
  if(is.null(df)) {
    names(skids)=as.character(skids)
    df=data.frame(pid=pid, skid=skids)
    rownames(df)=names(skids)
  } else {
    names(skids)=rownames(df)
  }
  fakenl=nat::as.neuronlist(as.list(skids), df=df)
  nat::nlapply(fakenl, read.neuron.catmaid, pid=pid, conn=conn, OmitFailures=OmitFailures, ...)
}
