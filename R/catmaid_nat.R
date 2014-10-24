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
#' @details When \code{OmitFailures} is not \code{NA}, \code{FUN} will be 
#'   wrapped in a call to \code{try} to ensure that failure for any single 
#'   neuron does not abort the nlapply/nmapply call. When 
#'   \code{OmitFailures=TRUE} the resultant neuronlist will be subsetted down to
#'   return values for which \code{FUN} evaluated successfully. When 
#'   \code{OmitFailures=FALSE}, "try-error" objects will be left in place. In 
#'   either of the last 2 cases error messages will not be printed because the 
#'   call is wrapped as \code{try(expr, silent=TRUE)}.
#' @export
#' @examples
#' \dontrun{
#' library(nat)
#' nl=read.neurons.catmaid(c(10418394,4453485), pid=1)
#' plot3d(nl)
#' 
#' ## with progress bar
#' conn=catmaid_login()
#' al = catmaid_get_annotationlist(pid=1, conn=conn)
#' lots_of_skids = subset(al$user, name=='albert')$neuron.id
#' # nb we make a progress bar and drop failures
#' nl=read.neurons.catmaid(lots_of_skids, pid=1, conn=conn, .progress='text', OmitFailures=T)
#' }
read.neurons.catmaid<-function(skids, pid, conn=NULL, OmitFailures=NA, ...) {
  if(is.null(names(skids))) names(skids)=paste(pid, skids, sep=".")
  df=data.frame(pid=pid, skid=skids)
  rownames(df)=names(skids)
  fakenl=nat::as.neuronlist(as.list(skids), df=df)
  nat::nlapply(fakenl, read.neuron.catmaid, pid=pid, conn=conn, OmitFailures=OmitFailures, ...)
}
