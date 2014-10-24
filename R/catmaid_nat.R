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
  res=catmaid_get_compact_skeleton(pid=pid, skid=skid, conn=NULL, ...)
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
#' @export
#' @examples
#' \dontrun{
#' library(nat)
#' nl=read.neurons.catmaid(c(10418394,4453485), pid=1)
#' plot3d(nl)
#' }
read.neurons.catmaid<-function(skids, pid, conn=NULL, ...) {
  l=lapply(skids, read.neuron.catmaid, pid=pid, conn=conn, ...)
  names(l)=paste(pid, skids, sep=".")
  df=data.frame(pid=pid, skid=skids)
  rownames(df)=names(l)
  nat::as.neuronlist(l, df=df)
}
