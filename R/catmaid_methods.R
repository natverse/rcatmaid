# Methods for catmaidneurons

#' @export
#' @aliases resample
#' @importFrom nat resample
resample.catmaidneuron<-function(x, stepsize=1, ...){
  r=NextMethod(x)
  c = tryCatch(connectors(x), error = function(e) NULL)
  if(!is.null(c)) {
    c$treenode_id = nabor::knn(
      data = nat::xyzmatrix(r),
      query = nat::xyzmatrix(c),
      k = 1
    )$nn.idx
    r$connectors = c
  }
  r
}

#' Prune nodes from a catmaid neuron, keeping the synapses
#'
#' @description Prune nodes from a catmaid neuron, keeping the synapses
#'
#' @param x a neuron object
#' @param target Nodes ids for removal
#' @param maxdist The threshold distance for keeping points
#' @param keep Whether to keep points in x that are near or far from the target
#' @param return.indices Whether to return the indices that pass the test rather
#'   than the 3D object/points (default FALSE)
#' @param ... additional arguments passed to methods (i.e.
#'   \code{\link[nat]{prune}}).
#' @return A pruned neuron object
#' @export
#' @aliases prune
#' @importFrom nat prune
#' @seealso \code{\link[nat]{prune}}
prune.catmaidneuron<- function (x,target,maxdist, keep = c("near", "far"),
                                return.indices = FALSE,...){
  # the "correct" way to do this is to use NextMethod() but the absence of
  # unit tests, I am not going to fiddle.
  pruned = nat:::prune.neuron(x,target=target, maxdist=maxdist, keep = keep,
                              return.indices = return.indices, ...)
  pruned$connectors = x$connectors[x$connectors$treenode_id%in%pruned$d$PointNo,]
  relevant.points = x$d[x$d$PointNo%in%pruned$d$PointNo,] 
  y = pruned
  y$d = relevant.points[match(pruned$d$PointNo,relevant.points$PointNo),]
  y$d$Parent = pruned$d$Parent
  y
}

#' Prune vertices from a CATMAID neuron, keeping the synapses
#'
#' @description Prune nodes from a catmaid neuron, keeping the synapses
#'
#' @param x a CATMAID neuron object
#' @inheritParams nat::prune_vertices
#' @param ... additional arguments passed to methods
#'   \code{\link[nat]{prune_vertices}}).
#' @return A pruned neuron object
#' @export
#' @aliases prune_vertices
#' @importFrom nat prune_vertices
#' @seealso \code{\link[nat]{prune_vertices}}
prune_vertices.catmaidneuron <- function (x,verticestoprune, invert = FALSE,...){
  pruned = nat::prune_vertices(x,verticestoprune,invert = invert,...)
  pruned$connectors = x$connectors[x$connectors$treenode_id%in%pruned$d$PointNo,]
  relevant.points = x$d[x$d$PointNo%in%pruned$d$PointNo,] 
  y = pruned
  y$d = relevant.points[match(pruned$d$PointNo,relevant.points$PointNo),]
  y$d$Parent = pruned$d$Parent
  y$tags = lapply(x$tags, function(t) t[t%in%pruned$d$PointNo])
  y$url = x$url
  y$headers = x$headers
  y$AD.segregation.index = x$AD.segregation.index
  class(y) = c("catmaidneuron","neuron")
  y
}


#' Prune a CATMAID neuron by removing segments with a given Strahler order
#'
#' @description Prune branches by Strahler order from a catmaid neuron, keeping the synapses
#'
#' @param x a CATMAID neuron object
#' @inheritParams nat::prune_strahler
#' @param ... additional arguments passed to methods
#' @return A pruned neuron object
#' @export
#' @aliases prune_strahler
#' @importFrom nat prune_strahler
#' @seealso \code{\link[nat]{prune_strahler}}
prune_strahler.catmaidneuron <- function(x, orderstoprune = 1:2, ...){
  tryCatch(prune_vertices.catmaidneuron(x, which(nat::strahler_order(x)$points %in%
                                                   orderstoprune), ...), error = function(c) stop(paste0("No points left after pruning. ",
                                                                                                         "Consider lowering orders to prune!")))
}
