#' Prune neuron by splitting it at CATMAID tags
#'
#' @details Split a neuron based on tags assigned in CATMAID. Remove points either downstream (from the root, must be soma to work properly) of the tagged points(s) or upstream.
#'
#' @param x a \code{neuron} or \code{neuronlist} object
#' @param tag a tag that has been assigned in CATMAID
#' @param remove.upstream Logical when \code{TRUE} points downstream of the tag(s) are removed, if true, points upstream are removed
#' @param ... Additional arguments, passed to \code{\link{nlapply}} or eventually to \code{\link{prune_vertices}}
#' @rdname prune_by_tag
#' @export
prune_by_tag <-function(x, ...) UseMethod("prune_by_tag")
#' @rdname prune_by_tag
#' @export
prune_by_tag.neuron <- function(x, tag, remove.upstream = TRUE, ...){
  classes = class(x)
  p = unlist(x$tags[names(x$tags)%in%tag])
  if(is.null(p)){
    stop(paste0("Neuron does not have a tag in: ",tag))
  }
  split.point = as.numeric(rownames(x$d[x$d$PointNo==p,]))
  n = nat::as.ngraph(x)
  leaves = nat::endpoints(x)
  downstream = suppressWarnings(unique(unlist(igraph::shortest_paths(n, split.point, to = leaves, mode = "out")$vpath)))
  x = nat::prune_vertices(x,verticestoprune = downstream, invert = remove.upstream, ...)
  class(x) = classes
  x
}
#' @rdname prune_by_tag
#' @export
prune_by_tag.catmaidneuron<- function(x, tag, remove.upstream = TRUE, ...){
  p = unlist(x$tags[names(x$tags)%in%tag])
  if(is.null(p)){
    stop(paste0("Neuron does not have a tag in: ",tag))
  }
  split.point = as.numeric(rownames(x$d[x$d$PointNo==p,]))
  n = nat::as.ngraph(x)
  leaves = nat::endpoints(x)
  downstream = suppressWarnings(unique(unlist(igraph::shortest_paths(n, split.point, to = leaves, mode = "out")$vpath)))
  pruned = nat::prune_vertices(x,verticestoprune = downstream, invert = remove.upstream, ...)
  pruned$connectors = x$connectors[x$connectors$treenode_id%in%pruned$d$PointNo,]
  relevant.points = subset(x$d, PointNo%in%pruned$d$PointNo)
  y = pruned
  y$d = relevant.points[match(pruned$d$PointNo,relevant.points$PointNo),]
  y$d$Parent = pruned$d$Parent
  class(y) = c("neuron","catmaidneuron")
  y
}
#' @rdname prune_by_tag
#' @export
prune_by_tag.neuronlist <- function(x, tag, remove.upstream = TRUE, ...){
  nlapply(x, tag = tag, prune_by_tag, remove.upstream = remove.upstream)
}

#' Functions for retrieving soma data
#'
#' @description Functions for retrieving data about somata from skeletons, that has been assigned in CATMAID using the tag system
#' @param x a neuron or neuronlist object
#' @param ... additional arguments passed to methods, i.e. \code{nlapply}
#'
#' @details CATMAID access required. Data collected and described in cited publication.
#'
#' @references Ohyama T, Schneider-Mizell CM, Fetter RD, Aleman JV, Franconville R, Rivera-Alba M, Mensh BD, Branson KM, Simpson JH, Truman JW, et al. (2015) A multilevel multimodal circuit enhances action selection in Drosophila. Nature.
#' @return Either the soma's 3D coordinates (soma) its id number (somaid) or its position in the neuron's skeleton (somapos)
#' @export
soma<-function(x, ...) UseMethod("soma")

#' @export
#' @rdname soma
soma.neuronlist<-function(x, ...) {
  rdf=plyr::ldply(x, soma, ...)
  rownames(rdf)=rdf[[1]]
  rdf[-1]
}

#' @export
#' @rdname soma
soma.neuron<-function(x) {
  r=if(length(somaid<-x$tags$soma[[1]])){
    x$d[match(somaid, x$d$PointNo),c("X","Y","Z")]
  } else {
    matrix(NA_real_, ncol = 3L, dimnames = list(NULL, c("X","Y","Z") ))
  }
  as.data.frame(r)
}

#' @export
#' @rdname soma
somaid<-function(x, ...) UseMethod("somaid")

#' @export
#' @rdname soma
somaid.neuronlist<-function(x, ...) {
  unlist(nlapply(x, somaid.neuron, ...))
}

#' @export
#' @rdname soma
somaid.neuron <- function(x) unlist(x$tags$soma)

#' @export
#' @rdname soma
somapos<-function(x, ...) UseMethod("somaid")

#' @export
#' @rdname soma
somapos.neuronlist<-function(x, ...) {
  unlist(nlapply(x, somapos), ...)
}

#' @export
#' @rdname soma
somapos.neuron <- function(x) match(somaid(x), x$d$PointNo)












