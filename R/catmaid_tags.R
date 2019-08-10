#' Retrieve soma information for skeletons (specified via CATMAID tags)
#'
#' @description \code{soma} fetches the XYZ location of the soma
#' @description \code{somaindex} fetches the raw index of the skeleton node
#'   associated with the soma (in range 1..N, number of vertices)
#' @description \code{somaid} fetches the identifier of the skeleton node
#'   associated with the soma
#' @param x a \code{\link{neuron}} or \code{\link{neuronlist}} object
#' @param ... additional arguments passed to methods, i.e. \code{\link{nlapply}}
#'
#' @details CATMAID access required. Data collected and described in cited
#'   publication.
#'
#' @references Ohyama T, Schneider-Mizell CM, Fetter RD, Aleman JV, Franconville
#'   R, Rivera-Alba M, Mensh BD, Branson KM, Simpson JH, Truman JW, et al.
#'   (2015) A multilevel multimodal circuit enhances action selection in
#'   Drosophila. Nature.
#' @return Either the soma's 3D coordinates (soma) its PointId number (somaid)
#'   or its position in the neuron's skeleton (somaindex)
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
soma.neuron<-function(x, ...) {
  r=if(length(somaindex<-x$tags$soma[[1]])){
    x$d[match(somaindex, x$d$PointNo),c("X","Y","Z")]
  } else {
    matrix(NA_real_, ncol = 3L, dimnames = list(NULL, c("X","Y","Z") ))
  }
  as.data.frame(r)
}

#' @export
#' @rdname soma
somaindex<-function(x, ...) UseMethod("somaindex")

#' @export
#' @rdname soma
somaindex.neuronlist<-function(x, ...) {
  unlist(nlapply(x, somaindex.neuron, ...))
}

#' @export
#' @rdname soma
somaindex.neuron <- function(x, ...) match(somaid(x), x$d$PointNo)


#' @export
#' @rdname soma
somaid<-function(x, ...) UseMethod("somaid")

#' @export
#' @rdname soma
somaid.neuronlist<-function(x, ...) {
  unlist(nlapply(x, somaid), ...)
}

#' @export
#' @rdname soma
somaid.neuron <- function(x, ...) unlist(x$tags$soma)
