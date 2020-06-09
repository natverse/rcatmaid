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
#' @return An Nx3 matrix (\code{soma}) or an integer vector (\code{somaid},
#'   \code{somaindex}) with missing values filled with \code{NA}. Should a
#'   neuron have multiple soma tags then \code{somaid} and \code{somaindex} will
#'   return both (resulting in the \code{*.neuronlist} methods returning a
#'   \code{list} not a \code{vector}) but \code{soma} will select the first
#'   coordinates (with a warning) so that it always returns just one xyz
#'   location.
#'
#' @export
#' @seealso \code{\link{nsoma}}
soma<-function(x, ...) UseMethod("soma")

#' @export
#' @rdname soma
soma.neuronlist<-function(x, ...) {
  rdf=t(sapply(x, soma))
  colnames(rdf)=c("X","Y","Z")
  rdf
}

#' @export
#' @rdname soma
soma.neuron<-function(x, ...) {
  sidx=suppressWarnings(somaindex(x))
  if(length(sidx)==0) {
    matrix(NA_real_, ncol = 3L, dimnames = list(NULL, c("X","Y","Z") ))
  } else {
    res=data.matrix(x$d[sidx, c("X","Y","Z")], rownames.force=F)
    if(nrow(res)>1) warning("Neuron has multiple soma tags. Keeping first!")
    res[1, , drop=FALSE]
  }
}

#' @export
#' @rdname soma
somaindex<-function(x, ...) UseMethod("somaindex")

#' @export
#' @rdname soma
somaindex.neuronlist<-function(x, ...) {
  sapply(x, somaindex, ...)
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
  sapply(x, somaid, ...)
}

#' @export
#' @rdname soma
somaid.neuron <- function(x, ...) {
  id=unlist(x$tags$soma)
  if(!length(id)) id=NA_integer_
  else if(length(id)>1) warning("Neuron has multiple soma tags!")
  id
}
