#' @export
xform.catmaidneuron <- function(x, reg, ...) {
  xt=NextMethod(x)
  conns <- connectors(x)
  if(!is.null(conns)) {
    xyzmatrix(conns) <-  xform(xyzmatrix(conns), reg=reg, ...)
    xt[['connectors']] <- conns
  }
  xt
}
