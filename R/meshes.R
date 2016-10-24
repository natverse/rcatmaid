#' Convert a mesh to catmaid format
#' @references 
#' \url{https://github.com/catmaid/CATMAID/blob/master/sphinx-doc/source/volumes.rst}
#' 
#' @param x A mesh object such as hxsurf or a list with raw vertices (Nx3) and 
#'   indices (Nx3).
#' @param ... Additional fields for the catmaid mesh object
#' @export
as.catmaidmesh <- function(x, ...) UseMethod("as.catmaidmesh")

#' @export
#' @rdname as.catmaidmesh
as.catmaidmesh.hxsurf <- function(x, ...) {
  mesh=list(unname(as.matrix(x$Vertices[, 1:3])),
            unname(as.matrix(x$Regions[[1]])-1L))
  as.catmaidmesh(mesh, ...)
}

#' @export
#' @rdname as.catmaidmesh
as.catmaidmesh.mesh3d <- function(x, ...) {
  mesh=list(t(x$vb[1:3, ]),
            t(x$it)-1L)
  as.catmaidmesh(mesh, ...)
}


#' @export
#' @param title The title of the object as it will appear in the catmaid volume manager
#' @param comment An informative comment - e.g. how the mesh was generated.
#' @rdname as.catmaidmesh
as.catmaidmesh.default <- function(x, title=NULL, comment=NULL, ...) {
  nc=ncol(x[[2]])
  if(nc==3) type='trimesh'
  else stop("I only support trimeshes right now")
  list(title=title, type=type, mesh=x, ...)
}

as.catmaidmesh.catmaidmesh <- function(x, ...){
  x
}

#' Add a 3D mesh surface to catmaid volume mananger
#' 
#' @param x A mesh object such as hxsurf or a list with raw vertices and indices
#'   that will be passed to \code{\link{as.catmaidmesh}}
#' @inheritParams read.neuron.catmaid
#' @param ... Additional arguments passed to \code{\link{as.catmaidmesh}} such 
#'   as title, comments etc.
#'   
#' @return An invisible list with the post status repsonse on success (the
#'   function will throw an error on failure)
#' @export
#' @seealso \code{\link{as.catmaidmesh}}
#' @examples
#' \dontrun{
#' library(elmr)
#' FAFBNP.surf=xform_brain(JFRC2NP.surf, sample=JFRC2, ref=FAFB12)
#' for(r in FAFBNP.surf$RegionList[-1]){
#'   comment="Transformed from JFRC2 space onto FAFB12"
#'   name=paste("V12", sep=".", r)
#'   catmaid_volumes_add(subset(FAFBNP.surf, r), title=name, comment=comment)
#' }
#' }
catmaid_volumes_add <- function(x, conn=NULL, pid=1, ...) {
  l=as.catmaidmesh(x, ...)
  res=catmaid_fetch(path="1/volumes/add", body=l, encode='json')
  catmaid_error_check(res)
  invisible(res)
}

