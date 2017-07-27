#' Convert a mesh to CATMAID format
#' 
#' @details The CATMAID mesh format is documented in the reference below and 
#'   typically consists of a json encoded object with fields \itemize{
#'   
#'   \item title
#'   
#'   \item mesh The mesh itself (a list of length 2)
#'   
#'   \item comments (optional)
#'   
#'   }
#'   
#'   In R we hold this list in a \code{list} object with class 
#'   \code{catmaidmesh}. This can then be posted to the volume manager API.
#'   
#'   
#' @references 
#' \url{https://github.com/catmaid/CATMAID/blob/master/sphinx-doc/source/volumes.rst}
#' 
#' @param x A mesh object such as \code{\link[nat]{hxsurf}} or 
#'   \code{\link[rgl]{mesh3d}} object \emph{or} a list with raw vertices (Nx3) 
#'   and indices (Nx3).
#' @param ... Additional fields for the CATMAID mesh object
#' @seealso \code{\link{catmaid_add_volume}}
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
#' @importFrom rgl as.mesh3d
as.mesh3d.catmaidmesh <- function(x, ...) {
  rgl::tmesh3d(vertices = t(x$mesh[[1]]), indices = t(x$mesh[[2]]+1L), homogeneous=F)
}

#' @export
#' @param title The title of the object as it will appear in the catmaid volume manager
#' @param comment An informative comment - e.g. how the mesh was generated.
#' @rdname as.catmaidmesh
as.catmaidmesh.default <- function(x, title=NULL, comment=NULL, ...) {
  nc=ncol(x[[2]])
  if(nc==3) type='trimesh'
  else stop("I only support trimeshes right now")
  l=list(title=title, type=type, mesh=x, ...)
  class(l)='catmaidmesh'
  l
}

as.catmaidmesh.catmaidmesh <- function(x, ...){
  x
}

#' Add a 3D mesh surface to catmaid volume manager
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
#' for(r in FAFBNP.surf$RegionList){
#'   comment="Transformed from JFRC2 space onto FAFB12"
#'   name=paste("V12", sep=".", r)
#'   catmaid_add_volume(subset(FAFBNP.surf, r), title=name, comment=comment)
#' }
#' }
catmaid_add_volume <- function(x, conn=NULL, pid=1, ...) {
  l=as.catmaidmesh(x, ...)
  res=catmaid_fetch(path="1/volumes/add", body=l, encode='json')
  catmaid_error_check(res)
  invisible(res)
}

#' Fetch a mesh volume from CATMAID
#' 
#' @details Note that if \code{x} is a volume name as a character vector then it
#'   must be unique (something that CATMAID servers do not insist on).
#'   
#'   Note also that this function currently only supports
#'   
#' @param x The integer volume id or character volume name
#' @param rval The class to return.
#'   
#' @return When rval \code{mesh3d}, the standard format used by the 
#'   \code{rgl} and \code{nat} packages is the default. \code{catmaidmesh} is a
#'   tidy version of the mesh format used by catmaid. \code{raw} will allow any
#'   return value.
#'   
#' @inheritParams catmaid_get_compact_skeleton
#'   
#' @importFrom xml2 read_xml xml_attr xml_children
#' @seealso \code{\link{catmaid_get_volumelist}}, 
#'   \code{\link{catmaid_add_volume}}, \code{\link{as.catmaidmesh}}, 
#'   \code{\link[rgl]{mesh3d}}, \code{\link[nat]{hxsurf}},
#'   \code{\link[rgl]{shapelist3d}}
#' @examples 
#' \dontrun{
#' v375=catmaid_get_volume(375)
#' v13.AL_R=catmaid_get_volume("v13.AL_R")
#' 
#' v13.AL_R.mesh=as.mesh3d(v13.AL_R)
#' shade3d(v13.AL_R.mesh)
#' 
#' # find surfaces for olfactory glomeruli
#' vl=catmaid_get_volumelist()
#' glomids=vl$id[grepl("_glomerulus$", vl$name)]
#' # fetch them all
#' gg=lapply(glomids, catmaid_get_volume)
#' # ... and plot
#' mapply(shade3d, gg, col=rainbow(length(gg)))
#' 
#' # alternatively wrap them all in an rgl shapelist3d, plotting each object
#' sl=shapelist3d(gg, col=rainbow(length(gg)), plot=TRUE)
#' 
#' }
catmaid_get_volume <- function(x, rval=c("mesh3d","catmaidmesh", "raw"), 
                               conn=NULL, pid=1, ...) {
  if(!is.numeric(x)) {
    vl=catmaid_get_volumelist(conn=conn, pid=pid)
    x=vl$id[vl$name==x]
    if(!length(x))
      stop("No matching volume name!")
    if(length(x)>1)
      stop("Multiple volumes matching that name! Please use volume id.")
  }
  res=catmaid_fetch(paste0('/1/volumes/', x), conn=conn, pid=pid, ...)
  catmaid_error_check(res)
  
  meshxml=read_xml(res$mesh)
  idxs=scan(text=xml_attr(meshxml, 'index'), quiet = TRUE, what=1L)
  verts=scan(text=xml_attr(xml_children(meshxml), 'point'), quiet = TRUE)
  res$x=list(matrix(verts, ncol = 3, byrow = T),
                matrix(idxs, ncol = 3, byrow = T))
  res$mesh=NULL
  res$title=res$name
  res$name=NULL
  # make a catmaidmesh
  rval=match.arg(rval)
  if(rval=='raw') return(res)
  res2=do.call(as.catmaidmesh, res)
  if(rval=='catmaidmesh') res2 else as.mesh3d(res2)
}

#' Fetch a data frame containing details of all volumes on CATMAID server
#' 
#' @inheritParams catmaid_get_compact_skeleton
#' @return A data.frame wiht columns \itemize{
#'   
#'   \item comment
#'   
#'   \item name
#'   
#'   \item creation_time
#'   
#'   \item edition_time
#'   
#'   \item project
#'   
#'   \item user
#'   
#'   \item id
#'   
#'   \item editor }
#'   
#' @export
#' @seealso \code{\link{catmaid_get_volume}}
catmaid_get_volumelist <- function(conn=NULL, pid=1, ...) {
  catmaid_fetch('/1/volumes', simplifyVector = T, conn=conn, pid=pid, include_headers = F, ...)
}
