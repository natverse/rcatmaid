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
#' @return An invisible list with the post status response on success (the
#'   function will throw an error on failure)
#' @export
#' @seealso \code{\link{as.catmaidmesh}}
#' @examples
#' \dontrun{
#' library(elmr)
#' FAFBNP.surf=xform_brain(JFRC2NP.surf, sample=JFRC2, ref=FAFB14)
#' for(r in FAFBNP.surf$RegionList){
#'   comment="Transformed from JFRC2 space onto FAFB14"
#'   name=paste("v14", sep=".", r)
#'   catmaid_add_volume(subset(FAFBNP.surf, r), title=name, comment=comment)
#' }
#' }
catmaid_add_volume <- function(x, conn=NULL, pid=1, ...) {
  l=as.catmaidmesh(x, ...)
  res=catmaid_fetch(path=paste0(pid,"/volumes/add"), body=l, encode='json')
  catmaid_error_check(res)
  invisible(res)
}

#' Fetch a mesh volume from CATMAID
#' 
#' @details Note that if \code{x} is a volume name as a character vector then it
#'   must be unique (something that CATMAID servers do not insist on).
#'   
#'   Note also that this function is targeted at mesh volumes, but CATMAID also 
#'   support box volumes - these can still be returned and will have a valid set
#'   of vertices but an invalid set of indices.
#'   
#'   I have noticed that some CATMAID meshes are returned with the normals
#'   facing into the mesh (the opposite of what one would expect). This causes
#'   \code{\link[nat]{pointsinside}} to do the opposite of what one would
#'   expect. Use \code{invertFaces=TRUE} to fix this.
#'   
#' @param x The integer volume id or character volume name
#' @param rval The class to return.
#' @param invertFaces Whether to invert the faces (swapping inside and outside, 
#'   see details).
#'   
#' @return When rval \code{mesh3d}, the standard format used by the \code{rgl} 
#'   and \code{nat} packages is the default. \code{catmaidmesh} is a tidy 
#'   version of the mesh format used by catmaid. \code{raw} will allow any 
#'   return value.
#'   
#' @inheritParams catmaid_get_compact_skeleton
#' @export
#' @importFrom xml2 read_xml xml_attr xml_children
#' @seealso \code{\link{catmaid_get_volumelist}}, 
#'   \code{\link{catmaid_add_volume}}, \code{\link{as.catmaidmesh}}, 
#'   \code{\link[rgl]{mesh3d}}, \code{\link[nat]{hxsurf}}, 
#'   \code{\link[rgl]{shapelist3d}}
#' @examples 
#' ## NB all these examples refer to the FAFB adult Drosophila brain
#' ## CATMAID instance
#' \donttest{
#' v14.neuropil=catmaid_get_volume('v14.neuropil')
#' # specifying by name is easier / less fragile than numeric ids
#' }
#' 
#' \dontrun{
#' AL_R=catmaid_get_volume("AL_R")
#' shade3d(AL_R, col='red', alpha=.3)
#' 
#' if(require("Morpho")) {
#'   plotNormals(facenormals(v13.AL_R), long=5e3)
#' }
#' 
#' # find surfaces for olfactory glomeruli
#' vl=catmaid_get_volumelist()
#' x
#' glomids=vl$id[grepl("v14.[DV][MLAPC]{0,1}[0-9]{0,1}[dvaplm]{0,1}$", vl$name)]
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
                               invertFaces=FALSE, conn=NULL, pid=1, ...) {
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
  # we need to reorder triangles to ensure normals are pointing out
  if(isTRUE(invertFaces))
    res$x[[2]]=res$x[[2]][,c(1,3,2)]
  res2=do.call(as.catmaidmesh, res)
  if(rval=='catmaidmesh') res2 else as.mesh3d(res2)
}

#' Fetch a data frame containing details of all volumes on CATMAID server
#'
#' @inheritParams catmaid_get_compact_skeleton
#' @return Since \code{CATMAID v2018.07.19-457-ga09910b} a \code{data.frame}
#'   with columns \itemize{
#'
#'   \item id
#'
#'   \item name
#'
#'   \item comment
#'
#'   \item user_id
#'
#'   \item editor_id
#'
#'   \item project_id
#'
#'   \item creation_time
#'
#'   \item edition_time
#'
#'   \item annotations
#'
#'   }
#'
#' @export
#' @examples 
#' \donttest{
#' vl <- catmaid_get_volumelist()
#' head(vl)
#' # find meshes whose name matches neuropil
#' subset(vl, grepl('neuropil', name))
#' }
#' @seealso \code{\link{catmaid_get_volume}}
catmaid_get_volumelist <- function(conn=NULL, pid=1, ...) {
  old_version=catmaid_version(conn = conn, numeric = TRUE)<"2018.07.19-457"
  req = catmaid_fetch(
    '/1/volumes',
    parse.json = FALSE,
    conn = conn,
    pid = pid,
    include_headers = F,
    ...
  )
  
  text <- content(req, as = "text", encoding = "UTF-8")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  
  if(old_version){
    vols <- fromJSON(text, simplifyVector = TRUE, ...)
    catmaid_error_check(vols)
  } else {
    vols=fromJSON(text, simplifyVector = FALSE, ...)
    catmaid_error_check(vols)
    df=list2df(vols$data, unlist(vols$columns), stringsAsFactors=FALSE)
    colnames(df)=vols$columns
    df
  }
}
