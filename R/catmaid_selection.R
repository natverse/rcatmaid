#' Read a CATMAID selection file
#' 
#' @param f Path to the \code{.json} file saved by CATMAID
#' @param readNeurons Whether or not to read the neurons listed in the selection
#'   file
#' @param getNames Whether to fetch the names of the neurons (default 
#'   \code{TRUE}, ignored if neurons are being fetched)
#' @param ... Connection parameters passed to \code{read.neurons.catmaid}
#' @return A \code{data.frame} containing the selection information or (if 
#'   \code{readNeurons=TRUE}) a \code{\link[nat]{neuronlist}} containing this 
#'   information in the attached metadata. The columns will be \itemize{
#'   
#'   \item{skid} The numeric skeleton id
#'   
#'   \item{color} The colour of the selected neuron in catmaid
#'   
#'   \item{opacity} The opacity (alpha value) of the neuron name
#'   
#'   \item{name} Optionally the name of the neuron
#'   
#'   }
#' @export
#' @examples
#' f=system.file('catmaid-skeletons.json', package = 'catmaid')
#' read_catmaid_selection(f, getNames=FALSE)
#' \dontrun{
#' # read in the neurons for the selection
#' x=read_catmaid_selection(f, readNeurons=TRUE)
#' # plot using the CATMAID colour specifications
#' plot3d(x, col=color)
#' }
#' @seealso \code{\link{write_catmaid_selection}}
read_catmaid_selection <- function(f, readNeurons=FALSE, getNames=TRUE, ...) {
  j=jsonlite::read_json(f, simplifyVector = TRUE)
  colnames(j)[colnames(j)=='skeleton_id']='skid'
  
  if(readNeurons) {
    x=read.neurons.catmaid(j$skid, OmitFailures = T, ...)
    m=merge(x[,], j, by='skid', sort=FALSE)
    rownames(m)=m[['skid']]
    data.frame(x)=m
    x
  } else {
    if(getNames)
      j$name=catmaid_get_neuronnames(j$skid, ...)
    class(j)=c(class(j),'catmaid_view')
    j
  }
}


#' Make a CATMAID selection file based on neuronlist or skids
#'
#' @param x A \code{\link{neuronlist}}, data.frame or vector of skids
#' @param f Path to output file (usually ends in .json)
#' @param color Optional vector of colours in any format understood by
#'   \code{\link{col2rgb}}
#' @param opacity Optional vector of opacities (alpha values) in range 0-1
#' @param ... Additional arguments passed to \code{\link[jsonlite]{toJSON}}
#'
#' @export
#' @seealso \code{\link{read_catmaid_selection}}
#' @examples
#' \dontrun{
#' pns=read.neurons.catmaid("annotation:^PN$")
#' # extract the glomerulus from the name
#' pns[, 'glomerulus'] = stringr::str_match(pns[, 'name'],
#'                                          ".*glomerulus ([A-z0-9]+) .*")[, 2]
#' pns[, 'glomerulus']=addNA(factor(pns[, 'glomerulus']))
#' # get the same colours that nat would normally use in a plot
#' plotres=plot3d(pns, col=glomerulus)
#' cols=attr(plotres,'df')$col
#' # write out selection file with those colours
#' write_catmaid_selection(pns, f='pns-by-glom.json', color=cols)
#' }
write_catmaid_selection <- function(x, f, color=NULL, opacity=NULL, ...) {
  if(is.neuronlist(x))
    x=as.data.frame(x)
  if(is.data.frame(x)){
    df=x
    x=df$skid
    # see if the data.frame has colour or opacity information if these are 
    # missing
    if(is.null(color)) {
      colcol=na.omit(match(c("colour", "color", "col"), colnames(df)))
      if(any(is.finite(colcol)))
        color=df[[colcol[1]]]
    }
    if(is.null(opacity)) {
      opac_col=na.omit(match(c("opacity","alpha"), colnames(df)))
      if(any(is.finite(opac_col)))
        opacity=df[[opac_col[1]]]
    }
  }
  df=data.frame(skeleton_id=x)
  if(length(color))
    df$color=normalise_colours(color)
  if(length(opacity))
    df$opacity=opacity
  
  json=jsonlite::toJSON(df, pretty=TRUE, ...)
  json=gsub("  ", " ", json, fixed = T)
  writeLines(json, f)
}

# will take any unitary colour specification ("red", 1, "#FF0000") 
# and turn into a web style colour specification #FF0000
#' @importFrom grDevices col2rgb rgb
normalise_colours <- function(x) {
  if(is.null(x))
    return(NULL)
  cols=apply(col2rgb(x),2, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  tolower(cols)
}
