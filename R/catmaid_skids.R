#' Find skeleton ids (skids) for various inputs including textual queries
#' 
#' @details If the inputs are numeric or have length > 1 they are assumed 
#'   already to be skids and are simply converted to integers. If the the input 
#'   is a string starting with "name:" or "annotation:" they are used for a 
#'   query by \code{\link{catmaid_query_by_name}} or
#'   \code{\link{catmaid_query_by_annotation}}, respectively.
#' @param x one or more skids or a query expression (see details)
#' @param several.ok Logical 
#' @return \code{integer} vector of skids (of length 0 on failure).
#' @export
catmaid_skids<-function(x, several.ok=TRUE) {
  if(is.factor(x)) {
    x=as.character(x)
  }
  skids=integer()
  if(is.numeric(x)) {
    skids= as.integer(x)
  } else if(length(x) > 1) {
    intx=as.integer(x)
    if(all(is.finite(intx))) {
      skids=intx
    } else stop("Multiple values provided but they do not look like skids!")
  } else {
    # just one value provided
    intx=suppressWarnings(as.integer(x))
    if(is.finite(intx)) {
      return(intx)
    } else if(substr(x,1,5)=="name:") {
      # query by name
      df=catmaid_query_by_name(substr(x, 6, nchar(x)), type = 'neuron')
      if(is.null(df)) warning("No matches for query ",x,"!")
      else skids = df$skid
    } else if(substr(x,1,11)=="annotation:") {
      # query by annotation
      df=catmaid_query_by_annotation(substr(x, 12, nchar(x)), type = 'neuron')
      if(is.null(df)) warning("No matches for query ",x,"!")
      else {
        # handle multiple returned data.frames
        if(!is.data.frame(df)) df=jsonlite::rbind.pages(df)
        skids = df$skid
      }
    } else {
      stop("Unrecognised skid specification!")
    }
  }
  if(!several.ok && length(skids)>1) 
    stop("Only expecting one skid but I have: ", length(x), "!")
  skids
}
