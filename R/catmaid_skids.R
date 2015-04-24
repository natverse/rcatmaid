#' Find skeleton ids for various inputs including text query
catmaid_skids<-function(x, several.ok=TRUE) {
  if(is.numeric(x)) return(as.integer(x))
  
  if(is.factor(x)) {
    x=as.character(x)
  }
  skids=NULL
  if(length(x) > 1) {
    if(!several.ok)
      stop("Only expecting one skid but I have: ", length(x), "!")
    
    intx=as.integer(x)
    if(all(is.finite(intx))) {
      skids=intx
    } else stop("multiple values provided but they do not look like skids!")
  } else {
    # just one value provided
    intx=as.integer(x)
    if(is.finite(intx)) {
      return(intx)
    } else if(nchar(x,1,5)=="name:") {
      # query by name
      df=catmaid_query_by_name(substr(x, 6, nchar(x)), type = 'neuron')
      if(!is.null(df)) skids = df$skid
    } else if(nchar(x,1,11)=="annotation:") {
      df=catmaid_query_by_annotation(substr(x, 12, nchar(x)), type = 'neuron')
      if(!is.null(df)) skids = df$skid
    } else {
      stop("unrecognised skid specification!")
    }
  }
  if(!several.ok && length(skids)>1) 
    stop("Only expecting one skid but I have: ", length(x), "!")
  skids
}
