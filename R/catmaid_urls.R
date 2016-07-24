#' Parse a CATMAID URL to extract parameters including xyz location
#' 
#' @param x One or more catmaid urls
#'   
#' @return A \code{data.frame} containing parsed parameters. The columns will be
#'   named according to CATMAID's convention except for the x, y and z location
#'   (named as such rather than xp, yp, zp as returned by CATMAID).
#' @export
#' 
#' @examples
#' u="https://neuropil.janelia.org/tracing/fafb/v12/?pid=1&zp=177100&yp=166068&xp=373086&tool=tracingtool&sid0=7&s0=1.000000"
#' catmaid_parse_url(u)
catmaid_parse_url <- function(x) {
  if(length(x)>1) {
    l=lapply(x, catmaid_parse_url)
    return(do.call(rbind, l))
  }
  df=data.frame(baseurl=dirname(x), stringsAsFactors = F)
  params=basename(x)
  # trim off first character
  params=substr(params, 2, nchar(params))
  paramc=strsplit(params, "&", fixed = T)[[1]]
  
  paraml=lapply(paramc, strsplit, split="=", fixed=T)
  xx=sapply(paraml, "[[", 1)
  xx2=apply(xx,1,paste, collapse=' ')
  df=cbind(df, read.table(text = xx2, header = T))
  xyzcols=c("xp","yp", "zp")
  names(df)[match(xyzcols, names(df))]=c("x","y", "z")
  df
}
