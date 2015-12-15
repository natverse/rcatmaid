

#' Get contributor statistics for neurons from CATMAID
#' 
#' @inheritParams read.neuron.catmaid
#' @return a list containing different statistics including construction and 
#'   review times (aggregated across all the specified input neurons).
#' @export
#' @examples
#' \dontrun{
#' catmaid_get_contributor_stats(skids=c(10418394,4453485))
#' }
#' @seealso \code{\link{catmaid_get_review_status}}
catmaid_get_contributor_stats<-function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn)
  post_data=list()
  post_data[sprintf("skids[%d]", seq_along(skids)-1)]=as.list(skids)
  path=sprintf("/%d/skeleton/contributor_statistics_multiple", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, ...)
  res
}


# Neuron name:	42a PN left
# Node count:	2536
# Nodes contributed by:	
#   matt	1606
# larderet	456
# ingrid	256
# avinash	117
# claus	98
# lif	2
# larisa	1
# Number of presynaptic sites:	96
# Presynapses contributed by:	
#   claus	36
# matt	27
# larderet	14
# ingrid	11
# kathi	3
# avinash	2
# javier	2
# andreas	1
# Number of postsynaptic sites:	258
# Postsynapses contributed by:	
#   matt	153
# avinash	71
# claus	27
# ingrid	3
# larderet	2
# javier	1
# kathi	1
# Construction time:	3 hours 14 minutes
# Minimal review time (min):	52 minutes
# Multiuser review time (min):	2 hours 25 minutes
