% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catmaid_skeleton.R
\name{catmaid_get_treenodes_detail}
\alias{catmaid_get_treenodes_detail}
\title{Fetch position and other information for arbitrary treenode ids}
\usage{
catmaid_get_treenodes_detail(
  tnids = NULL,
  labels = NULL,
  skids = NULL,
  pid = 1,
  conn = NULL,
  raw = FALSE,
  ...
)
}
\arguments{
\item{tnids}{One or more (integer) treenode ids}

\item{labels}{One or more (character) labels with which nodes must be tagged}

\item{skids}{One or more skeleton ids or an expression compatible with
\code{\link{catmaid_skids}} (see Details for advice re many skids)}

\item{pid}{project id (default 1)}

\item{conn}{the \code{\link{catmaid_connection}} object}

\item{raw}{Whether to return completely unprocessed data (when \code{TRUE}) 
or to convert the nodes and connectors lists into processed data.frames 
(when \code{FALSE}, the default)}

\item{...}{Additional arguments passed to the \code{\link{catmaid_fetch}} 
function.}
}
\value{
\itemize{

  \item treenode_id,

  \item parent_id,

  \item x,

  \item y,

  \item z,

  \item confidence,

  \item radius,

  \item skid,

  \item edition_time,

  \item user_id

  }
}
\description{
Fetch position and other information for arbitrary treenode ids
}
\details{
The key feature of this function is that allows you to fetch
  information for arbitrary tree nodes that do not need to be from the same
  skeleton. Furthermore the nodes can be defined by the presence of labels
  (tags) or by a skeleton id. \code{labels} and \code{skids} specifiers can
  be combined in order e.g. to find details for the somata for a given set of
  skeleton ids. However these queries are slow for more than a few hundred
  skids, at which point it is better to fetch using the label and then filter
  by skid post hoc in R.
}
\examples{
\donttest{
# details for 3 nodes from two different skeletons
catmaid_get_treenodes_detail(c(9943214L, 25069047L, 12829015L))

# example label search
tosoma=catmaid_get_treenodes_detail(labels="to soma")
}

\dontrun{
# If you have a lot of skids to query you will need to break up your queries
# into smaller chunks. You could do this like so:
catmaid_get_treenodes_detail_chunked <- function(skids, chunksize=300, chunkstoread=NULL, ...) {
  nchunks=ceiling(length(skids)/chunksize)
  chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_along(skids)]
  
  l=list()
  if(is.null(chunkstoread)) chunkstoread=seq_len(nchunks)
  pb <- progress::progress_bar$new(total = length(skids),
                                   format = "  :current/:total [:bar]  eta: :eta",
                                   show_after=1)
  
  for(i in chunkstoread) {
    pb$tick(len = sum(chunks==i))
    l[[length(l)+1]]=catmaid_get_treenodes_detail(skids=skids[chunks==i], ...)
  }
  dplyr::bind_rows(l)
}

}
}
\seealso{
\code{\link{catmaid_get_treenode_table}},
  \code{\link{catmaid_get_connectors}},
  \code{\link{catmaid_get_compact_skeleton}}
}
