% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catmaid_metadata.R
\name{catmaid_get_neuronnames}
\alias{catmaid_get_neuronnames}
\title{Get names of neurons from CATMAID}
\usage{
catmaid_get_neuronnames(skids, pid = 1, conn = NULL, ...)
}
\arguments{
\item{skids}{One or more numeric skeleton ids or a character vector defining
a query (see \code{\link{catmaid_skids}} or examples for the syntax).}

\item{pid}{Project id (default 1)}

\item{conn}{A \code{catmaid_connection} objection returned by
\code{\link{catmaid_login}}. If \code{NULL} (the default) a new connection
object will be generated using the values of the \bold{catmaid.*} package
options as described in the help for \code{\link{catmaid_login}}.}

\item{...}{Additional arguments passed to the \code{\link{catmaid_fetch}}
function}
}
\value{
a character vector of neuron names, with a names attribute specifying
  the skeleton ids (skids). Missing values will be represented by a
  \code{NA_character} value.
}
\description{
Get names of neurons from CATMAID
}
\details{
Note that \code{catmaid_get_neuronnames} only queries the server for
  the unique set of input \code{skids} to save time.
}
\examples{
\dontrun{
catmaid_get_neuronnames(skids=c(10418394,4453485))
catmaid_get_neuronnames("name:ORN (left|right)")
catmaid_get_neuronnames("annotation:ORN PNs$")
}
}
\seealso{
\code{\link{catmaid_fetch}}, \code{\link{catmaid_skids}}
}
