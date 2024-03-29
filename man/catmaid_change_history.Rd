% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catmaid_skeleton.R
\name{catmaid_change_history}
\alias{catmaid_change_history}
\title{Return the edit history of a CATMAID skeleton}
\usage{
catmaid_change_history(skids, pid = 1, conn = NULL, ...)
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
a list of lists with the most recent ids last
}
\description{
Return the edit history of a CATMAID skeleton
}
\examples{
\dontrun{
catmaid_change_history(15886)
}
}
\seealso{
\href{https://github.com/catmaid/CATMAID/blob/ce1dee4485e76c3b33bc751d5440ce8477fb1f43/django/applications/catmaid/control/skeleton.py#L3982}{catmaid source code}
}
