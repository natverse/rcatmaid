% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catmaid_metadata.R
\name{catmaid_get_annotationlist}
\alias{catmaid_get_annotationlist}
\title{Get list of annotations (including user information) from CATMAID}
\usage{
catmaid_get_annotationlist(pid = 1, conn = NULL, raw = FALSE, ...)
}
\arguments{
\item{pid}{project id (default 1)}

\item{conn}{the \code{\link{catmaid_connection}} object}

\item{raw}{Whether to return completely unprocessed data (when \code{TRUE}) 
or to convert the nodes and connectors lists into processed data.frames 
(when \code{FALSE}, the default)}

\item{...}{Additional arguments passed to the \code{\link{catmaid_fetch}} 
function.}
}
\value{
A list containing two data.frames, annotations and users. The users
  data.frame describes the users associated with each annotations, with one row
  for each valid annotations/user pair.
}
\description{
Get list of annotations (including user information) from CATMAID
}
\examples{
\dontrun{
al=catmaid_get_annotationlist(pid=1)
# table of the number of users who have contributed to each annotation
table(al$annotations$num_users_annotation)
}
}
