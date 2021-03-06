% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catmaid_metadata.R
\name{catmaid_query_by_name}
\alias{catmaid_query_by_name}
\alias{catmaid_query_by_annotation}
\title{Get list of neurons/annotations querying by neuron/annotation name}
\usage{
catmaid_query_by_name(
  query,
  pid = 1,
  maxresults = 500,
  type = c("neuron", "annotation"),
  raw = FALSE,
  ...
)

catmaid_query_by_annotation(
  query,
  pid = 1,
  maxresults = 500,
  type = c("neuron", "annotation"),
  raw = FALSE,
  conn = NULL,
  ...
)
}
\arguments{
\item{query}{A query string (NB this is a regular expression)}

\item{pid}{project id (default 1)}

\item{maxresults}{The maximum number of results to return}

\item{type}{Type of results to return. Defaults to \code{c("neuron",
"annotation")}. Only relevant when \code{raw=FALSE}.}

\item{raw}{Whether to return completely unprocessed data (when \code{TRUE}) 
or to convert the nodes and connectors lists into processed data.frames 
(when \code{FALSE}, the default)}

\item{...}{Additional arguments passed to the \code{\link{catmaid_fetch}} 
function.}

\item{conn}{the \code{\link{catmaid_connection}} object}
}
\value{
For \code{catmaid_query_by_name} a data.frame containing the results
  with an attribute "annotations" containing the annotations as a separate
  data.frame. For both functions the main data.frame has the following
  columns \itemize{

  \item id

  \item name

  \item type (neuron or annotation)

  \item skid (the main identifier for the neuron skeleton, catmaid often
  calls this skeleton_id)

  }

For \code{catmaid_query_by_annotation} a data.frame containing the 
  results.
}
\description{
These functions try to match a \code{query} against the
  \code{name} of a neuron or annotation (\code{catmaid_query_by_name})
  \emph{or} against an annotation (tag) that has been applied to a neuron or
  annotation (\code{catmaid_query_by_annotation}). Note that 1) objects can
  be tagged with multiple annotations and 2) annotation tags are recursive so
  one annotation can be tagged with a second annotation.

  The most common use will be to get a data.frames of neurons when you should
  use \code{type="neuron"}. You can also return both neurons and annotations
  (the default) and just annotations. See \code{type} argument for details.

  Note that \code{catmaid_query_by_name} prepends a \code{"/"} to the
  \code{query} argument so that it is treated as a regular expression (which
  by some slight quirk of the API would otherwise not be the case).
}
\examples{
\dontrun{
catmaid_query_by_name("ORN")
# using regex functionality
catmaid_query_by_name("ORN (left|right)")
# fancier regex
catmaid_query_by_name("^[0-9a-f &]+ PN (left|right)")
}
\dontrun{
# query matches 3 specific annotations
catmaid_query_by_annotation("ORN PNs")

# what are those 3 annotations?
al=catmaid_get_annotationlist()
subset(al$annotations, grepl("ORN PNs", name))

# Insist on specific annotation by using regex start/finish symbols
catmaid_query_by_annotation("^ORN PNs$")
}
}
\seealso{
\code{\link{catmaid_get_annotationlist}}
}
