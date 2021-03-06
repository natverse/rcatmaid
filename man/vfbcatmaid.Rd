% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vfb.R
\name{vfbcatmaid}
\alias{vfbcatmaid}
\title{A connection to the vfb catmaid server for a given dataset}
\usage{
vfbcatmaid(dataset = c("fafb", "l1em", "fanc"), ...)
}
\arguments{
\item{dataset}{Character vector specifying dataset. See datasets section..}

\item{...}{Additional arguments passed to \code{\link{catmaid_login}}}
}
\description{
A connection to the vfb catmaid server for a given dataset
}
\section{Datasets}{
 The following three datasets are currently available
  \itemize{

  \item \code{fafb} The full adult female brain \emph{Drosophila} dataset

  \item \code{l1em} The L1 first instar larval \emph{Drosophila} dataset

  \item \code{fanc} The Full Adult female Nerve Cord \emph{Drosophila}
  dataset }
}

\examples{
\donttest{
vfbcatmaid("fafb")
}
\dontrun{
vfbcatmaid("l1em")
vfbcatmaid("fanc")
}
}
\seealso{
\code{\link{catmaid_login}},
  \url{https://catmaid.virtualflybrain.org/}
}
