#' A connection to the vfb catmaid server for a given dataset
#'
#' @section Datasets: The following three datasets are currently available
#'   \itemize{
#'
#'   \item \code{fafb} The full adult female brain \emph{Drosophila} dataset
#'
#'   \item \code{l1em} The L1 first instar larval \emph{Drosophila} dataset
#'
#'   \item \code{fanc} The Full Adult female Nerve Cord \emph{Drosophila}
#'   dataset }
#'
#' @param dataset Character vector specifying dataset. See datasets section..
#' @param ... Additional arguments passed to \code{\link{catmaid_login}}
#'
#' @export
#' @seealso \code{\link{catmaid_login}},
#'   \url{https://catmaid.virtualflybrain.org/}
#' @examples
#' \donttest{
#' vfbcatmaid("fafb")
#' }
#' \dontrun{
#' vfbcatmaid("l1em")
#' vfbcatmaid("fanc")
#' }
vfbcatmaid <- function(dataset=c("fafb", "l1em", "fanc"), ...) {
  dataset=match.arg(dataset)
  baseu=".catmaid.virtualflybrain.org"
  url=paste0("https://", dataset, baseu)
  catmaid_login(server=url, ...)
}

