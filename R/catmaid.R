#' Programmatic access to the CATMAID API
#'
#' To use, first call \code{\link{catmaid_login}} to authenticate to a catmaid
#' server. You can then use functions such as
#' \code{\link{catmaid_get_neuronnames}} or
#' \code{\link{catmaid_get_compact_skeleton}} to query the server. You can also
#' roll your own server queries using the low-level \code{\link{catmaid_fetch}}
#' command.
#'
#' @section Connections: By default all query functions will use the
#'   server/authentication details specified by the last successful call to
#'   \code{\link{catmaid_login}} unless a \code{\link{catmaid_connection}}
#'   object is provided as an argument.
#'
#' @section Default Login: See \code{\link{catmaid_login}} for details of the
#'   environment variables that can be set to specify default login values. It
#'   makes sense to set these in your \code{\link{.Renviron}} file if you
#'   regularly use a specific CATMAID server. Note that you can have different
#'   \code{.Renviron} files in the home folder of different Rstudio projects if
#'   you regularly use more than one CATMAID server. Note that the use of 
#'   environment variables is now preferred over setting R options.
#'
#' @section Unit tests: Unit tests can be run as per the examples section below.
#'   In order to do live tests you will need to set environment variables with
#'   your catmaid login credentials using the function
#'   \code{\link{catmaid_connection_setenv}}.
#' @name catmaid-package
#' @aliases catmaid
#' @docType package
#' @keywords package
#' @import httr nat
#' @examples
#' \dontrun{
#' ## test package
#' library(catmaid)
#' library(testthat)
#' test_package("catmaid")
#'
#' ## same but use appropriate login info
#' conn=catmaid_login(server="https://myserver.com", user="calvin", password='hobbes')
#' ## alternatively, login using an API token
#' # see http://catmaid.github.io/dev/api.html#api-token for how to get one
#' conn=catmaid_login(server="https://myserver.com",
#'                    token = "9944b09199c62bcf9418ad846dd0e4bbdfc6ee4b")
#' catmaid_connection_setenv(conn)
#' test_package("catmaid")
#' }
#' @references The first version of this package was based in large part on
#'   python code at
#'   \url{https://github.com/schlegelp/CATMAID-to-Blender/blob/master/CATMAIDImport.py}.
#'    See \url{http://catmaid.org} for further details about CATMAID and
#'   \url{https://github.com/acardona/CATMAID/blob/master/django/applications/catmaid/urls.py}
#'    for a list of URLs that the web API accepts.
NULL

#' A sample CATMAID neuron (from the adult lateral horn of class AV4b1)
#'
#' @details This neuron was reconstructed by Philipp Schelegel and Greg Jefferis
#'   and is in FAFB14 space. and is from the
#' @references A Complete Electron Microscopy Volume of the Brain of Adult
#'   \emph{Drosophila melanogaster}.
#'   Zheng, Zhihao and Lauritzen, J. Scott and Perlman, Eric and Robinson,
#'   Camenzind G. and Nichols, Matthew and Milkie, Daniel and Torrens, Omar and
#'   Price, John and Fisher, Corey B. and Sharifi, Nadiya and Calle-Schuler,
#'   Steven A. and Kmecova, Lucia and Ali, Iqbal J. and Karsh, Bill and
#'   Trautman, Eric T. and Bogovic, John and Hanslovsky, Philipp and Jefferis,
#'   Gregory S. X. E. and Kazhdan, Michael and Khairy, Khaled and Saalfeld,
#'   Stephan and Fetter, Richard D. and Bock, Davi D. bioRxiv (2017).
#'   \href{https://doi.org/10.1101/140905}{doi:10.1101/140905}
#' @docType data
#' @name AV4b1
#' @usage data(AV4b1)
"AV4b1"
