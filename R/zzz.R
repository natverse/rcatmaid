# will store information about e.g. any connections that we have made
.package_statevars <- new.env()

.onLoad <- function(libname, pkgname) {
  .package_statevars$msgpack_available <- requireNamespace('RcppMsgPack', quietly = TRUE)
  
  invisible()
}
