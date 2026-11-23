# zzz.R -------------------------------------------------------------------
# Package hooks. Keep startup quiet: the only default is the cache switch.

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("nflrest.cache"))) {
    options(nflrest.cache = TRUE)
  }
}
