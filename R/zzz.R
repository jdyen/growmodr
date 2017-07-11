.onLoad <- function(libname, pkgname) {
  modules <- paste0('growmod', names(stanmodels), '_mod')
  for (m in modules) loadModule(m, what = TRUE)
}

.onAttach <- function(...) {
  growmodLib <- dirname(system.file(package = 'growmod'))
  pkgdesc <- suppressWarnings(utils::packageDescription('growmod', lib.loc = growmodLib))
  if (length(pkgdesc) > 1) {
    builddate <- gsub(';.*$', '', pkgdesc$Packaged)
    packageStartupMessage(paste('growmod (Version ', pkgdesc$Version, ', packaged: ', builddate, ')', sep ''))
  }
}

