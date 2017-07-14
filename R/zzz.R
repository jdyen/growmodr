.onLoad <- function(libname, pkgname) {
  modules <- paste0('stan_fit4', names(growmod:::stanmodels), '_mod')
  for (m in modules) loadModule(m, what = TRUE)
}

