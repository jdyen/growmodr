#' @rdname growmod
#' @export
validate <- function(x, ...) {
  UseMethod('validate')
}

#' @rdname growmod
#' @export
validate.grow_mod <- function(x, ...) {
  # generate model file
  
  # fetch data (should be OK to use data set from X)
  #   Need to change from plot to holdout (can we re-use?)
  
  # pre-compile model to avoid compiling repeatedly
  mod.def <- stan_model(file = mod.file)
  
  if ((cores > 1) & (Sys.info()['sysname'] == 'Darwin')) {
    mod <- mclapply(1:n.cv, stan_cv_internal,
                    mod.def,
                    data.set,
                    model,
                    n.cv,
                    n.iter,
                    n.chains,
                    inits,
                    seed,
                    control, 
                    spline_params,
                    stan_cores,
                    mc.cores = cores,
                    ...)
  } else {
    mod <- lapply(1:n.cv, stan_cv_internal,
                  mod.def,
                  data.set,
                  model,
                  n.cv,
                  n.iter,
                  n.chains,
                  inits,
                  seed,
                  control,
                  spline_params,
                  stan_cores,
                  ...)
  }
  out_full <- do.call('rbind', mod)
  r2_cv <- cor(out_full$size_pred, out_full$size_real) ** 2
  rmsd_cv <- sqrt(mean((out_full$size_pred - out_full$size_real) ** 2))
  md_cv <- mean((out_full$size_pred - out_full$size_real))
  mod_cv <- list(r2 = r2_cv,
                 rmsd = rmsd_cv,
                 md = md_cv)
  class(mod_cv) <- 'growmod_cv'
  mod_cv
}

#' @rdname growmod
#' @export
validate.grow_mod_multi <- function(x, ...) {
  
  mod_cv <- x
  class(mod_cv) <- 'growmod_cv_multi'
  mod_cv
}

