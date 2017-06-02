### DO WE NEED TO REFIT MODELS SPECIALLY WITH HOLDOUT DATA OR CAN WE JUST STORE PARAMS
## AND USE predict.growmod?

#' @rdname growmod
#' @export
validate <- function(x, ...) {
  UseMethod('validate')
}

#' @rdname growmod
#' @export
validate.grow_mod <- function(x, ...) {
  # generate model file
  # pre-compile model to avoid compiling repeatedly
  mod_compiled <- stan_model(file = x$mod_file)
  
  # fetch data (should be OK to use data set from X)
  #   Need to change from plot to holdout (can we re-use?)
  # THis would be ideal.
  
  
  if ((cores > 1) & (Sys.info()['sysname'] == 'Darwin')) {
    mod <- mclapply(1:n.cv, stan_cv_internal,
                    mod_compiled,
                    data,
                    model,
                    n_cv,
                    n_iter,
                    n_chains,
                    inits,
                    seed,
                    control, 
                    spline_params,
                    stan_cores,
                    mc.cores = cores,
                    ...)
  } else {
    mod <- lapply(1:n.cv, stan_cv_internal,
                  mod_compiled,
                  data,
                  model,
                  n_cv,
                  n_iter,
                  n_chains,
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

stan_cv_internal <- function(i,
                             mod_compiled,
                             data,
                             model,
                             n_cv,
                             n_iter,
                             n_chains,
                             inits,
                             seed,
                             control,
                             spline_params,
                             stan_cores,
                             ...) {
  # subset data appropriately
  # we have train_data and mod_file
  
  # fit model just to training set
  
  # use predict.growmod(mod_train, data_test) to get predictions
}

