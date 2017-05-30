# Fitting growth models using growmod functions

#' @name growmod
#' @title Fitting growth models using growmod
#' @description Fit a growth model to data on sizes through time. Currently
#'   uses Stan to fit models for one of twelve common growth curves.
NULL

#' @rdname growmod
#' @export
#' @import rstan
#' 
#' @param x list containing data on size, age, species, and traits
#' @param formula list containing data on size, age, species, and traits
#' @param size list containing data on size, age, species, and traits
#' @param index list containing data on size, age, species, and traits
#' @param block list containing data on size, age, species, and traits
#' @param predictors list containing data on size, age, species, and traits
#' @param data list containing data on size, age, species, and traits
#' @param model growth model form to be fitted
#' @param n_iter number of HMC iterations to run for stan model
#' @param n_burnin number of HMC iterations to run for stan model
#' @param n_thin thinning rate for HMC chains
#' @param n_chains number of HMC chains
#' @param spline_params named list of settings for spline model (degree, n_knots, spline_type)
#' @param \dots parameters to be passed to stan model call
#' 
#' @details \code{growmod} takes a formula or data vectors as arguments
#'   and constructs and fits a Stan model for a chosen growth curve.
#' 
#' @return \code{mod} A fitted \code{grow_mod} object containing
#'   parameter estimates, validation statistics, and the original
#'   data set used to fit the model. 
#'
#' @examples
#' \dontrun {
#'   # simulate some data
#'   data_sim <- growth_data_sim(n = 100,
#'                               nblock = 5,
#'                               age_range = c(0, 1),
#'                               include_predictors = TRUE,
#'                               true_model = 'hillslope')
#'
#'   # fit the correct model
#'   mod1 <- growmod(size ~ (age | block),
#'                   predictors = predictors,
#'                   data = data_sim,
#'                   model = 'hillslope',
#'                   n_iter = 1000,
#'                   n_burnin = 500,
#'                   n_chains = 2,
#'                   stan_cores = 1)
#'
#'   # plot the fitted model
#'   plot(mod1)
#'   
#'   # summarise the fitted model
#'   summary(mod1)
#'   
#'   # print the fitted model
#'   print(mod1)
#'
#'   # cross validate the fitted model
#'   mod1_cv <- validate(mod1)
#'                    
#'   # fit an incorrect model
#'   mod2 <- growmod(size ~ (age | block),
#'                   predictors = predictors,
#'                   data = data_sim,
#'                   model = 'koblog',
#'                   n_iter = 1000,
#'                   n_burnin = 500,
#'                   n_chains = 2,
#'                   stan_cores = 1)
#'
#'   # compare the fitted models using summary measures of fit
#'   compare(mod1, mod2)
#'   
#'   # compare the fitted models using cross validation
#'   mod2_cv <- validate(mod2)
#'   compare(mod1_cv, mod2_cv)
#'   
#'   # example of different predictor sets for each parameter
#'   # TO BE ADDED
#'   
#'   # example of multiple models fitted in one call
#'   # TO BE ADDED
#'   
#'   # example of multiple models fitted in one call with different
#'   #  predictors for each model and parameter
#'   # TO BE ADDED
#'   
#' }
#' 
growmod <- function(x, ...) {
  UseMethod('growmod')
}

#' @rdname growmod
#' @export
#' @import rstan
growmod.formula <- function(formula,
                            predictors = NULL,
                            data = NULL,
                            model = 'hillslope',
                            n_iter = 5000,
                            n_burnin = floor(n_iter / 2),
                            n_thin = 1,
                            n_chains = 4,
                            stan_cores = 1,
                            spline_params = list(degree = 8,
                                                 n_knots = 10,
                                                 spline_type = 'ispline'),
                            ...) {
  # collate data from formula
  form_tmp <- formula
  pred_vars <- all.vars(form_tmp[[length(form_tmp)]])
  index_var <- pred_vars[1]
  size_resp <- all.vars(form_tmp)[1]
  
  # check for size data in data and workspace
  if (!is.null(data)) {
    if (exists(size_resp, data)) {
      size_data <- get(size_resp, data)
    }
  } else {
    if (exists(size_resp, parent.frame())) {
      size_data <- get(size_resp, parent.frame())
    } else {
      stop(paste0(size_resp, ' not found'),
           call. = FALSE)
    }
  }
  
  # check for index variable in data and workspace
  if (!is.null(data)) {
    if (exists(index_var, data)) {
      index_data <- get(index_var, data)
    }
  } else {
    if (exists(index_var, parent.frame())) {
      index_data <- get(index_var, parent.frame())
    } else {
      stop(paste0(index_var, ' not found'),
           call. = FALSE)
    }
  }

  # check if a blocking variable has been provided
  if (length(pred_vars) == 1) {
    cat('model has no blocking variable and will assume that all data points are
         samples from a single growth curve.')
    block_var <- NULL
    block_data <- NULL
  } else {
    block_var <- pred_vars[2]
    if (!is.null(data)) {
      if (exists(block_var, data)) {
        block_data <- get(block_var, data)
      }
    } else {
      if (exists(block_var, parent.frame())) {
        block_data <- get(block_var, parent.frame())
      } else {
        stop(paste0(block_var, ' not found'),
             call. = FALSE)
      }
    }
  }
  n <- length(size_data)
  if (!is.null(block_data)) {
    nblock <- length(unique(block_data))
  }
  
  # check all data for errors
  if (length(index_data) != n) {
    stop(paste0(index_var, ' should be the same length as ', size_resp, '.'),
         call. = FALSE)
  }
  if (!is.null(block_var)) {
    if (length(block_data) != n) {
      stop(paste0(block_var, ' should be the same length as ', size_resp, '.'),
           call. = FALSE)
    }
  }
  
  # fit model
  mod <- growmod.default(size = size_data,
                         index = index_data,
                         block = block_data,
                         predictors = predictors,
                         model = model,
                         n_iter = n_iter,
                         n_burnin = n_burnin,
                         n_thin = n_thin,
                         n_chains = n_chains,
                         stan_cores = stan_cores,
                         spline_params = spline_params,
                         ...)
  
  # return outputs
  mod
}

#' @rdname growmod
#' @export
#' @import rstan
growmod.default <- function(size,
                            index,
                            block,
                            predictors,
                            model,
                            n_iter,
                            n_burnin,
                            n_thin,
                            n_chains,
                            stan_cores,
                            spline_params,
                            ...) {
  # basic checks
  n <- length(size)
  if (!is.null(block)) {
    nblock <- length(unique(block))
  }
  
  # check data for errors
  if (length(index) != n) {
    stop('index should be the same length as size.',
         call. = FALSE)
  }
  if (!is.null(block)) {
    if (length(block) != n) {
      stop('block should be the same length as size.',
           call. = FALSE)
    }
  }
  
  # setup model parameters and extract predictors
  if (length(model) == 1) {
    if (model != 'spline') {
      mod_params <- get(paste0(model, '_param_fetch'))()
      num_params <- mod_params$num_par
    } else {
      num_params <- spline_params$n_knots + spline_params$degree
    }
    pred_set <- check_preds(predictors = predictors,
                            model = model,
                            block_data = block,
                            num_params = num_params,
                            n = n,
                            nblock = nblock)
  } else {
    mod_params <- vector('list', length = length(model))
    for (i in seq(along = model)) {
      if (model != 'spline') {
        mod_params[[i]] <- get(paste0(model[i], '_param_fetch'))()
      } else {
        mod_params[[i]] <- list(num_par = (spline_params$n_knots + spline_params$degree))
      }
    }
    num_params <- sapply(mod_params, function(x) x$num_par)
    pred_set <- vector('list', length = length(model))
    for (i in seq(along = model)) {
      if (!is.null(predictors)) {
        if (is.matrix(predictors) | is.data.frame(predictors)) {
          predictors_tmp <- predictors
        } else {
          if (is.list(predictors)) {
            if (length(predictors) == length(model)) {
              predictors_tmp <- predictors[[i]]
            } else {
              predictors_tmp <- predictors
            }
          } else {
            stop('predictors must be a matrix, data.frame or list.',
                 call. = FALSE)
          }
        }
      } else {
        predictors_tmp <- NULL
      }
      pred_set[[i]] <- check_preds(predictors = predictors_tmp,
                                   model = model[i],
                                   block_data = block,
                                   num_params = num_params[i],
                                   n = n,
                                   nblock = nblock)
    }
  }
  
  # setup data set for stan model
  if (length(model) == 1) {
    data_set <- growmod_data(data_set = list(size = size,
                                             index = index,
                                             block = block,
                                             predictors = pred_set),
                             model = model,
                             num_params = num_params,
                             spline_params = spline_params,
                             n_plot = 100)
  } else {
    data_set <- vector('list', length = length(model))
    for (i in seq(along = model)) {
      data_set[[i]] <- growmod_data(data_set = list(size = size,
                                                    index = index,
                                                    block = block,
                                                    predictors = pred_set[[i]]),
                                    model = model[i],
                                    num_params = num_params[i],
                                    spline_params = spline_params,
                                    n_plot = 100)
    }
  }

  # set some defaults for basic Stan settings
  if (!hasArg(control)) {
    control <- list(adapt_delta = 0.99)
  }
  if (!hasArg(inits)) {
    inits <- '0'
  }
  
  # fit models; loop over multiple models if required
  if (length(model) == 1) {
    # generate stan model
    mod_file <- gen_mod_file(model = model,
                             spline_params = spline_params,
                             mod_file = NULL,
                             include_pred = !is.null(predictors),
                             include_block = !is.null(block))
    
    # fit model
    stan_mod <- stan(file = mod_file,
                data = data_set,
                chains = n_chains,
                iter = n_iter,
                warmup = n_burnin,
                thin = n_thin,
                cores = stan_cores,
                ...)
    
    # summarise fitted stan model
    log_lik_tmp <- extract_log_lik(stan_mod)
    loo <- loo(log_lik_tmp)
    waic <- waic(log_lik_tmp)
    fitted_vals_tmp <- exp(get_posterior_mean(stan_mod, pars = 'mu'))
    fitted_vals <- fitted_vals_tmp[, ncol(fitted_vals_tmp)]
    r2 <- round(cor(fitted_vals, data_set$size_data) ** 2, 3)
    rmsd <- round(sqrt(mean((fitted_vals - data_set$size_data) ** 2)), 3)
    md <- round(mean((fitted_vals - data_set$size_data)), 3)

    # put outputs into a named list
    ## THINK ABOUT THIS FOR VALIDATE METHOD
    ##  DOESN'T REALLY NEED FULL STAN MODEL,
    ##    JUST data_set AND compiled stan_mod
    mod <- list(fitted = fitted_vals,
                r2 = r2,
                rmsd = rmsd,
                md = md,
                loo = loo,
                waic = waic,
                stan_mod = stan_mod)
    
    # set model class for single growth curve model
    class(mod) <- 'grow_mod'
  } else {
    mod <- vector('list', length = length(model))
    
    for (i in seq(along = model)) {
      # generate stan model
      mod_file <- gen_mod_file(model = model[i],
                               spline_params = spline_params,
                               mod_file = NULL,
                               include_pred = !is.null(predictors),
                               include_block = !is.null(block))
      
      # fit model
      stan_mod <- fit_stan_model(size = size_data,
                                 index = index_data,
                                 block = block_data,
                                 predictors = pred_set[[i]],
                                 model = model[i],
                                 n_iter = n_iter,
                                 n_burnin = n_burnin,
                                 n_thin = n_thin,
                                 n_chains = n_chains,
                                 stan_cores = stan_cores,
                                 spline_params = spline_params,
                                 ...)
      
      # summarise fitted tsan model
      mod[[i]] <- summary(stan_mod)
    }
    
    # set model class for multiple growth curve models
    class(mod) <- 'grow_mod_multi'
  }
  
  # return outputs
  mod
}

#' @rdname growmod
#' @export
compare <- function(x, ...) {
  UseMethod('compare')
}

#' @rdname growmod
#' @export
compare.grow_mod <- function(..., x) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(x)) {
      stop("If 'x' is specified then '...' should not be specified.")
    }
    nms <- as.character(match.call(expand.dots = TRUE))[-1L]
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list.")
    }
    dots <- x
    nms <- names(dots)
    if (!length(nms)) {
      nms <- paste0('model', seq_along(dots))
    }
  }
}

#' @rdname growmod
#' @export
compare.grow_mod_cv <- function(..., x) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(x)) {
      stop("If 'x' is specified then '...' should not be specified.")
    }
    nms <- as.character(match.call(expand.dots = TRUE))[-1L]
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list.")
    }
    dots <- x
    nms <- names(dots)
    if (!length(nms)) {
      nms <- paste0('model', seq_along(dots))
    }
  }
}

#' @rdname growmod
#' @export
compare.grow_mod_multi <- function(..., x) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(x)) {
      stop("If 'x' is specified then '...' should not be specified.")
    }
    nms <- as.character(match.call(expand.dots = TRUE))[-1L]
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list.")
    }
    dots <- x
    nms <- names(dots)
    if (!length(nms)) {
      nms <- paste0('model', seq_along(dots))
    }
  }
}

#' @rdname growmod
#' @export
compare.grow_mod_cv_multi <- function(..., x) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(x)) {
      stop("If 'x' is specified then '...' should not be specified.")
    }
    nms <- as.character(match.call(expand.dots = TRUE))[-1L]
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list.")
    }
    dots <- x
    nms <- names(dots)
    if (!length(nms)) {
      nms <- paste0('model', seq_along(dots))
    }
  }
}

#' @rdname growmod
#' @export
validate <- function(x, ...) {
  UseMethod('validate')
}

#' @rdname growmod
#' @export
validate.grow_mod <- function(x, ...) {
  
  mod_cv <- x
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

#' @rdname growmod
#' @export
plot.grow_mod <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
plot.grow_mod_cv <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
plot.grow_mod_multi <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
plot.grow_mod_cv_multi <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
summary.grow_mod <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
summary.grow_mod_cv <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
summary.grow_mod_multi <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
summary.grow_mod_cv_multi <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
print.grow_mod <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
print.grow_mod_cv <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
print.grow_mod_multi <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
print.grow_mod_cv_multi <- function(x, ...) {
  
}