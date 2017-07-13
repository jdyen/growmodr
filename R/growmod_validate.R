#' @name growmod-validate
#' @title Validate growth models fitted with growmod
#' @description Validate a fitted growth model using cross validation
#'     or by specifying your own test data set
#' @export
#' @import rstan
#' @import loo
#' @import stats
#' @useDynLib growmod, .registration = TRUE
#' 
#' @param formula list containing data on size, age, species, and traits
#' @param x fitted growmod or growmod_multi object
#' @param data list containing data on size, age, species, and traits
#' @param model growth model form to be fitted
#' @param n_cv number of cross validation folds
#' @param test_data (optional) holdout data set for model validation; only used if \code{is.null(n_cv)}
#' @param n_iter number of HMC iterations to run for stan model
#' @param n_burnin number of HMC iterations to run for stan model
#' @param n_thin thinning rate for HMC chains
#' @param n_chains number of HMC chains
#' @param stan_cores number of local cores to use in stan model fitting
#' @param spline_params named list of settings for spline model (degree, n_knots, spline_type)
#' @param \dots parameters to be passed to stan model call
#' 
#' @details \code{validate} takes a formula or fitted growmod object as arguments
#'   and validates the fitted Stan model for a chosen growth curve. The growth
#'   model is refitted from scratch if not already defined.
#' 
#' @return \code{mod_cv} A fitted \code{growmod_cv} object containing
#'   parameter estimates, validation statistics, and the original
#'   data set used to fit the model. 
#'   \item{size_real}{observed size data}
#'   \item{size_pred}{predicted size data under cross validation or holdout tests}
#'   \item{r2}{r-squared based on Pearson's r between observed and predicted size data}
#'   \item{rmsd}{root-mean-squared deviance of the validated model}
#'   \item{md}{mean deviance of the validated model}
#'   \item{model}{growth model used in model fitting}
#'   \item{val_type}{type of validation used: cross validation or holdout data set}
#'
#' @examples
#' \dontrun{
#'   # simulate some data
#'   data_sim <- growmod_sim(n = 100,
#'                           nblock = 5,
#'                           age_range = c(0, 50),
#'                           include_predictors = TRUE,
#'                           true_model = 'hillslope')
#'
#'   # fit the correct model
#'   mod <- growmod(size ~ (index | block / predictors),
#'                  data = data_sim,
#'                  model = 'hillslope',
#'                  n_iter = 1000,
#'                  n_burnin = 500,
#'                  n_chains = 2,
#'                  stan_cores = 1)
#'   
#'   # validate fitted model
#'   mod_val <- validate(mod, n_cv = 'loo')
#'   
#' }
#' 
validate <- function(x, ...) {
  UseMethod('validate')
}

#' @rdname growmod-validate
#' @export
validate.formula <- function(formula,
                             data = NULL,
                             model = 'hillslope',
                             n_cv = NULL,
                             test_data = NULL,
                             n_iter = 5000,
                             n_burnin = floor(n_iter / 2),
                             n_thin = 1,
                             n_chains = 4,
                             stan_cores = 1,
                             spline_params = list(degree = 8,
                                                  n_knots = 10,
                                                  spline_type = 'ispline'),
                             ...) {
  mod <- growmod(formula,
                 data = data,
                 model = model,
                 n_iter = n_iter,
                 n_burnin = n_burnin,
                 n_thin = n_thin,
                 n_chains = n_chains,
                 stan_cores = stan_cores,
                 spline_params = spline_params,
                 ...)
  if (!is.null(n_cv)) {
    mod_cv <- validate(mod, n_cv = n_cv)
  } else {
    if (!is.null(test_data)) {
      mod_cv <- validate(mod, test_data = test_data)
    } else {
      stop('n_cv or test_data must be provided for model validation',
           call. = FALSE)
    }
  }
  class(mod_cv) <- 'growmod_cv'
  mod_cv
}

#' @rdname growmod-validate
#' @export
validate.growmod <- function(x,
                             n_cv = NULL,
                             test_data = NULL,
                             n_iter = NULL,
                             n_burnin = NULL,
                             n_thin = NULL,
                             n_chains = NULL,
                             ...) {
  # generate model file
  if (!is.null(x$stanmod)) {
    mod_compiled <- x$stanmod
  } else {
    if (x$model != 'logistic3') {
      model_tmp <- x$model
    } else {
      model_tmp <- 'threeparl'
    }
    mod_name <- paste(model_tmp,
                      ifelse(is.null(predictors), 'nopred', 'pred'),
                      ifelse(is.null(block), 'onemod', 'blockmod'),
                      sep = '_')
    mod_compiled <- get(mod_name, growmod:::stanmodels)
  }
  
  # set sampling details
  if (is.null(n_iter)) {
    n_iter <- x$n_iter
  }
  if (is.null(n_burnin)) {
    n_burnin <- x$n_burnin
  }
  if (is.null(n_thin)) {
    n_thin <- x$n_thin
  }
  if (is.null(n_chains)) {
    n_chains <- x$n_chains
  }
  
  # check for type of CV
  if (is.null(n_cv)) {
    if (is.null(test_data)) {
      stop('Either the number of folds (n_cv) or test_data must be provided
           for model validation', call. = FALSE)
    } else {
      cat('Performing model validation based on', deparse(substitute(test_data)),'\n')
    }
  } else {
    if (n_cv == 'loo') {
      if (length(x$data_set$block_data)) {
        n_cv <- x$data_set$n_block
        n_cv <- ifelse(n_cv > 1, n_cv, 2)
      } else {
        n_cv <- x$data_set$n
      }
    } else {
      if (is.numeric(n_cv)) {
        n_cv <- ifelse(n_cv > x$data_set$n, n, n_cv)
        cat(paste0('Performing ', n_cv, '-fold cross validation\n'))
      } else {
        stop('n_cv must specify a number of folds or be set to "loo" for model validation',
             call. = FALSE)
      }
    }
  }
  
  # run cv in loop
  if (!is.null(test_data)) {
    all_vars <- all.vars(x$formula[[length(x$formula)]])
    index_var <- all_vars[1]
    size_resp <- all.vars(x$formula)[1]

    # check for size data in test_data
    if (exists(size_resp, test_data)) {
      size_data <- get(size_resp, test_data)
    } else {
      stop(paste0(size_resp, ' not found in', deparse(substitute(test_data))),
           call. = FALSE)
    }

    # check for index variable in test_data
    if (exists(index_var, test_data)) {
      index_data <- get(index_var, test_data)
    } else {
      stop(paste0(index_var, ' not found in', deparse(substitute(test_data))),
           call. = FALSE)
    }

    # check for predictors
    if (length(all_vars) == 3) {
      pred_var <- all_vars[3]
      if (exists(pred_var, test_data)) {
        predictors <- get(pred_var, test_data)
      } else {
        stop(paste0(pred_var, ' not found in', deparse(substitute(test_data))),
             call. = FALSE)
      }
    } else {
      predictors <- NULL
    }
    
    # check if a blocking variable has been provided
    if (length(all_vars) == 1) {
      block_var <- NULL
      block_data <- NULL
    } else {
      block_var <- all_vars[2]
      if (exists(block_var, test_data)) {
        block_data <- get(block_var, test_data)
      } else {
        stop(paste0(block_var, ' not found in', deparse(substitute(test_data))),
             call. = FALSE)
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
    
    # predict to test data
    train_data <- list(data_set = x$data_set,
                       predictors = x$predictors)
    test_data <- list(size = size_data,
                      index = index_data,
                      block = block_data,
                      predictors = predictors)
    pred_test <- predict_internal(mod_compiled = mod_compiled,
                                  train_data = train_data,
                                  test_data = test_data,
                                  model = x$model,
                                  n_iter = n_iter,
                                  n_burnin = n_burnin,
                                  n_thin = n_thin,
                                  n_chains = n_chains,
                                  spline_params = x$spline_params,
                                  stan_cores = x$stan_cores,
                                  ...)
    
    # prepare outputs
    size_real <- test_data$size
    size_pred <- pred_test
    r2_cv <- cor(size_real, size_pred) ** 2
    rmsd_cv <- sqrt(mean((size_real - size_pred) ** 2))
    md_cv <- mean((size_real - size_pred))
    type <- 'a holdout data set'
  } else {
    mod <- lapply(1:n_cv, stan_cv_internal,
                  mod_compiled,
                  x$data_set,
                  x$predictors,
                  x$model,
                  n_cv,
                  n_iter,
                  n_burnin,
                  n_thin,
                  n_chains,
                  x$spline_params,
                  x$stan_cores,
                  ...)
    
    # prepare outputs
    out_full <- do.call('rbind', mod)
    size_real <- out_full$size_real
    size_pred <- out_full$size_pred
    r2_cv <- round(cor(size_real, size_pred) ** 2, 3)
    rmsd_cv <- round(sqrt(mean((size_real - size_pred) ** 2)), 3)
    md_cv <- round(mean((size_real - size_pred)), 3)
    type <- paste0(n_cv, '-fold cross validation')
  }
  mod_cv <- list(size_real = size_real,
                 size_pred = size_pred,
                 r2 = r2_cv,
                 rmsd = rmsd_cv,
                 md = md_cv,
                 model = x$model,
                 val_type = type)
  class(mod_cv) <- 'growmod_cv'
  mod_cv
}

#' @rdname growmod-validate
#' @export
validate.growmod_multi <- function(x,
                                   n_cv = NULL,
                                   test_data = NULL,
                                   n_iter = NULL,
                                   n_burnin = NULL,
                                   n_thin = NULL,
                                   n_chains = NULL,
                                   ...) {
  
  mod_cv <- vector('list', length(x))
  names(mod_cv) <- sapply(x, function(x) x$model)
  for (i in seq(along = x)) {
    cat(paste0('Validating model ', i, ' of ', length(x), '.\n'))
    if (!is.null(n_cv)) {
      mod_cv[[i]] <- validate(x[[i]], n_cv = n_cv)
    } else {
      if (!is.null(test_data)) {
        mod_cv[[i]] <- validate(x[[i]], test_data = test_data)
      } else {
        stop('n_cv or test_data must be provided for model validation',
             call. = FALSE)
      }
    }
  }
  class(mod_cv) <- 'growmod_cv_multi'
  mod_cv
}

stan_cv_internal <- function(i,
                             mod_compiled,
                             data,
                             predictors,
                             model,
                             n_cv,
                             n_iter,
                             n_burnin,
                             n_thin,
                             n_chains,
                             spline_params,
                             stan_cores,
                             ...) {
  # defaults to structured loo or structured k-fold if blocks included in model
  if (length(data$block_data)) {
    if (data$n_block > 1) {
      n_cv <- ifelse(n_cv > data$n_block, data$n_block, n_cv)
      if (n_cv == data$n_block) {
        block_id <- i
        cv_id <- which(data$block_data == i)
      } else {
        n_holdout <- floor(data$n_block / n_cv)
        n_holdout <- ifelse(n_holdout == 0, 1, n_holdout)
        if (i < n_cv) {
          block_id <- ((i - 1) * n_holdout + 1):(i * n_holdout)
          cv_id <- NULL
          for (i in seq_along(block_id)) {
            cv_id <- c(cv_id, which(data$block_data == block_id[i]))
          }
        } else {
          block_id <- ((i - 1) * n_holdout + 1):data$n_block
          cv_id <- NULL
          for (i in seq_along(block_id)) {
            cv_id <- c(cv_id, which(data$block_data == block_id[i]))
          }
        }
      }
    } else {
      if (n_cv == data$n) {
        cv_id <- i
      } else {
        n_holdout <- floor(data$n / n_cv)
        n_holdout <- ifelse(n_holdout == 0, 1, n_holdout)
        if (i < n_cv) {
          cv_id <- ((i - 1) * n_holdout + 1):(i * n_holdout)
        } else {
          cv_id <- ((i - 1) * n_holdout + 1):data$n
        }
      }
    }
  } else {
    if (n_cv == data$n) {
      cv_id <- i
    } else {
      n_holdout <- floor(data$n / n_cv)
      n_holdout <- ifelse(n_holdout == 0, 1, n_holdout)
      if (i < n_cv) {
        cv_id <- ((i - 1) * n_holdout + 1):(i * n_holdout)
      } else {
        cv_id <- ((i - 1) * n_holdout + 1):data$n
      }
    }
  }
  if (model != 'spline') {
    mod_params <- get(paste0(model, '_param_fetch'))()
    num_params <- mod_params$num_par
  } else {
    num_params <- spline_params$n_knots + spline_params$degree
  }

  # check predictors
  if (!is.null(predictors)) {
    if (is.matrix(predictors) | is.data.frame(predictors)) {
      predictors_tmp <- predictors[-block_id, ]
      if (!is.matrix(predictors_tmp)) {
        predictors_tmp <- matrix(predictors_tmp, nrow = 1)
      }
      if (length(block_id) == 1) {
        predictors_tmp_test <- matrix(predictors[block_id, ], nrow = 1)
      } else {
        predictors_tmp_test <- predictors[block_id, ]
      }
    } else {
      if (is.list(predictors)) {
        predictors_tmp <- vector('list', length = length(predictors))
        predictors_tmp_test <- vector('list', length = length(predictors))
        for (j in seq(along = predictors)) {
          predictors_tmp[[j]] <- predictors[[j]][-block_id, ]
          if (!is.matrix(predictors_tmp[[j]])) {
            predictors_tmp[[j]] <- matrix(predictors_tmp[[j]], ncol = ncol(predictors[[j]]))
          }
          if (length(block_id) == 1) {
            predictors_tmp_test[[j]] <- matrix(predictors[[j]][block_id, ], ncol = ncol(predictors[[j]]))
          } else {
            predictors_tmp_test[[j]] <- predictors[[j]][block_id, ]
          }
        }
      }
    }
  } else {
    predictors_tmp <- NULL
    predictors_tmp_test <- NULL
  }
  pred_set <- check_preds(predictors = predictors_tmp,
                          model = model,
                          block_data = data$block_data[-cv_id],
                          num_params = num_params,
                          n = length(data$size_data[-cv_id]),
                          nblock = length(unique(data$block_data[-cv_id])))
  pred_set_test <- check_preds(predictors = predictors_tmp_test,
                               model = model,
                               block_data = data$block_data[cv_id],
                               num_params = num_params,
                               n = length(data$size_data[cv_id]),
                               nblock = length(unique(data$block_data[cv_id])))
  data_tmp <- list(size = data$size_data[-cv_id],
                   index = data$age[-cv_id],
                   block = data$block_data[-cv_id],
                   predictors = pred_set)
  test_data <- list(index = data$age[cv_id],
                    block = data$block_data[cv_id],
                    predictors = pred_set_test)
  data_cv <- growmod_data(data_set = data_tmp,
                          model = model,
                          num_params = num_params,
                          spline_params = spline_params,
                          n_plot = 10,
                          test_data = test_data)

  # fit model and sample predicted values
  stan_mod <- rstan::sampling(object = mod_compiled,
                              data = data_cv,
                              chains = n_chains,
                              iter = n_iter,
                              warmup = n_burnin,
                              thin = n_thin,
                              cores = stan_cores,
                              ...)
  
  # extract out-of-sample predictions
  size_real <- data$size_data[cv_id]
  cv_tmp <- rstan::summary(stan_mod, par = 'size_pred')$summary[, 'mean']

  # need to switch for model type (no blocks, with blocks, with predictors)
  out <- data.frame(size_pred = cv_tmp, size_real = size_real)
  out
}

predict_internal <- function(mod_compiled, train_data, test_data,
                             model,
                             n_iter, n_burnin, n_thin, n_chains,
                             spline_params,
                             stan_cores,
                             ...) {
  if (model != 'spline') {
    mod_params <- get(paste0(model, '_param_fetch'))()
    num_params <- mod_params$num_par
  } else {
    num_params <- spline_params$n_knots + spline_params$degree
  }
  pred_set <- check_preds(predictors = train_data$predictors,
                          model = model,
                          block_data = train_data$data_set$block_data,
                          num_params = num_params,
                          n = length(train_data$data_set$size_data),
                          nblock = length(unique(train_data$data_set$block_data)))
  pred_set_test <- check_preds(predictors = test_data$predictors,
                               model = model,
                               block_data = test_data$block,
                               num_params = num_params,
                               n = length(test_data$size),
                               nblock = length(unique(test_data$block)))
  data_tmp <- list(size = train_data$data_set$size_data,
                   index = train_data$data_set$age,
                   block = train_data$data_set$block_data,
                   predictors = pred_set)
  test_data_tmp <- list(index = test_data$index,
                        block = test_data$block,
                        predictors = pred_set_test)
  data_cv <- growmod_data(data_set = data_tmp,
                          model = model,
                          num_params = num_params,
                          spline_params = spline_params,
                          n_plot = 10,
                          test_data = test_data_tmp)
  
  # fit model just to training set
  stan_mod <- rstan::sampling(object = mod_compiled,
                              data = data_cv,
                              chains = n_chains,
                              iter = n_iter,
                              warmup = n_burnin,
                              thin = n_thin,
                              cores = stan_cores,
                              ...)
  
  # return size_pred
  size_pred <- rstan::summary(stan_mod, par = 'size_pred')$summary[, 'mean']

  size_pred
}

