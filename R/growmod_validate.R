### DO WE NEED TO REFIT MODELS SPECIALLY WITH HOLDOUT DATA OR CAN WE JUST STORE PARAMS
## AND USE predict.growmod?

#' @rdname growmod
#' @export
validate <- function(x, ...) {
  UseMethod('validate')
}

#' @rdname growmod
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
  ## ADD THIS
}

#' @rdname growmod
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
    mod_compiled <- stan_model(file = x$mod_file)
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
      cat('Performing model validation based on test_data\n')
    }
  } else {
    # UPDATE THIS TO ADDRESS MODEL VARIANTS
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
    # predict to test data
    pred_test <- predict(mod_cv, test_data = test_data)
    
    # prepare outputs
    size_real <- test_data$size_data
    size_pred <- pred_test
    r2_cv <- cor(size_real, size_pred) ** 2
    rmsd_cv <- sqrt(mean((size_real - size_pred) ** 2))
    md_cv <- mean((size_real - size_pred))
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
  }
  mod_cv <- list(size_real = size_real,
                 size_pred = size_pred,
                 r2 = r2_cv,
                 rmsd = rmsd_cv,
                 md = md_cv,
                 model = x$model)
  class(mod_cv) <- 'growmod_cv'
  mod_cv
}

#' @rdname growmod
#' @export
validate.growmod_multi <- function(x,
                                   n_cv,
                                   train_data = NULL,
                                   test_data = NULL,
                                   n_iter = NULL,
                                   n_burnin = NULL,
                                   n_thin = NULL,
                                   n_chains = NULL,
                                   ...) {
  
  mod_cv <- vector('list', length(x))
  names(mod_cv) <- sapply(x, function(x) x$model)
  for (i in seq(along = x)) {
    mod_cv[[i]] <- validate(x[[i]])
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

  ## NEED to check dimension of predictors -- could be n rows rather htan nblock rows
  ## (might be easier to do this above through
  #    definition of block_id to avoid changing block_id section here)
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
            predictors_tmp[[j]] <- matrix(predictors_tmp[[j]], nrow = 1)
          }
          if (length(block_id) == 1) {
            predictors_tmp_test[[j]] <- matrix(predictors[[j]][block_id, ], nrow = 1)
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

  # fit model just to training set
  stan_mod <- sampling(object = mod_compiled,
                       data = data_cv,
                       chains = n_chains,
                       iter = n_iter,
                       warmup = n_burnin,
                       thin = n_thin,
                       cores = stan_cores,
                       ...)
  
  # extract out-of-sample predictions
  size_real <- data$size_data[cv_id]
  cv_tmp <- summary(stan_mod, par = 'size_pred')$summary[, 'mean']

  # need to switch for model type (no blocks, with blocks, with predictors)
  out <- data.frame(size_pred = cv_tmp, size_real = size_real)
  out
}

