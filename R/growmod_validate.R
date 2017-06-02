### DO WE NEED TO REFIT MODELS SPECIALLY WITH HOLDOUT DATA OR CAN WE JUST STORE PARAMS
## AND USE predict.growmod?

#' @rdname growmod
#' @export
validate <- function(x, ...) {
  UseMethod('validate')
}

#' @rdname growmod
#' @export
validate.grow_mod <- function(x,
                              n_cv,
                              n_iter, n_burnin, n_thin,
                              n_chains,
                              train_data = NULL,
                              test_data = NULL,
                              ...) {
  # generate model file
  mod_compiled <- stan_model(file = x$mod_file)
  
  # run cv in loop
  if (!is.null(test_data)) {
    # fit to train data
    
    # predict to test data
    pred_test <- predict(mod_cv, test_data = test_data)
    
    # prepare outputs
    size_real <- test_data$size_data
    size_pred <- pred_test
    r2_cv <- cor(size_real, size_pred) ** 2
    rmsd_cv <- sqrt(mean((size_real - size_pred) ** 2))
    md_cv <- mean((size_real - size_pred))
  } else {
    mod <- lapply(1:n.cv, stan_cv_internal,
                  mod_compiled,
                  x$data_set,
                  x$predictors
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
    size_real <- x$data_set$size_data
    size_pred <- out_full$size_pred
    r2_cv <- cor(size_real, size_pred) ** 2
    rmsd_cv <- sqrt(mean((size_real - size_pred) ** 2))
    md_cv <- mean((size_real - size_pred))
  }
  mod_cv <- list(size_real = size_real,
                 size_pred = size_pred,
                 r2 = r2_cv,
                 rmsd = rmsd_cv,
                 md = md_cv)
  class(mod_cv) <- 'growmod_cv'
  mod_cv
}

#' @rdname growmod
#' @export
validate.grow_mod_multi <- function(x, n_cv, n_iter, n_chains,
                                    test_data = NULL,
                                    ...) {
  
  mod_cv <- vector('list', length(x))
  for (i in seq(along = x)) {
    # generate model file
    mod_compiled <- stan_model(file = x[[i]]$mod_file)
    
    # run cv in loop
    if (!is.null(test_data)) {
      # fit to train data
    } else {
      mod <- lapply(1:n.cv, stan_cv_internal,
                    mod_compiled,
                    x[[i]]$data_set,
                    x[[i]]$predictors,
                    x[[i]]$model,
                    n_cv,
                    n_iter,
                    n_burnin,
                    n_thin,
                    n_chains,
                    x[[i]]$spline_params,
                    x[[i]]$stan_cores,
                    ...)
      
      # prepare outputs
      out_full <- do.call('rbind', mod)
      r2_cv <- cor(out_full$size_pred, out_full$size_real) ** 2
      rmsd_cv <- sqrt(mean((out_full$size_pred - out_full$size_real) ** 2))
      md_cv <- mean((out_full$size_pred - out_full$size_real))
      mod_cv[[i]] <- list(r2 = r2_cv,
                          rmsd = rmsd_cv,
                          md = md_cv)
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
  ## NEED a version for structured loo, structured k-fold, random loo, random k-fold
  # structured loo or unstructured k-fold
  if (n_cv == data$n_block) {
    cv_id <- which(data$block_data == i)
  } else {
    n_holdout <- floor(data$n / n_cv)
    if (i < n.cv) {
      cv_id <- ((i - 1) * n_holdout + 1):(i * n_holdout)
    } else {
      cv_id <- ((i - 1) * n_holdout + 1):(data$n)
    }
  }
  if (model != 'spline') {
    mod_params <- get(paste0(model, '_param_fetch'))()
    num_params <- mod_params$num_par
  } else {
    num_params <- spline_params$n_knots + spline_params$degree
  }
  if (!is.null(predictors)) {
    if (is.matrix(predictors) | is.data.frame(predictors)) {
      predictors_tmp <- predictors[-cv_id, ]
      predictors_tmp_test <- predictors[cv_id, ]
    } else {
      if (is.list(predictors)) {
        predictors_tmp <- vector('list', length = length(predictors))
        predictors_tmp_test <- vector('list', length = length(predictors))
        for (j in seq(along = predictors)) {
          predictors_tmp[[j]] <- predictors[[j]][-cv_id, ]
          predictors_tmp_test[[j]] <- predictors[[j]][cv_id, ]
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
  data_tmp <- list(size = data$size_data[-cv_id],
                   index = data$age[-cv_id],
                   block = data$block_data[-cv_id],
                   predictors = pred_set)
  data_cv <- growmod_data(data_set = data_tmp,
                          model = model,
                          num_params = num_params,
                          spline_params = spline_params,
                          n_plot = 10)
  
  # fit model just to training set
  stan_mod <- sampling(object = mod_compiled,
                       data_cv = data_set,
                       chains = n_chains,
                       iter = n_iter,
                       warmup = n_burnin,
                       thin = n_thin,
                       cores = stan_cores,
                       ...)
  
  # use predict.growmod(mod_train, data_test) to get predictions
  pred_test <- check_preds(predictors = predictors_tmp_test,
                           model = model,
                           block_data = data$block_data[cv_id],
                           num_params = num_params,
                           n = length(data$size_data[cv_id]),
                           nblock = length(unique(data$block_data[cv_id])))
  test_data <- list(size = data$size_data[cv_id],
                    index = data$age[cv_id],
                    block = data$block_data[cv_id],
                    predictors = pred_set)
  # need to switch for model type (no blocks, with blocks, with predictors)
  ## THIS MIGHT NEED TO HAPPEN EARLIER
  param_est <- get_posterior_mean(pars = paste0('b', 1:num_params))
  ## WORK OUT DETAILS FOR DIMENSIONS
  b_params <- matrix(param_est, ncol = num_params)
  # define this function or use existing data sim function (move to utils.R)
  cv_tmp <- calc_growth_curve(model = model,
                              index = test_data$index,
                              params = (param_est %*% test_data$predictors))
  out <- data.frame(size_pred = cv_tmp, size_real = test_data$size)
  out
}
