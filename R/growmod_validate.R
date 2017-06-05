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
  #mod_compiled <- stan_model(file = x$mod_file)
  # don't need this line if it works
  mod_compiled <- x$stanmod
  
  ## SET n_iter etc to NULL and estimate from fitted model
  
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
    if (n_cv > x$data_set$n_block) {
      n_cv <- x$data_set$n_block
    }
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
validate.grow_mod_multi <- function(x, n_cv, n_iter, n_chains,
                                    train_data = NULL,
                                    test_data = NULL,
                                    ...) {
  
  mod_cv <- vector('list', length(x))
  names(mod_cv) <- sapply(x, function(x) x$model)
  for (i in seq(along = x)) {
    # generate model file
    mod_compiled <- stan_model(file = x[[i]]$mod_file)
    
    # run cv in loop
    if (!is.null(test_data)) {
      # fit to train data
    } else {
      mod <- lapply(1:n_cv, stan_cv_internal,
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
      size_real <- out_full$size_real
      size_pred <- out_full$size_pred
      r2_cv <- round(cor(size_real, size_pred) ** 2, 3)
      rmsd_cv <- round(sqrt(mean((size_real - size_pred) ** 2)), 3)
      md_cv <- round(mean((size_real - size_pred)), 3)
      mod_cv[[i]] <- list(size_real = size_real,
                          size_pred = size_pred,
                          r2 = r2_cv,
                          rmsd = rmsd_cv,
                          md = md_cv,
                          model = x[[i]]$model)
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
                       data = data_cv,
                       chains = n_chains,
                       iter = n_iter,
                       warmup = n_burnin,
                       thin = n_thin,
                       cores = stan_cores,
                       ...)
  
  # use predict function to get predictions
  pred_test <- check_preds(predictors = predictors_tmp_test,
                           model = model,
                           block_data = data$block_data[cv_id],
                           num_params = num_params,
                           n = length(data$size_data[cv_id]),
                           nblock = length(unique(data$block_data[cv_id])))
  test_data <- list(size = data$size_data[cv_id],
                    index = data$age[cv_id],
                    block = data$block_data[cv_id],
                    predictors = pred_test)
  data_tmp_test <- growmod_data(data_set = test_data,
                                model = model,
                                num_params = num_params,
                                spline_params = spline_params,
                                n_plot = 10)
  
  # need to switch for model type (no blocks, with blocks, with predictors)
  h_est <- matrix(NA, nrow = length(unique(test_data$block)), ncol = num_params)
  for (j in 1:num_params) {
    param_tmp <- get_posterior_mean(stan_mod, pars = paste0('b', j))
    param_tmp <- param_tmp[, ncol(param_tmp)]
    pred_tmp <- data_tmp_test[which(names(data_tmp_test) == paste0('x', j))][[1]]
    h_est[, j] <- pred_tmp %*% param_tmp
  }
  cv_tmp <- calc_growth_curve(model = model,
                              index = test_data$index,
                              block = test_data$block,
                              params = h_est)
  out <- data.frame(size_pred = cv_tmp, size_real = test_data$size)
  out
}

