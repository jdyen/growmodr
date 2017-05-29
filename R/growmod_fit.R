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
#' 
#' @param formula list containing data on size, age, species, and traits
#' 
#' @param size list containing data on size, age, species, and traits
#' 
#' @param index list containing data on size, age, species, and traits
#' 
#' @param block list containing data on size, age, species, and traits
#' 
#' @param predictors list containing data on size, age, species, and traits
#' 
#' @param data list containing data on size, age, species, and traits
#' 
#' @param model growth model form to be fitted
#' 
#' @param n_iter number of HMC iterations to run for stan model
#' 
#' @param n_burnin number of HMC iterations to run for stan model
#' 
#' @param n_thin thinning rate for HMC chains
#' 
#' @param n_chains number of HMC chains
#' 
#' @param spline_params named list of settings for spline model (degree, n_knots, spline_type)
#' 
#' @param \dots parameters to be passed to stan model call
#' 
#' @details \code{growmod} takes a formula or data vectors as arguments
#'   and constructs and fits a Stan model for a chosen growth curve.
#' 
#' @return \code{mod} A fitted \code{grow_mod} object containing
#'   parameter estimates, validation statistics, and the original
#'   data set used to fit the model. 
#' 
#' growmod <- function(x, ...) {
  UseMethod('growmod')
}

#' @describeIn growmod Fit growth model from formula
growmod.formula <- function(formula,
                            predictors = NULL,
                            data,
                            model = 'hillslope',
                            n_iter = 5000,
                            n_burnin = 2000,
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
  
  # do X
  if (exists(size_resp, data)) {
    size_data <- get(size_resp, data)
  } else {
    if (exists(size_resp, parent.frame())) {
      size_data <- get(size_resp, parent.frame())
    } else {
      stop(paste0(size_resp, ' not available in data or in global environment'),
           call. = FALSE)
    }
  }
  
  # do Y
  if (exists(index_var, data)) {
    index_data <- get(index_var, data)
  } else {
    if (exists(index_var, parent.frame())) {
      index_data <- get(index_var, parent.frame())
    } else {
      stop(paste0(index_var, ' not available in data or in global environment'),
           call. = FALSE)
    }
  }

  # check if a blocking variable has been provided; return NULL if not;
  #  (fits a model assuming all data points are from the same growth curve)
  if (length(pred_vars) == 1) {
    block_var <- NULL
  } else {
    block_var <- pred_vars[2]
    if (exists(block_var, data)) {
      block_data <- get(block_var, data)
    } else {
      if (exists(block_var, parent.frame())) {
        block_data <- get(block_var, parent.frame())
      } else {
        stop(paste0(block_var, ' not available in data or in global environment'),
             call. = FALSE)
      }
    }
  }
  n <- length(size_data)

  # get predictor data
  if (!is.null(predictors)) {
    # DO X
  } else {
    cat('model has no predictors so will do Y')  ## decide whether predictors = NULL or something else
  }
  
  model <- match.arg(model)
  
  # check data for errors
  # define function: check_growmod_data(size_resp, index_var, block_var, predictors, model)
  # predictors must be matrix or list(length = num_params)
  
  
  ## set up to loop over multiple models? Or have wrapper for that?
  ## NEED TO HAVE
  #-no blocks or covariates (one growth curve)
  #-blocks but no covariates (multiple curves)
  #-blocks with identical covars on all params
  #-blocks with different covars for each param
  ## Have an option for long format data or relational data
  
  # fit model growmod.default
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

#' @describeIn growmod Default fitting method for growmod
growmod.default <- function(size,
                            index,
                            block,
                            predictors,
                            model,
                            n_iter, n_burnin, n_thin, n_chains,
                            stan_cores,
                            spline_params,
                            ...) {
  # switch_fun for model - switch(model) return (num_params)
  # define curve parameters using a paste function (e.g. paste(a, 1:num_params))
  # work out predictor params similarly

  # model funs - return num_params
  mod.file <- gen_mod_file(model = model)
  data.set <- fetch_model_data(data_set = list(size = size,
                                               age = age,
                                               block_id = block_id,
                                               predictors = predictors),
                               model = model,
                               n.plot = 100,
                               spline_params = spline_params)

  # fit model
  mod <- stan_mod_fit
    
  # set S3 class
  if (class(mod) != 'grow_mod') {
    class(mod) <- 'grow_mod'
  }
  
  # return outputs
  mod
}
