# utils.R

# check predictor variables
check_preds <- function(predictors,
                        model,
                        block_data,
                        num_params,
                        n,
                        nblock) {
  # get predictor data
  if (!is.null(predictors)) {
    if (is.null(block_data)) {
      warning('no blocking variable was provided so predictors will not be used.',
              call. = FALSE)
    } else {
      if (is.matrix(predictors) | is.data.frame(predictors)) {
        npred <- ncol(predictors)
        nblock_pred <- nrow(predictors)
      } else {
        if (is.list(predictors)) {
          if (length(predictors) != num_params) {
            stop(paste0('the ', model, ' model has ', num_params,
                        ' parameters but predictors only has ',
                        length(predictors), ' elements. There should 
                        be one element of predictors for each model parameter.'),
                 call. = FALSE)
          }
          npred <- sapply(predictors, ncol)
          nblock_pred <- sapply(predictors, nrow)
          if (!all(nblock_pred == nblock_pred[1])) {
            stop(paste0('all elements of predictors should have the same number
                 of rows for the ', model, ' model.'),
                 call. = FALSE)
          } else {
            nblock_pred <- nblock_pred[1]
          }
        } else {
          stop('predictors must be a matrix, data.frame or list.',
               call. = FALSE)
        }
      }
    }
    if (nblock != nblock_pred) {
      if (nblock_pred != n) {
        stop(paste0('predictors should have ', n, ' or ', nblock,
                    ' rows in each element'),
             call. = FALSE)
      }
      warning('there should be value of each predictor variable for each 
                 unique level of the blocking variable; tapply() has been
                 used to calculate mean values of each predictor for each
                 blocking level.',
              call. = FALSE)
      if (is.matrix(predictors) | is.data.frame(predictors)) {
        predictors_clean <- matrix(NA,
                                   nrow = nrow(nblock),
                                   ncol = ncol(predictors))
        for (i in 1:ncol(pred_list$predictors)) {
          predictors_clean[, i] <- tapply(predictors[, i],
                                          block_data,
                                          mean,
                                          na.rm = TRUE)
        }
      } else {
        predictors_clean <- vector('list', length = length(predictors))
        for (j in seq(along = predictors_clean)) {
          predictors_clean[[j]] <- matrix(NA,
                                          nrow = nrow(nblock),
                                          ncol = ncol(predictors[[j]]))
          for (i in 1:ncol(predictors[[j]])) {
            predictors_clean[[j]][, i] <- tapply(predictors[[j]][, i],
                                                 block_data,
                                                 mean,
                                                 na.rm = TRUE)
          }
        }
      }
    } else {
      predictors_clean <- predictors
    }
  } else {
    if (!is.null(block_data)) {
      cat('Note: model has no predictors and will assume that each block
          has an independent growth curve.\n')
    }
    predictors_clean <- NULL
    npred <- NULL
    nblock_pred <- NULL
  }
  predictors_clean
}

calc_growth_curve <- function(model, index, block, params) {
  eqn_set <- switch(model, 
                    'hillslope' = function(x, h1, h2, h3) h1 / (1 + exp(-h2 * (x - h3))),
                    'hillslope_log' = function(x, h1, h2, h3) h1 / (1 + exp(-h2 * (log(x) - h3))),
                    'power2' = function(x, h1, h2, h3) h1 * (x ** h2),
                    'expo' = function(x, h1, h2, h3) h1 + (h2 * log(x)),
                    'monod' = function(x, h1, h2, h3) h1 * (x / (h2 + x)),
                    'neg_exp' = function(x, h1, h2, h3) h1 * (1 - exp(-h2 * x)),
                    'koblog' = function(x, h1, h2, h3) h1 * log(1 + (x / h2)),
                    'power3' = function(x, h1, h2, h3) h1 * (x ** (h2 - (h3 / x))),
                    'logistic3' = function(x, h1, h2, h3) h1 / (1 + exp(-h2 * x + h3)),
                    'archibold' = function(x, h1, h2, h3) h1 / (h2 + (h3 ** x)),
                    'weibull3' = function(x, h1, h2, h3) h1 * (1 - exp(-h2 * (x ** h3))))
  out <- vector('numeric', length = length(index))
  for (i in 1:length(unique(block))) {
    x_vals <- index[which(block == unique(block)[i])]
    h_vals <- params[i, ]
    out[which(block == unique(block)[i])] <- eqn_set(x_vals,
                                                     h_vals[1],
                                                     h_vals[2],
                                                     h_vals[3])
  }
  out
}
