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
    if (is.numeric(predictors)) {
      predictors <- matrix(predictors, ncol = 1)
    }
    if (is.matrix(predictors) | is.data.frame(predictors)) {
      npred <- ncol(predictors)
      nblock_pred <- nrow(predictors)
    } else {
      if (length(predictors) != num_params) {
        stop(paste0('the ', model, ' model has ', num_params,
                    ' parameters but predictors has ',
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
    }
    if (nblock != nblock_pred) {
      if (nblock_pred != n) {
        stop(paste0('predictors should have ', n, ' or ', nblock,
                    ' rows in each element'),
             call. = FALSE)
      }
      warning('there should be one value of each predictor variable for each 
                 unique level of the blocking variable; tapply() has been
                 used to calculate mean values of each predictor for each
                 blocking level.',
              call. = FALSE)
      if (is.matrix(predictors) | is.data.frame(predictors)) {
        predictors_clean <- matrix(NA,
                                   nrow = nblock,
                                   ncol = ncol(predictors))
        for (i in 1:ncol(predictors)) {
          predictors_clean[, i] <- tapply(predictors[, i],
                                          block_data,
                                          mean,
                                          na.rm = TRUE)
        }
      } else {
        predictors_clean <- vector('list', length = length(predictors))
        for (j in seq(along = predictors_clean)) {
          predictors_clean[[j]] <- matrix(NA,
                                          nrow = nblock,
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
    predictors_clean <- NULL
    npred <- NULL
    nblock_pred <- NULL
  }
  predictors_clean
}