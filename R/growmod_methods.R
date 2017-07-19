#' Comparison method for \code{"growmod"} objects
#' 
#' @description Compare multiple fitted growmod models
#' 
#' @param x an object of class \code{"growmod"}, \code{"growmod_cv"},
#'   \code{"growmod_multi"} or \code{"growmod_cv_multi"}
#' @param \dots additional objects of class \code{"growmod"}, \code{"growmod_cv"},
#'   \code{"growmod_multi"} or \code{"growmod_cv_multi"}
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # simulate some data
#' data_test <- growmod_sim()
#'
#' # fit a growmod model
#' mod1 <- growmod(size ~ (index | block / predictors),
#'                 data = data_sim,
#'                 model = 'hillslope',
#'                 n_iter = 100,
#'                 n_burnin = 50,
#'                 n_chains = 1,
#'                 stan_cores = 1)
#'
#' # fit a second model
#' mod2 <- growmod(size ~ (index | block / predictors),
#'                 data = data_sim,
#'                 model = 'koblog',
#'                 n_iter = 100,
#'                 n_burnin = 50,
#'                 n_chains = 1,
#'                 stan_cores = 1)
#'
#' # compare the two fitted models
#' compare(mod1, mod2)
#' }
#'
compare <- function(x, ...) {
  UseMethod('compare')
}

#' @describeIn compare compare fitted \code{"growmod"} models
#' @export
compare.growmod <- function(..., x) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(x)) {
      stop("If 'x' is specified then '...' should not be specified.")
    }
    nms <- as.character(match.call(expand.dots = TRUE))[-1L]
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list.", call. = FALSE)
    }
    dots <- x
    nms <- names(dots)
    if (!length(nms)) {
      nms <- paste0('model', seq_along(dots))
    }
  }
  print_out <- rbind(sapply(dots, function(x) x$r2),
                     sapply(dots, function(x) x$rmsd),
                     sapply(dots, function(x) x$md),
                     sapply(dots, function(x) round(x$loo$looic, 2)),
                     sapply(dots, function(x) round(x$waic$waic, 2)))
  rownames(print_out) <- c('r2', 'rmsd', 'md', 'loo_ic', 'waic')
  colnames(print_out) <- sapply(dots, function(x) x$model)
  print_out
}

#' @describeIn compare compare fitted \code{"growmod_cv"} models
#' @export
compare.growmod_cv <- function(..., x) {
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
  print_out <- rbind(sapply(dots, function(x) x$r2),
                     sapply(dots, function(x) x$rmsd),
                     sapply(dots, function(x) x$md))
  rownames(print_out) <- c('r2', 'rmsd', 'md')
  colnames(print_out) <- sapply(dots, function(x) x$model)
  print_out
}

#' @describeIn compare compare fitted \code{"growmod_multi"} models
#' @export
compare.growmod_multi <- function(..., x) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(x)) {
      stop("If 'x' is specified then '...' should not be specified.")
    }
    nms <- as.character(match.call(expand.dots = TRUE))[-1L]
    if (length(dots) > 1) {
      out_tmp <- NULL
      for (i in seq_along(dots)) {
        out_tmp <- append(out_tmp, dots[[i]])
      }
    } else {
      out_tmp <- dots[[1]]
    }
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list.")
    }
    out_tmp <- x
    nms <- names(out_tmp)
    if (!length(nms)) {
      nms <- paste0('model', seq_along(out_tmp))
    }
  }
  print_out <- rbind(sapply(out_tmp, function(x) x$r2),
                     sapply(out_tmp, function(x) x$rmsd),
                     sapply(out_tmp, function(x) x$md),
                     sapply(out_tmp, function(x) round(x$loo$looic, 2)),
                     sapply(out_tmp, function(x) round(x$waic$waic, 2)))
  rownames(print_out) <- c('r2', 'rmsd', 'md', 'loo_ic', 'waic')
  colnames(print_out) <- sapply(out_tmp, function(x) x$model)
  print_out
}

#' @describeIn compare compare fitted \code{"growmod_cv_multi"} models
#' @export
compare.growmod_cv_multi <- function(..., x) {
  dots <- list(...)
  if (length(dots)) {
    if (!missing(x)) {
      stop("If 'x' is specified then '...' should not be specified.")
    }
    nms <- as.character(match.call(expand.dots = TRUE))[-1L]
    if (length(dots) > 1) {
      out_tmp <- NULL
      for (i in seq_along(dots)) {
        out_tmp <- append(out_tmp, dots[[i]])
      }
    } else {
      out_tmp <- dots[[1]]
    }
  } else {
    if (!is.list(x) || !length(x)) {
      stop("'x' must be a list.")
    }
    out_tmp <- x
    nms <- names(out_tmp)
    if (!length(nms)) {
      nms <- paste0('model', seq_along(out_tmp))
    }
  }
  print_out <- rbind(sapply(out_tmp, function(x) x$r2),
                     sapply(out_tmp, function(x) x$rmsd),
                     sapply(out_tmp, function(x) x$md))
  rownames(print_out) <- c('r2', 'rmsd', 'md')
  colnames(print_out) <- sapply(out_tmp, function(x) x$model)
  print_out
}

#' Plot method for \code{"growmod"} objects
#' @method plot growmod
#'
#' @param x an object of class \code{"growmod"}, \code{"growmod_cv"},
#'   \code{"growmod_multi"} or \code{"growmod_cv_multi"}
#' @param y unused default argument to plot function
#' @param \dots additional objects of class \code{"growmod"}, \code{"growmod_cv"},
#'   \code{"growmod_multi"} or \code{"growmod_cv_multi"}
#' @param group_blocks (logical; default equals TRUE) for \code{"growmod_multi"}
#'   or \code{"growmod_cv_multi"} objects; specify whether to plot each block
#'   in its own plot (default) or each model in its own plot
#' @return NULL
#' 
#' @import stats
#' @import graphics
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # simulate some data
#' data_test <- growmod_sim()
#'
#' # fit a growmod model
#' mod1 <- growmod(size ~ (index | block / predictors),
#'                 data = data_sim,
#'                 model = 'hillslope',
#'                 n_iter = 100,
#'                 n_burnin = 50,
#'                 n_chains = 1,
#'                 stan_cores = 1)
#'
#' # plot the fitted model
#' plot(mod1)
#' }
#'
plot.growmod <- function(x, y, ...) {
  mod_tmp <- x$stan_summary
  data_tmp <- x$data_set
  if (length(data_tmp$block_data)) {
    for (i in 1:data_tmp$n_block) {
      row_id <- grep(paste0('plot\\[[[:digit:]]*,', i, '\\]'), rownames(mod_tmp))
      plot_data <- mod_tmp[row_id, c('mean', '2.5%', '50%', '97.5%')]
      if (any(plot_data == Inf)) {
        plot_data[which(plot_data == Inf)] <- NA
        warning(paste0('Some fitted values for the ', x$model, ' model were Inf and have been removed'),
                call. = FALSE)
      }
      x_plot <- data_tmp$age_plot
      y_lims <- c(min(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)]), na.rm = TRUE),
                  max(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)]), na.rm = TRUE))
      plot(plot_data[, 1] ~ x_plot,
           type = 'l',
           las = 1,
           bty = 'l',
           xlab = 'Time',
           ylab = 'Height',
           ylim = y_lims)
      polygon(c(x_plot, rev(x_plot)),
              c(plot_data[, 2], rev(plot_data[, 4])),
              col = 'grey65',
              border = NA)
      lines(plot_data[, 1] ~ x_plot, lty = 1, lwd = 1.2)
      lines(plot_data[, 3] ~ x_plot, lty = 2, lwd = 1.2)
      points(data_tmp$size_data[which(data_tmp$block_data == i)] ~ data_tmp$age[which(data_tmp$block_data == i)],
             pch = 16)
      mtext(paste0('Block ', i), side = 3, line = 1, adj = 0)
    }
  } else {
    row_id <- grep(paste0('plot\\['), rownames(mod_tmp))
    plot_data <- mod_tmp[row_id, c('mean', '2.5%', '50%', '97.5%')]
    if (any(plot_data == Inf)) {
      plot_data[which(plot_data == Inf)] <- NA
      warning(paste0('Some fitted values for the ', x$model, ' model were Inf and have been removed'),
              call. = FALSE)
    }
    x_plot <- data_tmp$age_plot
    y_lims <- c(min(c(plot_data, data_tmp$size_data), na.rm = TRUE),
                max(c(plot_data, data_tmp$size_data), na.rm = TRUE))
    plot(plot_data[, 1] ~ x_plot,
         type = 'l',
         las = 1,
         bty = 'l',
         xlab = 'Time',
         ylab = 'Height',
         ylim = y_lims)
    polygon(c(x_plot, rev(x_plot)),
            c(plot_data[, 2], rev(plot_data[, 4])),
            col = 'grey65',
            border = NA)
    lines(plot_data[, 1] ~ x_plot, lty = 1, lwd = 1.2)
    lines(plot_data[, 3] ~ x_plot, lty = 2, lwd = 1.2)
    points(data_tmp$size_data ~ data_tmp$age,
           pch = 16)
  }
}

#' @describeIn plot.growmod plot fitted \code{"growmod_cv"} models
#' @method plot growmod_cv
#' @export
plot.growmod_cv <- function(x, y, ...) {
  if (any(x$size_pred == Inf)) {
    x$size_pred[which(x$size_pred == Inf)] <- NA
  }
  min_val <- min(0, min(x$size_real, na.rm = TRUE),
                 min(x$size_pred, na.rm = TRUE))
  max_val <- max(max(x$size_real, na.rm = TRUE),
                 max(x$size_pred, na.rm = TRUE))
  plot(x$size_real, x$size_pred,
       pch = 16,
       las = 1,
       bty = 'l',
       xlab = 'Observed size',
       ylab = 'Predicted size',
       col = 'gray40',
       xlim = c(min_val, max_val),
       ylim = c(min_val, max_val))
  lines(c(min_val, max_val + 2), c(min_val, max_val + 2),
        lty = 2, lwd = 2, col = 'gray60')
  abline(lm(x$size_pred ~ -1 + x$size_real),
         col = 'gray40', lwd = 2)
}

#' @describeIn plot.growmod plot fitted \code{"growmod_multi"} models
#' @method plot growmod_multi
#' @export
plot.growmod_multi <- function(x, y, group_blocks = TRUE, ...) {
  old_mfrow <- par()$mfrow
  noblock_mod <- any(sapply(x, function(x) length(x$data_set$block_data)) == 0)
  if ((group_blocks) & (!noblock_mod)) {
    num_plots <- length(x)
    extract_summary <- lapply(x, function(x) x$stan_summary)
    extract_data <- lapply(x, function(x) x$data_set)
    for (i in 1:extract_data[[1]]$n_block) {
      par(mfrow = c(round(sqrt(num_plots)), ceiling(num_plots / round(sqrt(num_plots)))))
      for (j in seq_along(x)) {
        mod_tmp <- extract_summary[[j]]
        data_tmp <- extract_data[[j]]
        row_id <- grep(paste0('plot\\[[[:digit:]]*,', i, '\\]'), rownames(mod_tmp))
        plot_data <- mod_tmp[row_id, c('mean', '2.5%', '50%', '97.5%')]
        if (any(plot_data == Inf)) {
          plot_data[which(plot_data == Inf)] <- NA
          warning(paste0('Some fitted values for the ', x$model, ' model were Inf and have been removed'),
                  call. = FALSE)
        }
        x_plot <- data_tmp$age_plot
        y_lims <- c(min(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)]), na.rm = TRUE),
                    max(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)]), na.rm = TRUE))
        plot(plot_data[, 1] ~ x_plot,
             type = 'l',
             las = 1,
             bty = 'l',
             xlab = 'Time',
             ylab = 'Height',
             ylim = y_lims)
        polygon(c(x_plot, rev(x_plot)),
                c(plot_data[, 2], rev(plot_data[, 4])),
                col = 'grey65',
                border = NA)
        lines(plot_data[, 1] ~ x_plot, lty = 1, lwd = 1.2)
        lines(plot_data[, 3] ~ x_plot, lty = 2, lwd = 1.2)
        points(data_tmp$size_data[which(data_tmp$block_data == i)] ~ data_tmp$age[which(data_tmp$block_data == i)],
               pch = 16)
        if (j == 1) {
          mtext(paste0('Block ', i), side = 3, line = 1, adj = 0)
        }
      }
    }
  } else {
    for (i in seq_along(x)) {
      if (length(x[[i]]$data_set$block_data)) {
        num_plots <- x[[i]]$data_set$n_block
      } else {
        num_plots <- 1
      }
      par(mfrow = c(round(sqrt(num_plots)), ceiling(num_plots / round(sqrt(num_plots)))))
      plot(x[[i]])
    }
  }
  par(mfrow = old_mfrow)  
}

#' @describeIn plot.growmod plot fitted \code{"growmod_cv_multi"} models
#' @method plot growmod_cv_multi
#' @export
plot.growmod_cv_multi <- function(x, y, ...) {
  old_mfrow <- par()$mfrow
  num_plots <- length(x)
  par(mfrow = c(round(sqrt(num_plots)), ceiling(num_plots / round(sqrt(num_plots)))))
  for (i in seq_along(x)) {
    plot(x[[i]])
  }
  par(mfrow = old_mfrow)  
}

#' Summary method for \code{"growmod"} objects
#' @method summary growmod
#' @param object an object of class \code{"growmod"}, \code{"growmod_cv"},
#'   \code{"growmod_multi"} or \code{"growmod_cv_multi"}
#' @param \dots unused additional arguments
#' @return NULL
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # simulate some data
#' data_test <- growmod_sim()
#'
#' # fit a growmod model
#' mod1 <- growmod(size ~ (index | block / predictors),
#'                 data = data_sim,
#'                 model = 'hillslope',
#'                 n_iter = 100,
#'                 n_burnin = 50,
#'                 n_chains = 1,
#'                 stan_cores = 1)
#'
#' # summarise the fitted model
#' summary(mod1)
#' }
#'
summary.growmod <- function(object, ...) {
  print_out <- c(object$r2, object$rmsd, object$md,
                 round(object$loo$looic, 2),
                 round(object$waic$waic, 2))
  names(print_out) <- c('r2', 'rmsd', 'md', 'loo_ic', 'waic')
  mod_type <- object$model
  rhats <- object$stan_summary[, 'Rhat']
  if (any(is.na(rhats))) {
    rhats <- rhats[-which(is.na(rhats))]
  }
  if (!is.null(object$predictors)) {
    cat(paste0('The ', object$model, ' model was fitted to growth data with ', 
               object$data_set$n, ' observations.\n',
               'This model had ', object$data_set$n_block,
               ' blocking variables and ', ncol(object$predictors), ' predictor variables.\n\n'))
  } else {
    if (length(object$data_set$block_data)) {
      cat(paste0('The ', object$model, ' model was fitted to growth data with ', 
                 object$data_set$n, ' observations.\n',
                 'This model had ', object$data_set$n_block,
                 ' blocking variables but did not include any predictor variables.\n\n'))
    } else {
      cat(paste0('The ', object$model, ' model was fitted to growth data with ', 
                 object$data_set$n, ' observations.\n',
                 'This model had no blocking variables or predictor variables.\n\n'))
    }
  }
  cat('The model call was:\n')
  print(object$call)
  cat('\nThe fitted model had the following summary statistics:\n')
  print(print_out)
  if (any(rhats > 1.1)) {
    cat('\nNote: some Rhats were greater than 1.1; consider increasing n_iter.\n')
  }
}

#' @describeIn summary.growmod summarise fitted \code{"growmod_cv"} models
#' @method summary growmod_cv
#' @export
summary.growmod_cv <- function(object, ...) {
  print_out <- c(object$r2, object$rmsd, object$md)
  names(print_out) <- c('r2', 'rmsd', 'md')
  mod_type <- object$model
  cat(paste0('The ', object$model, ' model was validated with ', 
             object$val_type, '.\n\n'))
  cat('The fitted model had the following summary statistics:\n')
  print(print_out)
}

#' @describeIn summary.growmod summarise fitted \code{"growmod_multi"} models
#' @method summary growmod_multi
#' @export
summary.growmod_multi <- function(object, ...) {
  cat(paste0(length(object), ' models were fitted to growth data.\n\n'))
  for (i in seq_along(object)) {
    cat(paste0('Summary of model ', i, ':\n'))
    summary(object[[i]])
    if (i < length(object)) {
      cat('\n\n')
    }
  }
}

#' @describeIn summary.growmod summarise fitted \code{"growmod_cv_multi"} models
#' @method summary growmod_cv_multi
#' @export
summary.growmod_cv_multi <- function(object, ...) {
  cat(paste0(length(object), ' models were validated with cross validation or a holdout data set.\n\n'))
  for (i in seq_along(object)) {
    cat(paste0('Summary of model ', i, ':\n'))
    summary(object[[i]])
    if (i < length(object)) {
      cat('\n\n')
    }
  }
}

# Print method for growmod objects
#' @export
print.growmod <- function(x, ...) {
  print(compare(x, ...))
}

#' @export
print.growmod_cv <- function(x, ...) {
  print(compare(x, ...))
}

#' @export
print.growmod_multi <- function(x, ...) {
  print(compare(x, ...))
}

#' @export
print.growmod_cv_multi <- function(x, ...) {
  print(compare(x, ...))
}

#' @export
fitted.growmod <- function(object, ...) {
  object$fitted
}

#' @export
fitted.growmod_cv <- function(object, ...) {
  object$size_pred
}

#' @export
fitted.growmod_multi <- function(object, ...) {
  out <- vector('list', length = length(object))
  for (i in seq_along(object)) {
    out[[i]] <- object[[i]]$fitted
  }
  out
}

#' @export
fitted.growmod_cv_multi <- function(object, ...) {
  out <- vector('list', length = length(object))
  for (i in seq_along(object)) {
    out[[i]] <- object[[i]]$size_pred
  }
  out
}

#' @export
residuals.growmod <- function(object, ...) {
  (object$data_set$size_data - object$fitted)
}

#' @export
residuals.growmod_cv <- function(object, ...) {
  (object$size_real - object$size_pred)
}

#' @export
residuals.growmod_multi <- function(object, ...) {
  out <- vector('list', length = length(object))
  for (i in seq_along(object)) {
    out[[i]] <- object[[i]]$data_set$size_data - object[[i]]$fitted
  }
  out
}

#' @export
residuals.growmod_cv_multi <- function(object, ...) {
  out <- vector('list', length = length(object))
  for (i in seq_along(object)) {
    out[[i]] <- object[[i]]$size_real - object[[i]]$size_pred
  }
  out
}

## PREDICT METHOD
# need three mod types: preds, blocks, noblocks
# default to fitted vals if no newdata provided
# newdata = list? Need to check newdata with fitted model
# make sure arguments are matched to generic
