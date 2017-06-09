#' @rdname growmod
#' @export
compare <- function(x, ...) {
  UseMethod('compare')
}

#' @rdname growmod
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

#' @rdname growmod
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

#' @rdname growmod
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

#' @rdname growmod
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

#' @rdname growmod
#' @export
plot.growmod <- function(x, ...) {
  mod_tmp <- x$stan_summary
  data_tmp <- x$data_set
  for (i in 1:data_tmp$n_block) {
    row_id <- grep(paste0('plot\\[[[:digit:]]*,', i, '\\]'), rownames(mod_tmp))
    plot_data <- mod_tmp[row_id, c('mean', '2.5%', '50%', '97.5%')]
    x_plot <- data_tmp$age_plot
    y_lims <- c(min(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)])),
                max(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)])))
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
}

#' @rdname growmod
#' @export
plot.growmod_cv <- function(x, ...) {
  min_val <- min(0, min(x$size_real), min(x$size_pred))
  max_val <- max(max(x$size_real), max(x$size_pred))
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

#' @rdname growmod
#' @export
plot.growmod_multi <- function(x, group_blocks = TRUE, ...) {
  old_mfrow <- par()$mfrow
  if (group_blocks) {
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
        x_plot <- data_tmp$age_plot
        y_lims <- c(min(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)])),
                    max(c(plot_data, data_tmp$size_data[which(data_tmp$block_data == i)])))
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
    num_plots <- x[[1]]$data_set$n_block
    for (i in seq_along(x)) {
      par(mfrow = c(round(sqrt(num_plots)), ceiling(num_plots / round(sqrt(num_plots)))))
      plot(x[[i]])
    }
  }
  par(mfrow = old_mfrow)  
}

#' @rdname growmod
#' @export
plot.growmod_cv_multi <- function(x, ...) {
  old_mfrow <- par()$mfrow
  num_plots <- length(x)
  par(mfrow = c(round(sqrt(num_plots)), ceiling(num_plots / round(sqrt(num_plots)))))
  for (i in seq_along(x)) {
    plot(x[[i]])
  }
  par(mfrow = old_mfrow)  
}

#' @rdname growmod
#' @export
summary.growmod <- function(x, ...) {
  print_out <- c(x$r2, x$rmsd, x$md, round(x$loo$looic, 2), round(x$waic$waic, 2))
  names(print_out) <- c('r2', 'rmsd', 'md', 'loo_ic', 'waic')
  mod_type <- x$model
  ## pull out rhats and print warning if some >1.1
  ## state number of obs, number of blocks, return model call (perhaps formatted)
}

#' @rdname growmod
#' @export
summary.growmod_cv <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
summary.growmod_multi <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
summary.growmod_cv_multi <- function(x, ...) {
  
}

#' @rdname growmod
#' @export
print.growmod <- function(x, ...) {
  compare(x, ...)
}

#' @rdname growmod
#' @export
print.growmod_cv <- function(x, ...) {
  compare(x, ...)
}

#' @rdname growmod
#' @export
print.growmod_multi <- function(x, ...) {
  compare(x, ...)
}

#' @rdname growmod
#' @export
print.growmod_cv_multi <- function(x, ...) {
  compare(x, ...)
}

#' @rdname growmod
#' @export
predict.growmod <- function(object, newdata, ...) {
  # switch for methods with and without blocks and predictors
  NULL  
}
