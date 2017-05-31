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
plot.grow_mod <- function(x, ...) {
  mod_tmp <- x$stan_summary
  data_tmp <- x$train_data
  plot_vals <- mod_tmp[grep('size_plot\\[', rownames(mod_tmp)), ]
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
