# prepare data sets for stan models
growmod_data <- function(data_set,
                         model,
                         num_params,
                         spline_params,
                         n_plot = 100,
                         test_data = NULL) {
  if (model != 'spline') {
    if (!is.null(data_set$predictors)) {
      if (is.data.frame(data_set$predictors) | is.matrix(data_set$predictors)) {
        for (i in 1:num_params) {
          assign(paste0('x', i), cbind(rep(1, nrow(data_set$predictors)),
                                       data_set$predictors))
        }
      } else {
        for (i in 1:num_params) {
          assign(paste0('x', i), cbind(rep(1, nrow(data_set$predictors[[i]])),
                                       data_set$predictors[[i]]))
        }
      }
    }
    offset_mod <- 0
    if ((model == 'power2') | (model == 'monod') | (model == 'neg_exp') | (model == 'koblog')) {
      offset_mod <- 0.1
    }
    if ((model == 'hillslope_log') | (model == 'expo') | (model == 'power3') | (model == 'weibull3')) {
      offset_mod <- 1
    }
    out <- with(data_set, list(n = length(size),
                               n_block = length(unique(block)),
                               size_data = size,
                               age = index + offset_mod,
                               block_data = as.integer(as.factor(block)),
                               n_plot = n_plot,
                               age_plot = seq(min(index) + offset_mod,
                                              max(index),
                                              length = n_plot)))
    if (!is.null(data_set$predictors)) {
      for (i in 1:num_params) {
        out <- append(out, list(get(paste0('x', i))))
        names(out)[length(out)] <- paste0('x', i)
        out <- append(out, list(ncol(get(paste0('x', i)))))
        names(out)[length(out)] <- paste0('n_x', i)
      }
    }
    if (is.null(test_data)) {
      out$age_holdout <- rnorm(5)
      out$block_holdout <- rep(1, out$n_pred)
      if (!is.null(data_set$predictors)) {
        for (i in 1:num_params) {
          out <- append(out, list(get(paste0('x', i))[1:length(out$age_holdout), ]))
          names(out)[length(out)] <- paste0('x', i, '_pred')
        }        
      }
    } else {
      out$age_holdout <- test_data$age
      out$block_holdout <- test_data$block_holdout
      if (!is.null(data_set$predictors)) {
        if (is.data.frame(test_data$predictors) | is.matrix(test_data$predictors)) {
          for (i in 1:num_params) {
            assign(paste0('x', i, '_pred'), cbind(rep(1, nrow(test_data$predictors)),
                                                  test_data$predictors))
          }
        } else {
          for (i in 1:num_params) {
            assign(paste0('x', i, '_pred'), cbind(rep(1, nrow(test_data$predictors[[i]])),
                                                  test_data$predictors[[i]]))
          }
        }
        for (i in 1:num_params) {
          out <- append(out, list(get(paste0('x', i, '_pred'))))
          names(out)[length(out)] <- paste0('x', i, '_pred')
        }        
      }
    }
    out$n_pred <- length(out$age_holdout)
  } else {
    if (!is.null(data_set$predictors)) {
      if (is.data.frame(data_set$predictors) | is.matrix(data_set$predictors)) {
        for (i in 1:num_params) {
          assign(paste0('x', i), cbind(rep(1, nrow(data_set$predictors)),
                                       data_set$predictors))
        }
      } else {
        warning('you have assigned separate predictors for each spline parameter;
              this is an unwieldy model and is not recommended.',
                call. = FALSE)
        for (i in 1:num_params) {
          assign(paste0('x', i), cbind(rep(1, nrow(data_set$predictors[[i]])),
                                       data_set$predictors[[i]]))
        }
      }
    }
    n.int.knots <- spline_params$n_knots
    bs.order <- spline_params$degree + 1
    age_index <- match(data_set$index, sort(unique(data_set$index)))
    if (!is.null(data_set$block)) {
      max.vals <- tapply(data_set$index, data_set$block, max)
      basis.func <- array(NA, dim = c(n_plot, length(unique(data_set$block)), (n.int.knots + bs.order - 1)))
      basis.func2 <- array(NA, dim = c(n_plot, length(unique(data_set$block)), (n.int.knots + bs.order - 1)))
      for (i in seq(along = unique(data_set$block))) {
        if (spline_params$spline_type == 'bspline') {
          if (n.int.knots > 0) {
            knots_set <- sort(c(rep(c(min(data_set$index), max(data_set$index)), bs.order),
                                quantile(seq(min(data_set$index), max(data_set$index), length = 100),
                                         exp(seq.int(from = log(0.001), to = log(1),
                                                     length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
          } else {
            knots_set <- sort(c(rep(c(min(data_set$index), max(data_set$index)), bs.order)))
          }
          basis.func[, i, ] <- splineDesign(knots_set,
                                            x = seq(min(data_set$index), max.vals[i], length = n_plot),
                                            ord = bs.order,
                                            derivs = 0)[, -1, drop = FALSE]
          basis.func2[, i, ] <- splineDesign(knots_set,
                                             x = seq(min(data_set$index), max.vals[i], length = 100),
                                             ord = bs.order,
                                             derivs = 1)[, -1, drop = FALSE]
        } else {
          if (spline_params$spline_type != 'ispline') {
            warning(paste0(spline_params$spline_type, ' is not a known spline model; ispline used by default.'),
                    call. = FALSE)
          }
          if (n.int.knots > 0) {
            knots_set <- quantile(seq(min(data_set$index), max(data_set$index), length = 100),
                                  exp(seq.int(from = log(0.001), to = log(1),
                                              length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
          } else {
            knots_set <- NULL
          }
          basis.func[, i, ] <- splines2::iSpline(seq(min(data_set$index), max.vals[i], length = n_plot),
                                                 knots = knots_set,
                                                 degree = (bs.order - 1),
                                                 Boundary.knots = c(min(data_set$index), max(data_set$index)))
          basis.func2[, i, ] <- splines2::iSpline(seq(min(data_set$index), max.vals[i], length = 100),
                                                  knots = knots_set,
                                                  degree = (bs.order - 1),
                                                  Boundary.knots = c(min(data_set$index), max(data_set$index)),
                                                  derivs = 1)
        }
      }
    } else {
      max.vals <- max(data_set$index)
      basis.func <- array(NA, dim = c(n_plot, (n.int.knots + bs.order - 1)))
      basis.func2 <- array(NA, dim = c(n_plot, (n.int.knots + bs.order - 1)))
      if (spline_params$spline_type == 'bspline') {
        if (n.int.knots > 0) {
          knots_set <- sort(c(rep(c(min(data_set$index), max(data_set$index)), bs.order),
                              quantile(seq(min(data_set$index), max(data_set$index), length = 100),
                                       exp(seq.int(from = log(0.001), to = log(1),
                                                   length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
        } else {
          knots_set <- sort(c(rep(c(min(data_set$index), max(data_set$index)), bs.order)))
        }
        basis.func <- splineDesign(knots_set,
                                   x = seq(min(data_set$index), max.vals, length = n_plot),
                                   ord = bs.order,
                                   derivs = 0)[, -1, drop = FALSE]
        basis.func2 <- splineDesign(knots_set,
                                    x = seq(min(data_set$index), max.vals, length = 100),
                                    ord = bs.order,
                                    derivs = 1)[, -1, drop = FALSE]
      } else {
        if (spline_params$spline_type != 'ispline') {
          warning(paste0(spline_params$spline_type, ' is not a known spline model; ispline used by default.'),
                  call. = FALSE)
        }
        if (n.int.knots > 0) {
          knots_set <- quantile(seq(min(data_set$index), max(data_set$index), length = 100),
                                exp(seq.int(from = log(0.001), to = log(1),
                                            length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
        } else {
          knots_set <- NULL
        }
        basis.func <- splines2::iSpline(seq(min(data_set$index), max.vals, length = n_plot),
                                        knots = knots_set,
                                        degree = (bs.order - 1),
                                        Boundary.knots = c(min(data_set$index), max(data_set$index)))
        basis.func2 <- splines2::iSpline(seq(min(data_set$index), max.vals, length = 100),
                                         knots = knots_set,
                                         degree = (bs.order - 1),
                                         Boundary.knots = c(min(data_set$index), max(data_set$index)),
                                         derivs = 1)
      }
    }
    
    if (spline_params$spline_type == 'bspline') {
      if (n.int.knots > 0) {
        knots_set <- sort(c(rep(c(min(data_set$index), max(data_set$index)), bs.order),
                            quantile(seq(min(data_set$index), max(data_set$index), length = 100),
                                     exp(seq.int(from = log(0.001), to = log(1),
                                                 length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
      } else {
        knots_set <- sort(c(rep(c(min(data_set$index), max(data_set$index)), bs.order)))
      }
      spline_out <- splineDesign(knots_set,
                                 x = sort(unique(data_set$index)),
                                 ord = bs.order,
                                 derivs = 0)[, -1, drop = FALSE]
    } else {
      if (n.int.knots > 0) {
        knots_set <- quantile(seq(min(data_set$index), max(data_set$index), length = 100),
                              exp(seq.int(from = log(0.001), to = log(1),
                                          length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
      } else {
        knots_set <- NULL
      }
      spline_out <- splines2::iSpline(sort(unique(data_set$index)),
                                      knots = knots_set,
                                      degree = (bs.order - 1),
                                      Boundary.knots = c(min(data_set$index), max(data_set$index)))
    }
    offset_mod <- 0.0
    out <- with(data_set, list(n = length(size),
                               n_block = length(unique(block)),
                               size_data = size,
                               age = index + offset_mod,
                               block_data = as.integer(as.factor(block)),
                               n_plot = n_plot,
                               age_plot = seq(min(index) + offset_mod,
                                              max(index),
                                              length = n_plot),
                               n_age = length(unique(age_index)),
                               n_k = (n.int.knots + bs.order - 1),
                               age_index = age_index,
                               b_spline = spline_out,
                               b_spline_plot = basis.func,
                               b_spline_deriv = basis.func2))
    if (!is.null(data_set$predictors)) {
      for (i in 1:num_params) {
        out <- append(out, list(get(paste0('x', i))))
        names(out)[length(out)] <- paste0('x', i)
        out <- append(out, list(ncol(get(paste0('x', i)))))
        names(out)[length(out)] <- paste0('n_x', i)
      }
    }
  }
  out
}
