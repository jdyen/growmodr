# prepare data sets for stan models
growmod_data <- function(data_set,
                         model,
                         num_params,
                         spline_params,
                         n_plot = 100) {
  if (model != 'spline') {
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
    offset_mod <- 0
    if ((model == 'power2') | (model == 'monod') | (model == 'neg_exp') | (model == 'koblog')) {
      offset_mod <- 0.1
    }
    if ((model == 'hillslope_log') | (model == 'expo') | (model == 'power3') | (model == 'weibull3')) {
      offset_mod <- 1
    }
    out <- with(data_set, list(n = length(size),
                               n_block = length(unique(block)),
                               size = size,
                               age = index + offset_mod,
                               block = as.integer(as.factor(block)),
                               n_plot = n_plot,
                               age_plot = seq(min(index) + offset_mod,
                                            max(index),
                                            length = n_plot)))
    for (i in 1:num_params) {
      append(out, get(paste0('x', i)))
      names(out)[length(out)] <- paste0('x', i)
      append(out, ncol(get(paste0('x', i))))
      names(out)[length(out)] <- paste0('n_x', i)
    }
  } else {
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
    n.int.knots <- spline_params$n_knots
    bs.order <- spline_params$degree + 1
    age_index <- match(data_set$index, sort(unique(data_set$index)))
    max.vals <- tapply(data_set$index, data_set$block, max)
    basis.func <- array(NA, dim = c(n_plot, length(unique(data_set$block)), (n.int.knots + bs.order - 1)))
    basis.func2 <- array(NA, dim = c(n_plot, length(unique(data_set$block)), (n.int.knots + bs.order - 1)))
    for (i in seq(along = unique(data_set$block))) {
      if (spline_params$spline_type = 'bspline') {
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
    out <- with(data_set, list(n = length(size),
                               n_block = length(unique(block)),
                               size = size,
                               age = index + offset_mod,
                               block = as.integer(as.factor(block)),
                               n_plot = n_plot,
                               age_plot = seq(min(index) + offset_mod,
                                              max(index),
                                              length = n_plot),
                               n_t = length(unique(t_index)),
                               n_k = (n.int.knots + bs.order - 1),
                               age_index = age_index,
                               b_spline = spline_out,
                               b_spline_plot = basis.func,
                               b_spline_deriv = basis.func2))
    for (i in 1:num_params) {
      append(out, get(paste0('x', i)))
      names(out)[length(out)] <- paste0('x', i)
      append(out, ncol(get(paste0('x', i))))
      names(out)[length(out)] <- paste0('n_x', i)
    }
  }
  out
}
