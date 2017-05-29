fetch_model_data <- function(data_set,
                             model,
                             all_traits = FALSE,
                             n.plot,
                             spline_params) {
  if (model != 'spline') {
    # ADD CHECKS FOR MISSING OR LACK OF PARAMS
    if (is.list(data_set$predictors)) {
      x_h <- cbind(rep(1, length(unique(data_set$block_id))),
                   data_set$predictors[[1]][row.match, ])
      x_a <- cbind(rep(1, length(unique(data_set$block_id))),
                   data_set$predictors[[2]][row.match, ])
      x_b <- cbind(rep(1, length(unique(data_set$block_id))),
                   data_set$predictors[[3]][row.match, ])
    } else {
      x_h <- cbind(rep(1, length(unique(data_set$block_id))),
                   data_set$predictors[row.match, ])
      x_a <- x_h
      x_b <- x_h  ### IF THREE PARAMS
    }
    offset.mod <- x_tmp$yr_offset
    if (!is.null(x_b)) {
      out <- with(data_set, list(n = length(size),
                                 n_sp = length(unique(block_id)),
                                 n_xh = ncol(x_h),
                                 n_xa = ncol(x_a),
                                 n_xb = ncol(x_b),
                                 h = size,
                                 t = age + offset.mod,
                                 sp = as.integer(as.factor(block_id)),
                                 x_h = x_h,
                                 x_a = x_a,
                                 x_b = x_b,
                                 n_plot = n.plot,
                                 t_plot = seq(min(age) + offset.mod, max(age), length = n.plot)))
    } else {
      out <- with(data_set, list(n = length(size),
                                 n_sp = length(unique(block_id)),
                                 n_xh = ncol(x_h),
                                 n_xa = ncol(x_a),
                                 h = size,
                                 t = age + offset.mod,
                                 sp = as.integer(as.factor(block_id)),
                                 x_h = x_h,
                                 x_a = x_a,
                                 n_plot = n.plot,
                                 t_plot = seq(min(age) + offset.mod, max(age), length = n.plot)))
    }
  } else {
    row.match <- match(unique(data_set$block_id), data_set$block_id)
    ## CATCH FOR LIST PREDICTORS RATHER THAN MATRIX
    x <- cbind(rep(1, length(unique(data_set$block_id))),
               data_set$predictors[row.match, ])
    n.int.knots <- spline_params$n_knots
    bs.order <- spline_params$degree + 1
    t_index <- match(data_set$age, sort(unique(data_set$age)))
    max.vals <- tapply(data_set$age, data_set$block_id, max)
    basis.func <- array(NA, dim = c(n.plot, length(unique(data_set$block_id)), (n.int.knots + bs.order - 1)))
    basis.func2 <- array(NA, dim = c(n.plot, length(unique(data_set$block_id)), (n.int.knots + bs.order - 1)))
    for (i in seq(along = unique(data_set$block_id))) {
      if (spline_params$spline_type = 'bspline') {
        if (n.int.knots > 0) {
          knots_set <- sort(c(rep(c(min(data_set$age), max(data_set$age)), bs.order),
                              quantile(seq(min(data_set$age), max(data_set$age), length = 100),
                                       exp(seq.int(from = log(0.001), to = log(1),
                                                   length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
        } else {
          knots_set <- sort(c(rep(c(min(data_set$age), max(data_set$age)), bs.order)))
        }
        basis.func[, i, ] <- splineDesign(knots_set,
                                          x = seq(min(data_set$age), max.vals[i], length = n.plot),
                                          ord = bs.order,
                                          derivs = 0)[, -1, drop = FALSE]
        basis.func2[, i, ] <- splineDesign(knots_set,
                                           x = seq(min(data_set$age), max.vals[i], length = 100),
                                           ord = bs.order,
                                           derivs = 1)[, -1, drop = FALSE]
      } else {
        if (n.int.knots > 0) {
          knots_set <- quantile(seq(min(data_set$age), max(data_set$age), length = 100),
                                exp(seq.int(from = log(0.001), to = log(1),
                                            length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
        } else {
          knots_set <- NULL
        }
        basis.func[, i, ] <- splines2::iSpline(seq(min(data_set$age), max.vals[i], length = 100),
                                               knots = knots_set,
                                               degree = (bs.order - 1),
                                               Boundary.knots = c(min(data_set$age), max(data_set$age)))
        basis.func2[, i, ] <- splines2::iSpline(seq(min(data_set$age), max.vals[i], length = 100),
                                                knots = knots_set,
                                                degree = (bs.order - 1),
                                                Boundary.knots = c(min(data_set$age), max(data_set$age)),
                                                derivs = 1)
      }
    }
    if (spline_params$spline_type == 'bspline') {
      if (n.int.knots > 0) {
        knots_set <- sort(c(rep(c(min(data_set$age), max(data_set$age)), bs.order),
                            quantile(seq(min(data_set$age), max(data_set$age), length = 100),
                                     exp(seq.int(from = log(0.001), to = log(1),
                                                 length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
      } else {
        knots_set <- sort(c(rep(c(min(data_set$age), max(data_set$age)), bs.order)))
      }
      spline_out <- splineDesign(knots_set,
                                 x = sort(unique(data_set$age)),
                                 ord = bs.order,
                                 derivs = 0)[, -1, drop = FALSE]
    } else {
      if (n.int.knots > 0) {
        knots_set <- quantile(seq(min(data_set$age), max(data_set$age), length = 100),
                              exp(seq.int(from = log(0.001), to = log(1),
                                          length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
      } else {
        knots_set <- NULL
      }
      spline_out <- splines2::iSpline(sort(unique(data_set$age)),
                                      knots = knots_set,
                                      degree = (bs.order - 1),
                                      Boundary.knots = c(min(data_set$age), max(data_set$age)))
    }
    out <- with(data_set, list(n = length(size),
                               n_sp = length(unique(block_id)),
                               n_x = ncol(x),
                               h = size,
                               t = age,
                               n_t = length(unique(t_index)),
                               n_k = (n.int.knots + bs.order - 1),
                               t_index = t_index,
                               b_spline = spline_out,
                               sp = as.integer(as.factor(block_id)),
                               x = x,
                               n_plot = n.plot,
                               t_plot = seq(min(age), max(age), length = n.plot),
                               b_spline_plot = basis.func,
                               b_spline_deriv = basis.func2))
  }
  out
}


# Fix up names in this or possibly delete -- just needs to be in a list in the inputs
fetch_trait_list <- function(data_set, model, all_traits = FALSE) {
  row.match <- match(unique(data_set$block_id), data_set$block_id)
  if (!all_traits) {
    if ((model == 'hillslope') | (model == 'hillslope_log')) {
      x_h <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$SD[row.match],
                   data_set$SLA[row.match])
      x_a <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$SD_MASS[row.match])
      x_b <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match],
                   data_set$SD_MASS[row.match])
    }
    if ((model == 'power2')) {
      x_h <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$SD[row.match],
                   data_set$SLA[row.match])
      x_a <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match],
                   data_set$SD_MASS[row.match])
      x_b <- NULL
    }
    if ((model == 'expo') | (model == 'monod') | (model == 'neg_exp') | (model == 'koblog')) {
      x_h <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$SLA[row.match])
      x_a <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match])
      x_b <- NULL
    }
    if ((model == 'power3') | (model == 'logistic3') | (model == 'archibold') | (model == 'weibull3')) {
      x_h <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$SLA[row.match])
      x_a <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$SD_MASS[row.match])
      x_b <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$SD_MASS[row.match])
    }
  } else {
    if ((model == 'power2') | (model == 'expo') | (model == 'monod') | (model == 'neg_exp') | (model == 'koblog')) {
      x_h <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match],
                   data_set$SD_MASS[row.match],
                   data_set$SD[row.match],
                   data_set$SLA[row.match])
      x_a <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match],
                   data_set$SD_MASS[row.match],
                   data_set$SD[row.match],
                   data_set$SLA[row.match])
      x_b <- NULL
    }
    if ((model == 'hillslope') | (model == 'hillslope_log') | (model == 'power3') | (model == 'logistic3') | (model == 'archibold') | (model == 'weibull3')) {
      x_h <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match],
                   data_set$SD_MASS[row.match],
                   data_set$SD[row.match],
                   data_set$SLA[row.match])
      x_a <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match],
                   data_set$SD_MASS[row.match],
                   data_set$SD[row.match],
                   data_set$SLA[row.match])
      x_b <- cbind(rep(1, length(unique(data_set$SPP))),
                   data_set$N[row.match],
                   data_set$SD_MASS[row.match],
                   data_set$SD[row.match],
                   data_set$SLA[row.match])
    }
  }
  if ((model == 'hillslope') | (model == 'archibold') | (model == 'logistic3')) {
    yr_offset <- 0
  }
  if ((model == 'power2') | (model == 'monod') | (model == 'neg_exp') | (model == 'koblog')) {
    yr_offset <- 0.1
  }
  if ((model == 'hillslope_log') | (model == 'expo') | (model == 'power3') | (model == 'weibull3')) {
    yr_offset <- 1
  }
  return(list(x_h = x_h, x_a = x_a, x_b = x_b, yr_offset = yr_offset))
}