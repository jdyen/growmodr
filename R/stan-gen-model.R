#' Fit growth model to data on sizes through time
#' 
#' Fit a Stan model to data on sizes through time
#' 
#' @param data list containing data on size, age, species, and traits
#' @param model growth model form to be fitted
#' @param save_loglik logical indicating whether to record loglik for LOO and WAIC calculations
#' @param n.iter number of HMC iterations to run for stan model
#' @param n.thin thinning rate for HMC chains
#' @param n.chains number of HMC chains
#' @param spline_params named list of settings for spline model (degree, n_knots, use_b_spline)
#' @param ... parameters to be passed to stan model call
#' 
#' @return stan_model fitted stan model object
#' 
stan_growth_model <- function(data, model,
                              save_loglik = FALSE,
                              n.iter = 10000,
                              n.burnin = round(n.iter / 2),
                              n.thin = max(1, floor(n.iter / 10000)),
                              n.chains = 4,
                              spline_params = list(degree = 8, n_knots = 10, use_b_spline = FALSE),
                              ...) {
  mod.file <- gen_mod_file(model)
  data.set <- fetch_model_data(data, model = model, all_traits = all_traits, n.plot = n.plot,
                               spline_params = spline_params)
  if (is.null(inits)) {
    inits <- '0'
  }
  if (is.null(pars)) {
    pars <- c('mu', 'h_plot', 'h_plot_agr')
    if ((model == 'hillslope') | (model == 'hillslope_log') | (model == 'power3') | (model == 'logistic3') | (model == 'archibold') | (model == 'weibull3')) {
      pars <- c(pars, 'hm', 'a', 'b', 'b0', 'b1', 'b2')
    } else {
      if ((model == 'power2') | (model == 'expo') | (model == 'monod') | (model == 'neg_exp') | (model == 'koblog')) {
        pars <- c(pars, 'hm', 'a', 'b0', 'b1')
      } else {
        pars <- c(pars, 'beta', 'b_param')
      }
    }
  }
  if (validate) {
    pars <- c(pars, 'log_lik')
  }
  if (is.null(control)) {
    control = list(adapt_delta = 0.99)
  }
  mod <- stan(file = mod.file,
              data = data.set,
              iter = n.iter,
              pars = pars,
              chains = n.chains,
              init = inits,
              seed = seed,
              control = control,
              cores = cores, ...)
  if (validate) {
    log_lik1 <- extract_log_lik(mod)
    loo <- loo(log_lik1)
    waic <- waic(log_lik1)
    fitted.vals.tmp <- exp(get_posterior_mean(mod, pars = 'mu'))
    fitted.vals <- fitted.vals.tmp[, ncol(fitted.vals.tmp)]
    r2 <- cor(fitted.vals, data.set$h) ** 2
    rmsd <- sqrt(mean((fitted.vals - data.set$h) ** 2))
    md <- mean((fitted.vals - data.set$h))
  } else {
    loo <- NULL
    waic <- NULL
    fitted.vals.tmp <- exp(get_posterior_mean(mod, pars = 'mu'))
    fitted.vals <- fitted.vals.tmp[, ncol(fitted.vals.tmp)]
    r2 <- cor(fitted.vals, data.set$h) ** 2
    rmsd <- sqrt(mean((fitted.vals - data.set$h) ** 2))
    md <- mean((fitted.vals - data.set$h))
  }
  return(list(stan_model = mod, data_set = data.set,
              loo = loo, waic = waic,
              r2 = r2, rmsd = rmsd, md = md))
}

stan_growth_mod_int <- function(i,
                                data,
                                mod.list,
                                all_traits,
                                validate = TRUE,
                                n.iter = 1000,
                                n.chains = 4,
                                spline_params = list(degree = 8, n_knots = 10)) {
  out <- stan_growth_model(data,
                           model = mod.list[i],
                           all_traits = all_traits[i],
                           validate = validate,
                           n.iter = n.iter,
                           n.chains = n.chains,
                           spline_params = spline_params)
  out
}

stan_model_compare <- function(mod1, mod2) {
  if (class(mod1) == 'stanfit') {
    log_lik1 <- extract_log_lik(mod1)
    loo1 <- loo(log_lik1)
  } else {
    loo1 <- mod1$loo
  }
  if (class(mod2) == 'stanfit') {
    log_lik2 <- extract_log_lik(mod2)
    loo2 <- loo(log_lik2)
  } else {
    loo2 <- mod2$loo
  }
  compare(loo1, loo2)
}

gen_mod_file <- function(model) {
  if (model != 'spline') {
  mod.params <- get(paste0(model, '_param_fetch'))()
  three.par <- mod.params$three.par
  mod.mu <- mod.params$mu
  mu.plot <- mod.params$mu.plot
  mu.plot.agr <- mod.params$mu.agr
  cat(
  'data {
    int<lower=0> n;
    int<lower=0> n_sp;
    int<lower=0> n_xh;
    int<lower=0> n_xa;
    vector<lower=0>[n] h;
    vector[n] t;
    int<lower=0, upper=n_sp> sp[n];
    matrix[n_sp, n_xh] x_h;
    matrix[n_sp, n_xa] x_a;\n',
    if (three.par) {
      ' int<lower=0> n_xb;\n'
    },
    if (three.par) {
      ' matrix[n_sp, n_xb] x_b;\n'
    },
    ' int<lower=0> n_plot;
    vector[n_plot] t_plot;
  }

  parameters {
    real<lower=0> sigma_obs;
    real<lower=0> sd_h;
    real<lower=0> sd_a;\n',
    if (three.par) {
      ' real<lower=0> sd_b;\n'
    },
    if (three.par) {
      ' vector[n_xb] b2;\n'
    },
    if (three.par) {
      ' vector<lower=0>[n_sp] b;\n'
    },
    ' vector<lower=0>[n_sp] hm;
    vector<lower=0>[n_sp] a;
    vector[n_xh] b0;
    vector[n_xa] b1;
  }

  transformed parameters {
    vector[n] mu;
    vector[n_sp] psi_h;
    vector[n_sp] psi_a;\n',
    if (three.par) {
      ' vector[n_sp] psi_b;\n'
    },
    ' for (i in 1:n)
      mu[i] = ', mod.mu, ';\n',
    ' psi_h = x_h * b0;
    psi_a = x_a * b1;\n',
    if (three.par) {
      ' psi_b = x_b * b2;\n'
    },
  '}

  model {
    log(h) ~ normal(mu, sigma_obs);
    hm ~ normal(psi_h, sd_h);
    a ~ normal(psi_a, sd_a);\n',
    if (three.par) {
      ' b ~ normal(psi_b, sd_b);\n'
    },
    if (three.par) {
      ' b2 ~ normal(0, 10);\n'
    },
    if (three.par) {
      ' sd_b ~ normal(0, 2);\n'
    },
    ' b0 ~ normal(0, 10);
    b1 ~ normal(0, 10);
    sd_h ~ normal(0, 2);
    sd_a ~ normal(0, 2);
  }

  generated quantities {
    matrix[n_plot, n_sp] mu_plot_growth;
    matrix[n_plot, n_sp] mu_plot_agr;
    matrix<lower=0>[n_plot, n_sp] h_plot;
    matrix<lower=0>[n_plot, n_sp] h_plot_agr;
    vector[n] log_lik;
  
    for (i in 1:n_plot)
      for (j in 1:n_sp)
        mu_plot_growth[i, j] = ', mu.plot, ';\n',
    ' for (i in 1:n_plot)
      for (j in 1:n_sp)
        h_plot[i, j] = exp(mu_plot_growth[i, j]);
    for (i in 1:n_plot)
      for (j in 1:n_sp)
        mu_plot_agr[i, j] = ', mu.plot.agr, ';\n',
    ' for (i in 1:n_plot)
      for (j in 1:n_sp)
        h_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
    for (i in 1:n)
      log_lik[i] = normal_lpdf(log(h[i]) | mu[i], sigma_obs);
  }',
  file = (mod.file <- tempfile()))
  } else {
  cat(
  'data {
    int<lower=0> n;
    int<lower=0> n_sp;
    int<lower=0> n_x;
    vector<lower=0>[n] h;
    vector[n] t;
    int<lower=0> n_t;
    int<lower=0> n_k;
    int<lower=0> t_index[n];
    int<lower=0, upper=n_sp> sp[n];
    matrix[n_sp, n_x] x;
    row_vector[n_k] b_spline[n_t];
    int<lower=0> n_plot;
    vector[n_plot] t_plot;
    row_vector[n_k] b_spline_plot[n_plot, n_sp];
    row_vector[n_k] b_spline_deriv[n_plot, n_sp];
  }
      
  parameters {
    real<lower=0> sigma_obs;
    vector<lower=0>[n_k] sd_param;
    vector[n_k] beta[n_sp];
    matrix[n_x, n_k] b_param;
  }
      
  transformed parameters {
    vector[n] mu;
    matrix[n_sp, n_k] psi;
    for (i in 1:n)
      mu[i] = b_spline[t_index[i]] * beta[sp[i]];
    psi = x * b_param;
  }
      
  model {
    log(h) ~ normal(mu, sigma_obs);
    for (j in 1:n_sp)
      beta[j] ~ normal(psi[j], sd_param);
    for (j in 1:n_x)
      b_param[j] ~ normal(0, 1);
    sd_param ~ normal(0, 2);
    sigma_obs ~ normal(0, 2);
  }
      
  generated quantities {
    matrix[n_plot, n_sp] mu_plot_growth;
    matrix[n_plot, n_sp] mu_plot_agr;
    matrix<lower=0>[n_plot, n_sp] h_plot;
    matrix<lower=0>[n_plot, n_sp] h_plot_agr;
    vector[n] log_lik;
      
    for (i in 1:n_plot)
      for (j in 1:n_sp)
        mu_plot_growth[i, j] = b_spline_plot[i, j] * beta[j];
    for (i in 1:n_plot)
      for (j in 1:n_sp)
        h_plot[i, j] = exp(mu_plot_growth[i, j]);
    for (i in 1:n_plot)
      for (j in 1:n_sp)
        mu_plot_agr[i, j] = b_spline_deriv[i, j] * beta[j];
    for (i in 1:n_plot)
      for (j in 1:n_sp)
        h_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
    for (i in 1:n)
      log_lik[i] = normal_lpdf(log(h[i]) | mu[i], sigma_obs);
  }\n',
  file = (mod.file <- tempfile()))
  }
  mod.file
}

hillslope_param_fetch <- function() {
  three.par <- TRUE
  mu <- 'hm[sp[i]] / (1 + exp(-a[sp[i]] * (t[i] - b[sp[i]])))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] / (1 + exp(-a_holdout[sp_holdout[i]] * (t_holdout[i] - b_holdout[sp_holdout[i]])))'
  mu.plot <- 'hm[j] / (1 + exp(-a[j] * (t_plot[i] - b[j])))'
  mu.agr <- 'mu_plot_growth[i, j] * (a[j] * (exp(-a[j] * (t_plot[i] - b[j]))) / (1 + exp(-a[j] * (t_plot[i] - b[j]))))'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

hillslope_log_param_fetch <- function() {
  three.par <- TRUE
  mu <- 'hm[sp[i]] / (1 + exp(-a[sp[i]] * (log(t[i]) - b[sp[i]])))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] / (1 + exp(-a_holdout[sp_holdout[i]] * (log(t_holdout[i]) - b_holdout[sp_holdout[i]])))'
  mu.plot <- 'hm[j] / (1 + exp(-a[j] * (log(t_plot[i]) - b[j])))'
  mu.agr <- 'mu_plot_growth[i, j] * (a[j] * (exp(-a[j] * (log(t_plot[i]) - b[j]))) / (1 + exp(-a[j] * (log(t_plot[i]) - b[j]))))'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

power2_param_fetch <- function() {
  three.par <- FALSE
  mu <- 'hm[sp[i]] * (t[i] ^ a[sp[i]])'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] * (t_holdout[i] ^ a_holdout[sp_holdout[i]])'
  mu.plot <- 'hm[j] * (t_plot[i] ^ a[j])'
  mu.agr <- 'hm[j] * a[j] * (t_plot[i] ^ (a[j] - 1))'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

expo_param_fetch <- function() {
  three.par <- FALSE
  mu <- 'hm[sp[i]] + (a[sp[i]] * log(t[i]))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] + (a_holdout[sp_holdout[i]] * log(t_holdout[i]))'
  mu.plot <- 'hm[j] + (a[j] * log(t_plot[i]))'
  mu.agr <- 'a[j] / t_plot[i]'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

monod_param_fetch <- function() {
  three.par <- FALSE
  mu <- 'hm[sp[i]] * (t[i] / (a[sp[i]] + t[i]))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] * (t_holdout[i] / (a_holdout[sp_holdout[i]] + t_holdout[i]))'
  mu.plot <- 'hm[j] * (t_plot[i] / (a[j] + t_plot[i]))'
  mu.agr <- 'hm[j] * (a[j] / ((a[j] + t_plot[i]) ^ 2))'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

neg_exp_param_fetch <- function() {
  three.par <- FALSE
  mu <- 'hm[sp[i]] * (1 - exp(-a[sp[i]] * t[i]))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] * (1 - exp(-a_holdout[sp_holdout[i]] * t_holdout[i]))'
  mu.plot <- 'hm[j] * (1 - exp(-a[j] * t_plot[i]))'
  mu.agr <- 'hm[j] * a[j] * exp(-a[j] * t_plot[i])'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

koblog_param_fetch <- function() {
  three.par <- FALSE
  mu <- 'hm[sp[i]] * log(1 + (t[i] / a[sp[i]]))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] * log(1 + (t_holdout[i] / a_holdout[sp_holdout[i]]))'
  mu.plot <- 'hm[j] * log(1 + (t_plot[i] / a[j]))'
  mu.agr <- 'hm[j] / (a[j] + t_plot[i])'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

power3_param_fetch <- function() {
  three.par <- TRUE
  mu <- 'hm[sp[i]] * (t[i] ^ (a[sp[i]] - (b[sp[i]] / t[i])))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] * (t_holdout[i] ^ (a_holdout[sp_holdout[i]] - (b_holdout[sp_holdout[i]] / t_holdout[i])))'
  mu.plot <- 'hm[j] * (t_plot[i] ^ (a[j] - (b[j] / t_plot[i])))'
  mu.agr <- 'mu_plot_growth[i, j] * (((a[j] - (b[j] / t_plot[i])) / t_plot[i]) + (b[j] * log(t_plot[i])) / (t_plot[i] ^ 2))'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

logistic3_param_fetch <- function() {
  three.par <- TRUE
  mu <- 'hm[sp[i]] / (1 + exp(-a[sp[i]] * t[i] + b[sp[i]]))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] / (1 + exp(-a_holdout[sp_holdout[i]] * t_holdout[i] + b_holdout[sp_holdout[i]]))'
  mu.plot <- 'hm[j] / (1 + exp(-a[j] * t_plot[i] + b[j]))'
  mu.agr <- 'a[j] * hm[j] / (2 * cosh(b[j] - a[j] * t_plot[i]) + 2)'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

archibold_param_fetch <- function() {
  three.par <- TRUE
  mu <- 'hm[sp[i]] / (a[sp[i]] + (b[sp[i]] ^ t[i]))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] / (a_holdout[sp_holdout[i]] + (b_holdout[sp_holdout[i]] ^ t_holdout[i]))'
  mu.plot <- 'hm[j] / (a[j] + (b[j] ^ t_plot[i]))'
  mu.agr <- '-hm[j] * (b[j] ^ t_plot[i]) * log(b[j]) / ((a[j] + (b[j] ^ t_plot[i])) ^ 2)'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

weibull3_param_fetch <- function() {
  three.par <- TRUE
  mu <- 'hm[sp[i]] * (1 - exp(-a[sp[i]] * (t[i] ^ b[sp[i]])))'
  mu.holdout <- 'hm_holdout[sp_holdout[i]] * (1 - exp(-a_holdout[sp_holdout[i]] * (t_holdout[i] ^ b_holdout[sp_holdout[i]])))'
  mu.plot <- 'hm[j] * (1 - exp(-a[j] * (t_plot[i] ^ b[j])))'
  mu.agr <- 'hm[j] * a[j] * b[j] * (t_plot[i] ^ (b[j] - 1)) * exp(-a[j] * (t_plot[i] ^ b[j]))'
  return(list(three.par = three.par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu.plot = mu.plot,
              mu.agr = mu.agr))
}

fetch_model_data <- function(data_set, model, all_traits = FALSE, n.plot,
                             spline_params) {
  if (model != 'spline') {
    x_tmp <- fetch_trait_list(data_set = data_set, model = model, all_traits = all_traits)
    x_h <- x_tmp$x_h
    x_a <- x_tmp$x_a
    x_b <- x_tmp$x_b
    offset.mod <- x_tmp$yr_offset
    if (!is.null(x_b)) {
      out <- with(data_set, list(n = length(SPP),
                                 n_sp = length(unique(SPP)),
                                 n_xh = ncol(x_h),
                                 n_xa = ncol(x_a),
                                 n_xb = ncol(x_b),
                                 h = Ht,
                                 t = Yrs + offset.mod,
                                 sp = as.integer(as.factor(SPP)),
                                 x_h = x_h,
                                 x_a = x_a,
                                 x_b = x_b,
                                 n_plot = n.plot,
                                 t_plot = seq(min(Yrs) + offset.mod, max(Yrs), length = n.plot)))
    } else {
      out <- with(data_set, list(n = length(SPP),
                                 n_sp = length(unique(SPP)),
                                 n_xh = ncol(x_h),
                                 n_xa = ncol(x_a),
                                 h = Ht,
                                 t = Yrs + offset.mod,
                                 sp = as.integer(as.factor(SPP)),
                                 x_h = x_h,
                                 x_a = x_a,
                                 n_plot = n.plot,
                                 t_plot = seq(min(Yrs) + offset.mod, max(Yrs), length = n.plot)))
    }
  } else {
    row.match <- match(unique(data_set$SPP), data_set$SPP)
    x <- cbind(rep(1, length(unique(data_set$SPP))),
               data_set$N[row.match],
               data_set$SD_MASS[row.match],
               data_set$SD[row.match],
               data_set$SLA[row.match])
    n.int.knots <- spline_params$n_knots
    bs.order <- spline_params$degree + 1
    t_index <- match(data_set$Yrs, sort(unique(data_set$Yrs)))
    max.vals <- tapply(data_set$Yrs, data_set$SPP, max)
    basis.func <- array(NA, dim = c(n.plot, length(unique(data_set$SPP)), (n.int.knots + bs.order - 1)))
    basis.func2 <- array(NA, dim = c(n.plot, length(unique(data_set$SPP)), (n.int.knots + bs.order - 1)))
    for (i in seq(along = unique(data_set$SPP))) {
      if (spline_params$use_b_spline) {
        if (n.int.knots > 0) {
          knots_set <- sort(c(rep(c(min(data_set$Yrs), max(data_set$Yrs)), bs.order),
                              quantile(seq(min(data_set$Yrs), max(data_set$Yrs), length = 100),
                                       exp(seq.int(from = log(0.001), to = log(1),
                                               length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
        } else {
          knots_set <- sort(c(rep(c(min(data_set$Yrs), max(data_set$Yrs)), bs.order)))
        }
        basis.func[, i, ] <- splineDesign(knots_set,
                                          x = seq(min(data_set$Yrs), max.vals[i], length = n.plot),
                                          ord = bs.order,
                                          derivs = 0)[, -1, drop = FALSE]
        basis.func2[, i, ] <- splineDesign(knots_set,
                                           x = seq(min(data_set$Yrs), max.vals[i], length = 100),
                                           ord = bs.order,
                                           derivs = 1)[, -1, drop = FALSE]
      } else {
        if (n.int.knots > 0) {
          knots_set <- quantile(seq(min(data_set$Yrs), max(data_set$Yrs), length = 100),
                                exp(seq.int(from = log(0.001), to = log(1),
                                            length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
        } else {
          knots_set <- NULL
        }
        basis.func[, i, ] <- splines2::iSpline(seq(min(data_set$Yrs), max.vals[i], length = 100),
                                               knots = knots_set,
                                               degree = (bs.order - 1),
                                               Boundary.knots = c(min(data_set$Yrs), max(data_set$Yrs)))
        basis.func2[, i, ] <- splines2::iSpline(seq(min(data_set$Yrs), max.vals[i], length = 100),
                                                knots = knots_set,
                                                degree = (bs.order - 1),
                                                Boundary.knots = c(min(data_set$Yrs), max(data_set$Yrs)),
                                                derivs = 1)
      }
    }
    if (spline_params$use_b_spline) {
      if (n.int.knots > 0) {
        knots_set <- sort(c(rep(c(min(data_set$Yrs), max(data_set$Yrs)), bs.order),
                            quantile(seq(min(data_set$Yrs), max(data_set$Yrs), length = 100),
                                     exp(seq.int(from = log(0.001), to = log(1),
                                                 length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
      } else {
        knots_set <- sort(c(rep(c(min(data_set$Yrs), max(data_set$Yrs)), bs.order)))
      }
      spline_out <- splineDesign(knots_set,
                                 x = sort(unique(data_set$Yrs)),
                                 ord = bs.order,
                                 derivs = 0)[, -1, drop = FALSE]
    } else {
      if (n.int.knots > 0) {
        knots_set <- quantile(seq(min(data_set$Yrs), max(data_set$Yrs), length = 100),
                              exp(seq.int(from = log(0.001), to = log(1),
                                          length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
      } else {
        knots_set <- NULL
      }
      spline_out <- splines2::iSpline(sort(unique(data_set$Yrs)),
                                      knots = knots_set,
                                      degree = (bs.order - 1),
                                      Boundary.knots = c(min(data_set$Yrs), max(data_set$Yrs)))
    }
    out <- with(data_set, list(n = length(SPP),
                               n_sp = length(unique(SPP)),
                               n_x = ncol(x),
                               h = Ht,
                               t = Yrs,
                               n_t = length(unique(t_index)),
                               n_k = (n.int.knots + bs.order - 1),
                               t_index = t_index,
                               b_spline = spline_out,
                               sp = as.integer(as.factor(SPP)),
                               x = x,
                               n_plot = n.plot,
                               t_plot = seq(min(Yrs), max(Yrs), length = n.plot),
                               b_spline_plot = basis.func,
                               b_spline_deriv = basis.func2))
  }
  out
}

fetch_trait_list <- function(data_set, model, all_traits = FALSE) {
  row.match <- match(unique(data_set$SPP), data_set$SPP)
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
