stan_growth_model_cv <- function(data, model, all_traits,
                                 n.cv = NULL,
                                 n.iter = 10000,
                                 n.burnin = round(n.iter / 2),
                                 n.thin = max(1, floor(n.iter / 10000)),
                                 n.chains = 4,
                                 inits = NULL,
                                 seed = 12528921,
                                 control = NULL,
                                 cores = 1,
                                 spline_params = list(degree = 8, n_knots = 10, use_b_spline = FALSE),
                                 ...) {
  mod.file <- gen_mod_file_cv(model)
  data.set <- fetch_model_data(data, model = model, all_traits = all_traits, n.plot = 100,
                               spline_params = spline_params)
  if (is.null(inits)) {
    inits <- '0'
  }
  if (is.null(control)) {
    control = list(adapt_delta = 0.99)
  }
  if (is.null(n.cv)) {
    n.cv <- data.set$n_sp
  }
  mod.def <- stan_model(file = mod.file)
  if ((cores > 1) & (Sys.info()['sysname'] == 'Darwin')) {
    mod <- mclapply(1:n.cv, stan_cv_internal,
                    mod.def,
                    data.set,
                    model,
                    n.cv,
                    n.iter,
                    n.chains,
                    inits,
                    seed,
                    control, 
                    spline_params,
                    mc.cores = parallel::detectCores(),
                    ...)
  } else {
    mod <- lapply(1:n.cv, stan_cv_internal,
                  mod.def,
                  data.set,
                  model,
                  n.cv,
                  n.iter,
                  n.chains,
                  inits,
                  seed,
                  control,
                  spline_params, ...)
  }
  out.full <- do.call('rbind', mod)
  r2_cv <- cor(out.full$h_pred, out.full$h_real) ** 2
  rmsd_cv <- sqrt(mean((out.full$h_pred - out.full$h_real) ** 2))
  md_cv <- mean((out.full$h_pred - out.full$h_real))
  elpd_cv <- sum(out.full$loglik)
  return(list(r2_cv = r2_cv,
              rmsd_cv = rmsd_cv,
              md_cv = md_cv,
              elpd_cv = elpd_cv))
}

stan_cv_internal <- function(i,
                             mod.def,
                             data.set,
                             model,
                             n.cv,
                             n.iter,
                             n.chains,
                             inits,
                             seed,
                             control, 
                             spline_params, ...) {
  data_set <- refine_data_cv(data.set, model, i, n = data.set$n, n.cv, spline_params)
  if (model != 'spline') {
    if (length(data_set$x_h_holdout) == ncol(data_set$x_h)) {
      data_set$x_h_holdout <- matrix(data_set$x_h_holdout, ncol = ncol(data_set$x_h))
    }
    if (length(data_set$x_a_holdout) == ncol(data_set$x_a)) {
      data_set$x_a_holdout <- matrix(data_set$x_a_holdout, ncol = ncol(data_set$x_a))
    }
    if (!is.null(data_set$x_b)) {
      if (length(data_set$x_b_holdout) == ncol(data_set$x_b)) {
        data_set$x_b_holdout <- matrix(data_set$x_b_holdout, ncol = ncol(data_set$x_b))
      }
    }
  } else {
    if (length(data_set$x_holdout) == ncol(data_set$x)) {
      data_set$x_holdout <- matrix(data_set$x_holdout, ncol = ncol(data_set$x))
    }
  }
  pars <- c('log_lik_train', 'log_lik_holdout', 'h_holdout_pred')
  mod <- sampling(object = mod.def,
                  data = data_set,
                  iter = n.iter,
                  pars = pars,
                  chains = n.chains,
                  init = inits,
                  seed = seed,
                  control = control, ...)
  out.loglik <- get_posterior_mean(mod, pars = c('log_lik_holdout'))
  out.hpred <- get_posterior_mean(mod, pars = c('h_holdout_pred'))
  h_holdout_real <- data_set$h_holdout
  out <- data.frame(loglik = out.loglik[, ncol(out.loglik)],
                    h_pred = out.hpred[, ncol(out.hpred)],
                    h_real = h_holdout_real)
  out
}

gen_mod_file_cv <- function(model) {
  if (model != 'spline') {
    mod.params <- get(paste0(model, '_param_fetch'))()
    three.par <- mod.params$three.par
    mod.mu <- mod.params$mu
    mod.mu.holdout <- mod.params$mu.holdout
    cat(
      'data {
      int<lower=0> n;
      int<lower=0> n_holdout;
      int<lower=0> n_sp;
      int<lower=0> n_sp_holdout;
      int<lower=0> n_xh;
      int<lower=0> n_xa;
      vector<lower=0>[n] h;
      vector<lower=0>[n_holdout] h_holdout;
      vector[n] t;
      vector[n_holdout] t_holdout;
      int<lower=0, upper=n_sp> sp[n];
      int<lower=0, upper=n_sp_holdout> sp_holdout[n_holdout];
      matrix[n_sp, n_xh] x_h;
      matrix[n_sp, n_xa] x_a;
      matrix[n_sp_holdout, n_xh] x_h_holdout;
      matrix[n_sp_holdout, n_xa] x_a_holdout;\n',
      if (three.par) {
        ' int<lower=0> n_xb;\n'
      },
      if (three.par) {
        ' matrix[n_sp, n_xb] x_b;
        matrix[n_sp_holdout, n_xb] x_b_holdout;\n'
      },
  '}
      
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
        ' vector<lower=0>[n_sp] b;
        vector<lower=0>[n_sp_holdout] b_holdout;\n'
      },
      ' vector<lower=0>[n_sp] hm;
      vector<lower=0>[n_sp_holdout] hm_holdout;
      vector<lower=0>[n_sp] a;
      vector<lower=0>[n_sp_holdout] a_holdout;
      vector[n_xh] b0;
      vector[n_xa] b1;
      }
      
      transformed parameters {
      vector[n] mu;
      vector[n_sp] psi_h;
      vector[n_sp_holdout] psi_h_holdout;
      vector[n_sp] psi_a;
      vector[n_sp_holdout] psi_a_holdout;\n',
      if (three.par) {
        ' vector[n_sp] psi_b;
        vector[n_sp_holdout] psi_b_holdout;\n'
      },
      ' for (i in 1:n)
      mu[i] = ', mod.mu, ';\n',
      ' psi_h = x_h * b0;
      psi_a = x_a * b1;\n',
      if (three.par) {
        ' psi_b = x_b * b2;\n'
      },
      ' psi_h_holdout = x_h_holdout * b0;
      psi_a_holdout = x_a_holdout * b1;\n',
      if (three.par) {
      ' psi_b_holdout = x_b_holdout * b2;\n'
      },
      '}
      
      model {
      log(h) ~ normal(mu, sigma_obs);
      hm ~ normal(psi_h, sd_h);
      a ~ normal(psi_a, sd_a);\n',
      if (three.par) {
        ' b ~ normal(psi_b, sd_b);\n'
      },
      ' hm_holdout ~ normal(psi_h_holdout, sd_h);
      a_holdout ~ normal(psi_a_holdout, sd_a);\n',
      if (three.par) {
      ' b_holdout ~ normal(psi_b_holdout, sd_b);\n'
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
      vector[n] log_lik_train;
      vector[n_holdout] mu_holdout;
      vector[n_holdout] h_holdout_pred;
      vector[n_holdout] log_lik_holdout;
      
      for (i in 1:n)
        log_lik_train[i] = normal_lpdf(log(h[i]) | mu[i], sigma_obs);
      for (i in 1:n_holdout)
        mu_holdout[i] = ', mod.mu.holdout, ';\n',
      ' for (i in 1:n_holdout)
        h_holdout_pred[i] = exp(mu_holdout[i]);
      for (i in 1:n_holdout)
        log_lik_holdout[i] = normal_lpdf(log(h_holdout[i]) | mu_holdout[i], sigma_obs);
      }\n',
  file = (mod.file <- tempfile()))
    } else {
      cat(
        'data {
        int<lower=0> n;
        int<lower=0> n_holdout;
        int<lower=0> n_sp;
        int<lower=0> n_sp_holdout;
        int<lower=0> n_x;
        vector<lower=0>[n] h;
        vector<lower=0>[n_holdout] h_holdout;
        vector[n] t;
        vector[n_holdout] t_holdout;
        int<lower=0> n_t;
        int<lower=0> n_t_holdout;
        int<lower=0> n_k;
        int<lower=0> t_index[n];
        int<lower=0> t_index_holdout[n_holdout];
        int<lower=0, upper=n_sp> sp[n];
        int<lower=0, upper=n_sp_holdout> sp_holdout[n_holdout];
        matrix[n_sp, n_x] x;
        matrix[n_sp_holdout, n_x] x_holdout;
        row_vector[n_k] b_spline[n_t];
        row_vector[n_k] b_spline_holdout[n_t_holdout];
        }
        
        parameters {
        real<lower=0> sigma_obs;
        vector<lower=0>[n_k] sd_param;
        vector[n_k] beta[n_sp];
        vector[n_k] beta_holdout[n_sp_holdout];
        matrix[n_x, n_k] b_param;
        }
        
        transformed parameters {
        vector[n] mu;
        matrix[n_sp, n_k] psi;
        matrix[n_sp_holdout, n_k] psi_holdout;
        for (i in 1:n)
        mu[i] = b_spline[t_index[i]] * beta[sp[i]];
        psi = x * b_param;
        psi_holdout = x_holdout * b_param;
        }
        
        model {
        log(h) ~ normal(mu, sigma_obs);
        for (j in 1:n_sp)
        beta[j] ~ normal(psi[j], sd_param);
        for (j in 1:n_sp_holdout)
          beta_holdout[j] ~ normal(psi_holdout[j], sd_param);
        for (j in 1:n_x)
        b_param[j] ~ normal(0, 10);
        sd_param ~ normal(0, 2);
        sigma_obs ~ normal(0, 2);
        }
        
        generated quantities {
        vector[n] log_lik_train;
        vector[n_holdout] log_lik_holdout;
        vector[n_holdout] mu_holdout;
        vector[n_holdout] h_holdout_pred;
        
        for (i in 1:n_holdout)
          mu_holdout[i] = b_spline_holdout[t_index_holdout[i]] * beta_holdout[sp_holdout[i]];
        for (i in 1:n_holdout)
          h_holdout_pred[i] = exp(mu_holdout[i]);
        for (i in 1:n)
          log_lik_train[i] = normal_lpdf(log(h[i]) | mu[i], sigma_obs);
        for (i in 1:n_holdout)
          log_lik_holdout[i] = normal_lpdf(log(h_holdout[i]) | mu_holdout[i], sigma_obs);
    }\n',
  file = (mod.file <- tempfile()))
  }
  mod.file
}

refine_data_cv <- function(data, model, i, n, n.cv, spline_params) {
  if (n.cv == data$n_sp) {
    holdout.id <- which(data$sp == i)
  } else {
    n.holdout <- floor(n / n.cv)
    if (i < n.cv) {
      holdout.id <- ((i - 1) * n.holdout + 1):(i * n.holdout)
    } else {
      holdout.id <- ((i - 1) * n.holdout + 1):(n)
    }
  }
  if (model != 'spline') {
    if (!is.null(data$x_b)) {
      out <- list(n = length(data$h[-holdout.id]),
                  n_holdout = length(holdout.id),
                  n_sp = length(unique(data$sp[-holdout.id])),
                  n_sp_holdout = length(unique(data$sp[holdout.id])),
                  n_xh = data$n_xh,
                  n_xa = data$n_xa,
                  n_xb = data$n_xb,
                  h = data$h[-holdout.id],
                  h_holdout = data$h[holdout.id],
                  t = data$t[-holdout.id],
                  t_holdout = data$t[holdout.id],
                  sp = as.integer(as.factor(data$sp[-holdout.id])),
                  sp_holdout = as.integer(as.factor(data$sp[holdout.id])),
                  x_h = data$x_h[unique(data$sp[-holdout.id]), ],
                  x_a = data$x_a[unique(data$sp[-holdout.id]), ],
                  x_b = data$x_b[unique(data$sp[-holdout.id]), ],
                  x_h_holdout = data$x_h[unique(data$sp[holdout.id]),],
                  x_a_holdout = data$x_a[unique(data$sp[holdout.id]),],
                  x_b_holdout = data$x_b[unique(data$sp[holdout.id]), ])
    } else {
      out <- list(n = length(data$h[-holdout.id]),
                  n_holdout = length(holdout.id),
                  n_sp = length(unique(data$sp[-holdout.id])),
                  n_sp_holdout = length(unique(data$sp[holdout.id])),
                  n_xh = data$n_xh,
                  n_xa = data$n_xa,
                  h = data$h[-holdout.id],
                  h_holdout = data$h[holdout.id],
                  t = data$t[-holdout.id],
                  t_holdout = data$t[holdout.id],
                  sp = as.integer(as.factor(data$sp[-holdout.id])),
                  sp_holdout = as.integer(as.factor(data$sp[holdout.id])),
                  x_h = data$x_h[unique(data$sp[-holdout.id]), ],
                  x_a = data$x_a[unique(data$sp[-holdout.id]), ],
                  x_h_holdout = data$x_h[unique(data$sp[holdout.id]), ],
                  x_a_holdout = data$x_a[unique(data$sp[holdout.id]), ])
    }
  } else {
    n.int.knots <- spline_params$n_knots
    bs.order <- spline_params$degree + 1
    if (spline_params$use_b_spline) {
      if (n.int.knots > 0) {
        knots_set <- sort(c(rep(c(min(data$t), max(data$t)), bs.order),
                            quantile(seq(min(data$t), max(data$t), length = 100),
                                     exp(seq.int(from = log(0.001), to = log(1),
                                                 length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])))
      } else {
        knots_set <- sort(c(rep(c(min(data$t), max(data$t)), bs.order)))
      }
      spline_out <- splineDesign(knots_set,
                                 x = sort(unique(data$t[-holdout.id])),
                                 ord = bs.order,
                                 derivs = 0)[, -1, drop = FALSE]
      spline_out2 <- splineDesign(knots_set,
                                  x = sort(unique(data$t[holdout.id])),
                                  ord = bs.order,
                                  derivs = 0)[, -1, drop = FALSE]
    } else {
      if (n.int.knots > 0) {
        knots_set <- quantile(seq(min(data$t), max(data$t), length = 100),
                              exp(seq.int(from = log(0.001), to = log(1),
                                          length.out = (n.int.knots + 2L)))[-c(1, (n.int.knots + 2L))])
      } else {
        knots_set <- NULL
      }
      spline_out <- splines2::iSpline(sort(unique(data$t[-holdout.id])),
                                      knots = knots_set,
                                      degree = (bs.order - 1),
                                      Boundary.knots = c(min(data$t), max(data$t)))
      spline_out2 <- splines2::iSpline(sort(unique(data$t[holdout.id])),
                                       knots = knots_set,
                                       degree = (bs.order - 1),
                                       Boundary.knots = c(min(data$t), max(data$t)))
    }
    out <- list(n = length(data$h[-holdout.id]),
                n_holdout = length(holdout.id),
                n_sp = length(unique(data$sp[-holdout.id])),
                n_sp_holdout = length(unique(data$sp[holdout.id])),
                n_x = data$n_x,
                h = data$h[-holdout.id],
                h_holdout = data$h[holdout.id],
                t = data$t[-holdout.id],
                t_holdout = data$t[holdout.id],
                n_t = length(unique(data$t_index[-holdout.id])),
                n_t_holdout = length(unique(data$t_index[holdout.id])),
                n_k = data$n_k,
                t_index = as.integer(as.factor(data$t_index[-holdout.id])),
                t_index_holdout = as.integer(as.factor(data$t_index[holdout.id])),
                sp = as.integer(as.factor(data$sp[-holdout.id])),
                sp_holdout = as.integer(as.factor(data$sp[holdout.id])),
                x = data$x[unique(data$sp[-holdout.id]), ],
                x_holdout = data$x[unique(data$sp[holdout.id]), ],
                b_spline = spline_out,
                b_spline_holdout = spline_out2)
  }
  out
}