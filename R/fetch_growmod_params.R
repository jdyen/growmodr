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