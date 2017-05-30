hillslope_param_fetch <- function() {
  num_par <- 3
  mu <- 'hm[sp[i]] / (1 + exp(-a[sp[i]] * (t[i] - b[sp[i]])))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] / (1 + exp(-a_holdout[sp_holdout[i]] * (t_holdout[i] - b_holdout[sp_holdout[i]])))'
  mu_plot <- 'hm[j] / (1 + exp(-a[j] * (t_plot[i] - b[j])))'
  mu_agr <- 'mu_plot_growth[i, j] * (a[j] * (exp(-a[j] * (t_plot[i] - b[j]))) / (1 + exp(-a[j] * (t_plot[i] - b[j]))))'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

hillslope_log_param_fetch <- function() {
  num_par <- 3
  mu <- 'hm[sp[i]] / (1 + exp(-a[sp[i]] * (log(t[i]) - b[sp[i]])))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] / (1 + exp(-a_holdout[sp_holdout[i]] * (log(t_holdout[i]) - b_holdout[sp_holdout[i]])))'
  mu_plot <- 'hm[j] / (1 + exp(-a[j] * (log(t_plot[i]) - b[j])))'
  mu_agr <- 'mu_plot_growth[i, j] * (a[j] * (exp(-a[j] * (log(t_plot[i]) - b[j]))) / (1 + exp(-a[j] * (log(t_plot[i]) - b[j]))))'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

power2_param_fetch <- function() {
  num_par <- 2
  mu <- 'hm[sp[i]] * (t[i] ^ a[sp[i]])'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] * (t_holdout[i] ^ a_holdout[sp_holdout[i]])'
  mu_plot <- 'hm[j] * (t_plot[i] ^ a[j])'
  mu_agr <- 'hm[j] * a[j] * (t_plot[i] ^ (a[j] - 1))'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

expo_param_fetch <- function() {
  num_par <- 2
  mu <- 'hm[sp[i]] + (a[sp[i]] * log(t[i]))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] + (a_holdout[sp_holdout[i]] * log(t_holdout[i]))'
  mu_plot <- 'hm[j] + (a[j] * log(t_plot[i]))'
  mu_agr <- 'a[j] / t_plot[i]'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

monod_param_fetch <- function() {
  num_par <- 2
  mu <- 'hm[sp[i]] * (t[i] / (a[sp[i]] + t[i]))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] * (t_holdout[i] / (a_holdout[sp_holdout[i]] + t_holdout[i]))'
  mu_plot <- 'hm[j] * (t_plot[i] / (a[j] + t_plot[i]))'
  mu_agr <- 'hm[j] * (a[j] / ((a[j] + t_plot[i]) ^ 2))'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

neg_exp_param_fetch <- function() {
  num_par <- 2
  mu <- 'hm[sp[i]] * (1 - exp(-a[sp[i]] * t[i]))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] * (1 - exp(-a_holdout[sp_holdout[i]] * t_holdout[i]))'
  mu_plot <- 'hm[j] * (1 - exp(-a[j] * t_plot[i]))'
  mu_agr <- 'hm[j] * a[j] * exp(-a[j] * t_plot[i])'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

koblog_param_fetch <- function() {
  num_par <- 2
  mu <- 'hm[sp[i]] * log(1 + (t[i] / a[sp[i]]))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] * log(1 + (t_holdout[i] / a_holdout[sp_holdout[i]]))'
  mu_plot <- 'hm[j] * log(1 + (t_plot[i] / a[j]))'
  mu_agr <- 'hm[j] / (a[j] + t_plot[i])'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

power3_param_fetch <- function() {
  num_par <- 3
  mu <- 'hm[sp[i]] * (t[i] ^ (a[sp[i]] - (b[sp[i]] / t[i])))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] * (t_holdout[i] ^ (a_holdout[sp_holdout[i]] - (b_holdout[sp_holdout[i]] / t_holdout[i])))'
  mu_plot <- 'hm[j] * (t_plot[i] ^ (a[j] - (b[j] / t_plot[i])))'
  mu_agr <- 'mu_plot_growth[i, j] * (((a[j] - (b[j] / t_plot[i])) / t_plot[i]) + (b[j] * log(t_plot[i])) / (t_plot[i] ^ 2))'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

logistic3_param_fetch <- function() {
  num_par <- 3
  mu <- 'hm[sp[i]] / (1 + exp(-a[sp[i]] * t[i] + b[sp[i]]))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] / (1 + exp(-a_holdout[sp_holdout[i]] * t_holdout[i] + b_holdout[sp_holdout[i]]))'
  mu_plot <- 'hm[j] / (1 + exp(-a[j] * t_plot[i] + b[j]))'
  mu_agr <- 'a[j] * hm[j] / (2 * cosh(b[j] - a[j] * t_plot[i]) + 2)'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

archibold_param_fetch <- function() {
  num_par <- 3
  mu <- 'hm[sp[i]] / (a[sp[i]] + (b[sp[i]] ^ t[i]))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] / (a_holdout[sp_holdout[i]] + (b_holdout[sp_holdout[i]] ^ t_holdout[i]))'
  mu_plot <- 'hm[j] / (a[j] + (b[j] ^ t_plot[i]))'
  mu_agr <- '-hm[j] * (b[j] ^ t_plot[i]) * log(b[j]) / ((a[j] + (b[j] ^ t_plot[i])) ^ 2)'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

weibull3_param_fetch <- function() {
  num_par <- 3
  mu <- 'hm[sp[i]] * (1 - exp(-a[sp[i]] * (t[i] ^ b[sp[i]])))'
  mu_holdout <- 'hm_holdout[sp_holdout[i]] * (1 - exp(-a_holdout[sp_holdout[i]] * (t_holdout[i] ^ b_holdout[sp_holdout[i]])))'
  mu_plot <- 'hm[j] * (1 - exp(-a[j] * (t_plot[i] ^ b[j])))'
  mu_agr <- 'hm[j] * a[j] * b[j] * (t_plot[i] ^ (b[j] - 1)) * exp(-a[j] * (t_plot[i] ^ b[j]))'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}

crazytest_param_fetch <- function() {
  num_par <- 5
  mu <- 'b1[sp[i]] + b2[sp[i]] + b3[sp[i]] * (t[i] - b4[sp[i]]) + b5[sp[i]]'
  mu_holdout <- 'b1[sp_holdout[i]] + b2[sp_holdout[i]] + b3[sp_holdout[i]] * (t_holdout[i] - b4[sp_holdout[i]]) + b5[sp_holdout[i]]'
  mu_plot <- 'b1[j] + b2[j] + b3[j] * (t_plot[i] - b4[j]) + b5[j]'
  mu_agr <- 'b1[j] + b2[j] + b3[j] * (t_plot[i] - b4[j]) + b5[j]'
  return(list(num_par = num_par,
              mu = mu,
              mu.holdout = mu.holdout,
              mu_plot = mu.plot,
              mu_agr = mu_agr))
}
