# h1 = hm; h2 = a; h3 = b;
# block = block
# age = t

hillslope_param_fetch <- function(include_block = TRUE) {
  num_par <- 3
  if (include_block) {
    mu <- 'h1[block_data[i]] / (1 + exp(-h2[block_data[i]] * (age[i] - h3[block_data[i]])))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] / (1 + exp(-h2_holdout[block_holdout[i]] * (age_holdout[i, 1] - h3_holdout[block_holdout[i]])))'
    mu_plot <- 'h1[j] / (1 + exp(-h2[j] * (age_plot[i] - h3[j])))'
    mu_agr <- 'mu_plot_growth[i, j] * (h2[j] * (exp(-h2[j] * (age_plot[i] - h3[j]))) / (1 + exp(-h2[j] * (age_plot[i] - h3[j]))))'
  } else {
    mu <- 'h1 / (1 + exp(-h2 * (age[i] - h3)))'
    mu_holdout <- 'h1_holdout / (1 + exp(-h2_holdout * (age_holdout[i, 1] - h3_holdout)))'
    mu_plot <- 'h1 / (1 + exp(-h2 * (age_plot[i] - h3)))'
    mu_agr <- 'mu_plot_growth[i] * (h2 * (exp(-h2 * (age_plot[i] - h3))) / (1 + exp(-h2 * (age_plot[i] - h3))))'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

hillslope_log_param_fetch <- function(include_block = TRUE) {
  num_par <- 3
  if (include_block) {
    mu <- 'h1[block_data[i]] / (1 + exp(-h2[block_data[i]] * (log(age[i]) - h3[block_data[i]])))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] / (1 + exp(-h2_holdout[block_holdout[i]] * (log(age_holdout[i, 1]) - h3_holdout[block_holdout[i]])))'
    mu_plot <- 'h1[j] / (1 + exp(-h2[j] * (log(age_plot[i]) - h3[j])))'
    mu_agr <- 'mu_plot_growth[i, j] * (h2[j] * (exp(-h2[j] * (log(age_plot[i]) - h3[j]))) / (1 + exp(-h2[j] * (log(age_plot[i]) - h3[j]))))'
  } else {
    mu <- 'h1 / (1 + exp(-h2 * (log(age[i]) - h3)))'
    mu_holdout <- 'h1_holdout / (1 + exp(-h2_holdout * (log(age_holdout[i, 1]) - h3_holdout)))'
    mu_plot <- 'h1 / (1 + exp(-h2 * (log(age_plot[i]) - h3)))'
    mu_agr <- 'mu_plot_growth[i] * (h2 * (exp(-h2 * (log(age_plot[i]) - h3))) / (1 + exp(-h2 * (log(age_plot[i]) - h3))))'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

power2_param_fetch <- function(include_block = TRUE) {
  num_par <- 2
  if (include_block) {
    mu <- 'h1[block_data[i]] * (age[i] ^ h2[block_data[i]])'
    mu_holdout <- 'h1_holdout[block_holdout[i]] * (age_holdout[i, 1] ^ h2_holdout[block_holdout[i]])'
    mu_plot <- 'h1[j] * (age_plot[i] ^ h2[j])'
    mu_agr <- 'h1[j] * h2[j] * (age_plot[i] ^ (h2[j] - 1))'
  } else {
    mu <- 'h1 * (age[i] ^ h2)'
    mu_holdout <- 'h1_holdout * (age_holdout[i, 1] ^ h2_holdout)'
    mu_plot <- 'h1 * (age_plot[i] ^ h2)'
    mu_agr <- 'h1 * h2 * (age_plot[i] ^ (h2 - 1))'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

expo_param_fetch <- function(include_block = TRUE) {
  num_par <- 2
  if (include_block) {
    mu <- 'h1[block_data[i]] + (h2[block_data[i]] * log(age[i]))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] + (h2_holdout[block_holdout[i]] * log(age_holdout[i, 1]))'
    mu_plot <- 'h1[j] + (h2[j] * log(age_plot[i]))'
    mu_agr <- 'h2[j] / age_plot[i]'
  } else {
    mu <- 'h1 + (h2 * log(age[i]))'
    mu_holdout <- 'h1_holdout + (h2_holdout * log(age_holdout[i, 1]))'
    mu_plot <- 'h1 + (h2 * log(age_plot[i]))'
    mu_agr <- 'h2 / age_plot[i]'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

monod_param_fetch <- function(include_block = TRUE) {
  num_par <- 2
  if (include_block) {
    mu <- 'h1[block_data[i]] * (age[i] / (h2[block_data[i]] + age[i]))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] * (age_holdout[i, 1] / (h2_holdout[block_holdout[i]] + age_holdout[i, 1]))'
    mu_plot <- 'h1[j] * (age_plot[i] / (h2[j] + age_plot[i]))'
    mu_agr <- 'h1[j] * (h2[j] / ((h2[j] + age_plot[i]) ^ 2))'
  } else {
    mu <- 'h1 * (age[i] / (h2 + age[i]))'
    mu_holdout <- 'h1_holdout * (age_holdout[i, 1] / (h2_holdout + age_holdout[i, 1]))'
    mu_plot <- 'h1 * (age_plot[i] / (h2 + age_plot[i]))'
    mu_agr <- 'h1 * (h2 / ((h2 + age_plot[i]) ^ 2))'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

neg_exp_param_fetch <- function(include_block = TRUE) {
  num_par <- 2
  if (include_block) {
    mu <- 'h1[block_data[i]] * (1 - exp(-h2[block_data[i]] * age[i]))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] * (1 - exp(-h2_holdout[block_holdout[i]] * age_holdout[i, 1]))'
    mu_plot <- 'h1[j] * (1 - exp(-h2[j] * age_plot[i]))'
    mu_agr <- 'h1[j] * h2[j] * exp(-h2[j] * age_plot[i])'
  } else {
    mu <- 'h1 * (1 - exp(-h2 * age[i]))'
    mu_holdout <- 'h1_holdout * (1 - exp(-h2_holdout * age_holdout[i, 1]))'
    mu_plot <- 'h1 * (1 - exp(-h2 * age_plot[i]))'
    mu_agr <- 'h1 * h2 * exp(-h2 * age_plot[i])'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

koblog_param_fetch <- function(include_block = TRUE) {
  num_par <- 2
  if (include_block) {
    mu <- 'h1[block_data[i]] * log(1 + (age[i] / h2[block_data[i]]))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] * log(1 + (age_holdout[i, 1] / h2_holdout[block_holdout[i]]))'
    mu_plot <- 'h1[j] * log(1 + (age_plot[i] / h2[j]))'
    mu_agr <- 'h1[j] / (h2[j] + age_plot[i])'
  } else {
    mu <- 'h1 * log(1 + (age[i] / h2))'
    mu_holdout <- 'h1_holdout * log(1 + (age_holdout[i, 1] / h2_holdout))'
    mu_plot <- 'h1 * log(1 + (age_plot[i] / h2))'
    mu_agr <- 'h1 / (h2 + age_plot[i])'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

power3_param_fetch <- function(include_block = TRUE) {
  num_par <- 3
  if (include_block) {
    mu <- 'h1[block_data[i]] * (age[i] ^ (h2[block_data[i]] - (h3[block_data[i]] / age[i])))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] * (age_holdout[i, 1] ^ (h2_holdout[block_holdout[i]] - (h3_holdout[block_holdout[i]] / age_holdout[i, 1])))'
    mu_plot <- 'h1[j] * (age_plot[i] ^ (h2[j] - (h3[j] / age_plot[i])))'
    mu_agr <- 'mu_plot_growth[i, j] * (((h2[j] - (h3[j] / age_plot[i])) / age_plot[i]) + (h3[j] * log(age_plot[i])) / (age_plot[i] ^ 2))'
  } else {
    mu <- 'h1 * (age[i] ^ (h2 - (h3 / age[i])))'
    mu_holdout <- 'h1_holdout * (age_holdout[i, 1] ^ (h2_holdout - (h3_holdout / age_holdout[i, 1])))'
    mu_plot <- 'h1 * (age_plot[i] ^ (h2 - (h3 / age_plot[i])))'
    mu_agr <- 'mu_plot_growth[i] * (((h2 - (h3 / age_plot[i])) / age_plot[i]) + (h3 * log(age_plot[i])) / (age_plot[i] ^ 2))'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

logistic3_param_fetch <- function(include_block = TRUE) {
  num_par <- 3
  if (include_block) {
    mu <- 'h1[block_data[i]] / (1 + exp(-h2[block_data[i]] * age[i] + h3[block_data[i]]))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] / (1 + exp(-h2_holdout[block_holdout[i]] * age_holdout[i, 1] + h3_holdout[block_holdout[i]]))'
    mu_plot <- 'h1[j] / (1 + exp(-h2[j] * age_plot[i] + h3[j]))'
    mu_agr <- 'h2[j] * h1[j] / (2 * cosh(h3[j] - h2[j] * age_plot[i]) + 2)'
  } else {
    mu <- 'h1 / (1 + exp(-h2 * age[i] + h3))'
    mu_holdout <- 'h1_holdout / (1 + exp(-h2_holdout * age_holdout[i, 1] + h3_holdout))'
    mu_plot <- 'h1 / (1 + exp(-h2 * age_plot[i] + h3))'
    mu_agr <- 'h2 * h1 / (2 * cosh(h3 - h2 * age_plot[i]) + 2)'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

archibold_param_fetch <- function(include_block = TRUE) {
  num_par <- 3
  if (include_block) {
    mu <- 'h1[block_data[i]] / (h2[block_data[i]] + (h3[block_data[i]] ^ age[i]))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] / (h2_holdout[block_holdout[i]] + (h3_holdout[block_holdout[i]] ^ age_holdout[i, 1]))'
    mu_plot <- 'h1[j] / (h2[j] + (h3[j] ^ age_plot[i]))'
    mu_agr <- '-h1[j] * (h3[j] ^ age_plot[i]) * log(h3[j]) / ((h2[j] + (h3[j] ^ age_plot[i])) ^ 2)'
  } else {
    mu <- 'h1 / (h2 + (h3 ^ age[i]))'
    mu_holdout <- 'h1_holdout / (h2_holdout + (h3_holdout ^ age_holdout[i, 1]))'
    mu_plot <- 'h1 / (h2 + (h3 ^ age_plot[i]))'
    mu_agr <- '-h1 * (h3 ^ age_plot[i]) * log(h3) / ((h2 + (h3 ^ age_plot[i])) ^ 2)'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

weibull3_param_fetch <- function(include_block = TRUE) {
  num_par <- 3
  if (include_block) {
    mu <- 'h1[block_data[i]] * (1 - exp(-h2[block_data[i]] * (age[i] ^ h3[block_data[i]])))'
    mu_holdout <- 'h1_holdout[block_holdout[i]] * (1 - exp(-h2_holdout[block_holdout[i]] * (age_holdout[i, 1] ^ h3_holdout[block_holdout[i]])))'
    mu_plot <- 'h1[j] * (1 - exp(-h2[j] * (age_plot[i] ^ h3[j])))'
    mu_agr <- 'h1[j] * h2[j] * h3[j] * (age_plot[i] ^ (h3[j] - 1)) * exp(-h2[j] * (age_plot[i] ^ h3[j]))'
  } else {
    mu <- 'h1 * (1 - exp(-h2 * (age[i] ^ h3)))'
    mu_holdout <- 'h1_holdout * (1 - exp(-h2_holdout * (age_holdout[i, 1] ^ h3_holdout)))'
    mu_plot <- 'h1 * (1 - exp(-h2 * (age_plot[i] ^ h3)))'
    mu_agr <- 'h1 * h2 * h3 * (age_plot[i] ^ (h3 - 1)) * exp(-h2 * (age_plot[i] ^ h3))'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}

crazytest_param_fetch <- function(include_block = TRUE) {
  num_par <- 5
  if (include_block) {
    mu <- 'h1[block_data[i]] + h2[block_data[i]] + h3[block_data[i]] * (age[i] - h4[block_data[i]]) + h5[block_data[i]]'
    mu_holdout <- 'h1[block_holdout[i]] + h2[block_holdout[i]] + h3[block_holdout[i]] * (age_holdout[i, 1] - h4[block_holdout[i]]) + h5[block_holdout[i]]'
    mu_plot <- 'h1[j] + h2[j] + h3[j] * (age_plot[i] - h4[j]) + h5[j]'
    mu_agr <- 'h1[j] + h2[j] + h3[j] * (age_plot[i] - h4[j]) + h5[j]'
  } else {
    mu <- 'h1 + h2 + h3 * (age[i] - h4) + h5'
    mu_holdout <- 'h1 + h2 + h3 * (age_holdout[i, 1] - h4) + h5'
    mu_plot <- 'h1 + h2 + h3 * (age_plot[i] - h4) + h5'
    mu_agr <- 'h1 + h2 + h3 * (age_plot[i] - h4) + h5'
  }
  return(list(num_par = num_par,
              mu = mu,
              mu_holdout = mu_holdout,
              mu_plot = mu_plot,
              mu_agr = mu_agr))
}
