data {
  int<lower=0> n;
  int<lower=0> n_pred;
  int<lower=0> n_block;
  int<lower=0> n_block_pred;
  int<lower=0> n_x1;
  int<lower=0> n_x2;
  int<lower=0> n_x3;
  matrix[n_block, n_x1] x1;
  matrix[n_block, n_x2] x2;
  matrix[n_block, n_x3] x3;
  matrix[n_block_pred, n_x1] x1_pred;
  matrix[n_block_pred, n_x2] x2_pred;
  matrix[n_block_pred, n_x3] x3_pred;
  vector<lower=0>[n] size_data;
  vector[n] age;
  matrix[n_pred, 1] age_holdout;
  int<lower=0, upper=n_block> block_data[n];
  int<lower=0, upper=n_block_pred> block_holdout[n_pred];
  int<lower=0> n_plot;
  vector[n_plot] age_plot;
}
      
parameters {
  real<lower=0> sigma_obs;
  real<lower=0> sd_h1;
  real<lower=0> sd_h2;
  real<lower=0> sd_h3;
  vector<lower=0>[n_block] h1;
  vector<lower=0>[n_block] h2;
  vector<lower=0>[n_block] h3;
  vector<lower=0>[n_block_pred] h1_holdout;
  vector<lower=0>[n_block_pred] h2_holdout;
  vector<lower=0>[n_block_pred] h3_holdout;
  vector[n_x1] b1;
  vector[n_x2] b2;
  vector[n_x3] b3;
}
      
transformed parameters {
  vector[n] mu;
  vector[n_block] psi1;
  vector[n_block] psi2;
  vector[n_block] psi3;
  vector[n_block_pred] psi1_pred;
  vector[n_block_pred] psi2_pred;
  vector[n_block_pred] psi3_pred;
  for (i in 1:n)
    mu[i] =  h1[block_data[i]] * (age[i] ^ (h2[block_data[i]] - (h3[block_data[i]] / age[i])));
  psi1 = x1 * b1;
  psi2 = x2 * b2;
  psi3 = x3 * b3;
  psi1_pred = x1_pred * b1;
  psi2_pred = x2_pred * b2;
  psi3_pred = x3_pred * b3;
}
      
model {
  log(size_data) ~ normal(mu, sigma_obs);
  sigma_obs ~ normal(0, 2);
  h1 ~ normal(psi1, sd_h1);
  h2 ~ normal(psi2, sd_h2);
  h3 ~ normal(psi3, sd_h3);
  h1_holdout ~ normal(psi1_pred, sd_h1);
  h2_holdout ~ normal(psi2_pred, sd_h2);
  h3_holdout ~ normal(psi3_pred, sd_h3);
  b1 ~ normal(0.0, 10.0);
  b2 ~ normal(0.0, 10.0);
  b3 ~ normal(0.0, 10.0);
  sd_h1 ~ normal(0.0, 2.0);
  sd_h2 ~ normal(0.0, 2.0);
  sd_h3 ~ normal(0.0, 2.0);
}
      
generated quantities {
  matrix[n_plot, n_block] mu_plot_growth;
  matrix[n_plot, n_block] mu_plot_agr;
  vector[n_pred] mu_pred;
  matrix<lower=0>[n_plot, n_block] size_plot;
  matrix<lower=0>[n_plot, n_block] size_plot_agr;
  vector<lower=0>[n_pred] size_pred;
  vector[n] log_lik;
      
  for (i in 1:n_plot)
    for (j in 1:n_block)
      mu_plot_growth[i, j] =  h1[j] * (age_plot[i] ^ (h2[j] - (h3[j] / age_plot[i])));
  for (i in 1:n_plot)
    for (j in 1:n_block)
      size_plot[i, j] = exp(mu_plot_growth[i, j]);
  for (i in 1:n_plot)
    for (j in 1:n_block)
      mu_plot_agr[i, j] =  mu_plot_growth[i, j] * (((h2[j] - (h3[j] / age_plot[i])) / age_plot[i]) + (h3[j] * log(age_plot[i])) / (age_plot[i] ^ 2));
  for (i in 1:n_plot)
    for (j in 1:n_block)
      size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
  for (i in 1:n_pred)
    mu_pred[i] =  h1_holdout[block_holdout[i]] * (age_holdout[i, 1] ^ (h2_holdout[block_holdout[i]] - (h3_holdout[block_holdout[i]] / age_holdout[i, 1])));
  for (i in 1:n_pred)
    size_pred[i] = exp(mu_pred[i]);
  for (i in 1:n)
    log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

