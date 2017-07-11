data {
  int<lower=0> n;
  int<lower=0> n_block;
  int<lower=0> n_pred;
  int<lower=0> n_block_pred;
  vector<lower=0>[n] size_data;
  int<lower=0> n_age;
  int<lower=0> n_age_pred;
  int<lower=0> n_k;
  int<lower=0> age_index[n];
  int<lower=0> age_index_pred[n_pred];
  int<lower=0, upper=n_block> block_data[n];
  int<lower=0, upper=n_block_pred> block_holdout[n_pred];
  row_vector[n_k] b_spline[n_age];
  row_vector[n_k] b_spline_pred[n_age_pred];
  int<lower=0> n_plot;
  vector[n_plot] age_plot;
  row_vector[n_k] b_spline_plot[n_plot, n_block];
  row_vector[n_k] b_spline_deriv[n_plot, n_block];
}
            
parameters {
  real<lower=0> sigma_obs;
  vector<lower=0>[n_k] block_sd;
  vector[n_k] psi_mean;
  vector<lower=0>[n_k] sd_h;
  vector[n_k] psi[n_block];
  vector[n_k] h[n_block];
  vector[n_k] h_holdout[n_block_pred];
}
            
transformed parameters {
  vector[n] mu;
  for (i in 1:n)
    mu[i] = b_spline[age_index[i]] * h[block_data[i]];
}
            
model {
  log(size_data) ~ normal(mu, sigma_obs);
  sigma_obs ~ normal(0, 2);
  for (j in 1:n_block)
    h[j] ~ normal(psi[j], sd_h);
  for (j in 1:n_block_pred)
    h_holdout[j] ~ normal(psi_mean, sd_h);
  block_sd ~ normal(0.0, 2.0);
  psi_mean ~ normal(0.0, 2.0);
  for (j in 1:n_block)
    psi[j] ~ normal(psi_mean, block_sd);
  sd_h ~ normal(0.0, 2.0);
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
       mu_plot_growth[i, j] = b_spline_plot[i, j] * h[j];
   for (i in 1:n_plot)
     for (j in 1:n_block)
       size_plot[i, j] = exp(mu_plot_growth[i, j]);
   for (i in 1:n_plot)
     for (j in 1:n_block)
        mu_plot_agr[i, j] = b_spline_deriv[i, j] * h[j];
   for (i in 1:n_plot)
     for (j in 1:n_block)
       size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
   for (i in 1:n_pred)
     mu_pred[i] = b_spline_pred[age_index_pred[i]] * h_holdout[block_holdout[i]];
   for (i in 1:n_pred)
     size_pred[i] = exp(mu_pred[i]);
   for (i in 1:n)
     log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

