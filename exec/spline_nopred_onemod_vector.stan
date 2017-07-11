data {
  int<lower=0> n;
  int<lower=0> n_pred;
  vector<lower=0>[n] size_data;
  vector[n] age;
  int<lower=0> n_age;
  int<lower=0> n_age_pred;
  int<lower=0> n_k;
  int<lower=0> age_index[n];
  int<lower=0> age_index_pred[n_pred];
  row_vector[n_k] b_spline[n_age];
  row_vector[n_k] b_spline_pred[n_age_pred];
  int<lower=0> n_plot;
  vector[n_plot] age_plot;
  row_vector[n_k] b_spline_plot[n_plot, 1];
  row_vector[n_k] b_spline_deriv[n_plot, 1];
}
            
parameters {
  real<lower=0> sigma_obs;
  vector[n_k] h;
}
            
transformed parameters {
  vector[n] mu;
  for (i in 1:n)
    mu[i] = b_spline[age_index[i]] * h; 
}
            
model {
  log(size_data) ~ normal(mu, sigma_obs);
  sigma_obs ~ normal(0, 2);
  h ~ normal(0.0, 2.0);
}
            
generated quantities {
  vector[n_plot] mu_plot_growth;
  vector[n_plot] mu_plot_agr;
  vector[n_pred] mu_pred;
  vector<lower=0>[n_plot] size_plot;
  vector<lower=0>[n_plot] size_plot_agr;
  vector<lower=0>[n_pred] size_pred;
  vector[n] log_lik;
            
  for (i in 1:n_plot)
    mu_plot_growth[i] = b_spline_plot[i, 1] * h;
  for (i in 1:n_plot)
    size_plot[i] = exp(mu_plot_growth[i]);
  for (i in 1:n_plot)
    mu_plot_agr[i] = b_spline_deriv[i, 1] * h;
  for (i in 1:n_plot)
    size_plot_agr[i] = exp(mu_plot_agr[i]);
  for (i in 1:n_pred)
    mu_pred[i] = b_spline_pred[age_index_pred[i]] * h;
  for (i in 1:n_pred)
    size_pred[i] = exp(mu_pred[i]);
  for (i in 1:n)
    log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

