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
  real h1;
  real h2;
  real h3;
  real h4;
  real h5;
  real h6;
  real h7;
  real h8;
  real h9;
  real h10;
  real h11;
  real h12;
  real h13;
  real h14;
  real h15;
  real h16;
  real h17;
  real h18;
}
            
transformed parameters {
  vector[n] mu;
  for (i in 1:n)
    mu[i] = b_spline[age_index[i]][1] * h1 + b_spline[age_index[i]][2] * h2 + b_spline[age_index[i]][3] * h3 + b_spline[age_index[i]][4] * h4 + b_spline[age_index[i]][5] * h5 + b_spline[age_index[i]][6] * h6 + b_spline[age_index[i]][7] * h7 + b_spline[age_index[i]][8] * h8 + b_spline[age_index[i]][9] * h9 + b_spline[age_index[i]][10] * h10 + b_spline[age_index[i]][11] * h11 + b_spline[age_index[i]][12] * h12 + b_spline[age_index[i]][13] * h13 + b_spline[age_index[i]][14] * h14 + b_spline[age_index[i]][15] * h15 + b_spline[age_index[i]][16] * h16 + b_spline[age_index[i]][17] * h17 + b_spline[age_index[i]][18] * h18;
}
            
model {
  log(size_data) ~ normal(mu, sigma_obs);
  sigma_obs ~ normal(0, 2);
  h1~ normal(0.0, 2.0);
  h2~ normal(0.0, 2.0);
  h3~ normal(0.0, 2.0);
  h4~ normal(0.0, 2.0);
  h5~ normal(0.0, 2.0);
  h6~ normal(0.0, 2.0);
  h7~ normal(0.0, 2.0);
  h8~ normal(0.0, 2.0);
  h9~ normal(0.0, 2.0);
  h10~ normal(0.0, 2.0);
  h11~ normal(0.0, 2.0);
  h12~ normal(0.0, 2.0);
  h13~ normal(0.0, 2.0);
  h14~ normal(0.0, 2.0);
  h15~ normal(0.0, 2.0);
  h16~ normal(0.0, 2.0);
  h17~ normal(0.0, 2.0);
  h18~ normal(0.0, 2.0);
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
    mu_plot_growth[i] = b_spline_plot[i, 1][1] * h1 + b_spline_plot[i, 1][2] * h2 + b_spline_plot[i, 1][3] * h3 + b_spline_plot[i, 1][4] * h4 + b_spline_plot[i, 1][5] * h5 + b_spline_plot[i, 1][6] * h6 + b_spline_plot[i, 1][7] * h7 + b_spline_plot[i, 1][8] * h8 + b_spline_plot[i, 1][9] * h9 + b_spline_plot[i, 1][10] * h10 + b_spline_plot[i, 1][11] * h11 + b_spline_plot[i, 1][12] * h12 + b_spline_plot[i, 1][13] * h13 + b_spline_plot[i, 1][14] * h14 + b_spline_plot[i, 1][15] * h15 + b_spline_plot[i, 1][16] * h16 + b_spline_plot[i, 1][17] * h17 + b_spline_plot[i, 1][18] * h18;
  for (i in 1:n_plot)
    size_plot[i] = exp(mu_plot_growth[i]);
  for (i in 1:n_plot)
    mu_plot_agr[i] = b_spline_deriv[i, 1][1] * h1 + b_spline_deriv[i, 1][2] * h2 + b_spline_deriv[i, 1][3] * h3 + b_spline_deriv[i, 1][4] * h4 + b_spline_deriv[i, 1][5] * h5 + b_spline_deriv[i, 1][6] * h6 + b_spline_deriv[i, 1][7] * h7 + b_spline_deriv[i, 1][8] * h8 + b_spline_deriv[i, 1][9] * h9 + b_spline_deriv[i, 1][10] * h10 + b_spline_deriv[i, 1][11] * h11 + b_spline_deriv[i, 1][12] * h12 + b_spline_deriv[i, 1][13] * h13 + b_spline_deriv[i, 1][14] * h14 + b_spline_deriv[i, 1][15] * h15 + b_spline_deriv[i, 1][16] * h16 + b_spline_deriv[i, 1][17] * h17 + b_spline_deriv[i, 1][18] * h18;
  for (i in 1:n_plot)
    size_plot_agr[i] = exp(mu_plot_agr[i]);
  for (i in 1:n_pred)
    mu_pred[i] = b_spline_pred[age_index_pred[i]][1] * h1 + b_spline_pred[age_index_pred[i]][2] * h2 + b_spline_pred[age_index_pred[i]][3] * h3 + b_spline_pred[age_index_pred[i]][4] * h4 + b_spline_pred[age_index_pred[i]][5] * h5 + b_spline_pred[age_index_pred[i]][6] * h6 + b_spline_pred[age_index_pred[i]][7] * h7 + b_spline_pred[age_index_pred[i]][8] * h8 + b_spline_pred[age_index_pred[i]][9] * h9 + b_spline_pred[age_index_pred[i]][10] * h10 + b_spline_pred[age_index_pred[i]][11] * h11 + b_spline_pred[age_index_pred[i]][12] * h12 + b_spline_pred[age_index_pred[i]][13] * h13 + b_spline_pred[age_index_pred[i]][14] * h14 + b_spline_pred[age_index_pred[i]][15] * h15 + b_spline_pred[age_index_pred[i]][16] * h16 + b_spline_pred[age_index_pred[i]][17] * h17 + b_spline_pred[age_index_pred[i]][18] * h18;
  for (i in 1:n_pred)
    size_pred[i] = exp(mu_pred[i]);
  for (i in 1:n)
    log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

