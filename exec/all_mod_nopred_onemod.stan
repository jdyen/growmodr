data {
  int<lower=0> model_id;
  int<lower=0> n;
  int<lower=0> n_pred;
  vector<lower=0>[n] size_data;
  vector[n] age;
  int<lower=0> n_plot;
  vector[n_plot] age_plot;
  matrix[n_pred, 1] age_holdout;
}
      
parameters {
  real<lower=0> sigma_obs;
  vector<lower=0> h1;
  vector<lower=0> h2;
  vector<lower=0> h3;
}
      
transformed parameters {
  vector[n] mu;
  real<lower=0> h1_holdout;
  real<lower=0> h2_holdout;
  real<lower=0> h3_holdout;
  if (model_id == 1) { 
    for (i in 1:n)   // archibold
      mu[i] =  h1 / (h2 + (h3 ^ age[i]));
  }
  if (model_id == 2) { 
    for (i in 1:n)   // hillslope
      mu[i] =  h1 / (1 + exp(- * (age[i] - h3)));
  }
  if (model_id == 3) { 
    for (i in 1:n)   // hillslope_log
      mu[i] =  h1 / (1 + exp(-h2 * (log(age[i]) - h3)));
  }
  if (model_id == 4) { 
    for (i in 1:n)   // expo
      mu[i] =  h1 + (h2 * log(age[i]));
  }
  if (model_id == 5) { 
    for (i in 1:n)   // koblog
      mu[i] =  h1 * log(1 + (age[i] / h2));
  }
  if (model_id == 6) { 
    for (i in 1:n)   // logistic3
      mu[i] =  h1 / (1 + exp(-h2 * age[i] + h3));
  }
  if (model_id == 7) { 
    for (i in 1:n)   // monod
      mu[i] =  h1 * (age[i] / (h2 + age[i]));
  }
  if (model_id == 8) { 
    for (i in 1:n)   // neg_exp
      mu[i] =  h1 * (1 - exp(-h2 * age[i]));
  }
  if (model_id == 9) { 
    for (i in 1:n)   // power2
      mu[i] =  h1 * (age[i] ^ h2);
  }
  if (model_id == 10) { 
    for (i in 1:n)   // power3
      mu[i] =  h1 * (age[i] ^ (h2 - (h3 / age[i])));
  }
  if (model_id == 11) { 
    for (i in 1:n)   // weibull3
      mu[i] =  h1 * (1 - exp(-h2 * (age[i] ^ h3)));
  }
  h1_holdout = h1;
  h2_holdout = h2;
  h3_holdout = h3;
}
      
model {
  log(size_data) ~ normal(mu, sigma_obs);
  sigma_obs ~ normal(0, 2);
  h1 ~ normal(0.0, 2.0);
  h2 ~ normal(0.0, 2.0);
  h3 ~ normal(0.0, 2.0);
}
      
generated quantities {
  matrix[n_plot] mu_plot_growth;
  matrix[n_plot] mu_plot_agr;
  vector[n_pred] mu_pred;
  matrix<lower=0>[n_plot] size_plot;
  matrix<lower=0>[n_plot] size_plot_agr;
  vector<lower=0>[n_pred] size_pred;
  vector[n] log_lik;

  for (i in 1:n_plot) {
    if (model_id == 1) {   // archibold
      mu_plot_growth[i] =  h1 / (h2 + (h3 ^ age_plot[i]));
    }
    if (model_id == 2) {   // hillslope
      mu_plot_growth[i] =  h1 / (1 + exp(-h2 * (age_plot[i] - h3)));
    }
    if (model_id == 3) {   // hillslope_log
      mu_plot_growth[i] =  h1 / (1 + exp(-h2 * (log(age_plot[i]) - h3)));
    }
    if (model_id == 4) {   // expo
      mu_plot_growth[i] =  h1 + (h2 * log(age_plot[i]));
    }
    if (model_id == 5) {   // koblog
      mu_plot_growth[i] =  h1 * log(1 + (age_plot[i] / h2));
    }
    if (model_id == 6) {   // logistic3
      mu_plot_growth[i] =  h1 / (1 + exp(-h2 * age_plot[i] + h3));
    }
    if (model_id == 7) {   // monod
      mu_plot_growth[i] =  h1 * (age_plot[i] / (h2 + age_plot[i]));
    }
    if (model_id == 8) {   // neg_exp
      mu_plot_growth[i] =  h1 * (1 - exp(-h2 * age_plot[i]));
    }
    if (model_id == 9) {   // power2
      mu_plot_growth[i] =  h1 * (age_plot[i] ^ h2);
    }
    if (model_id == 10) {   // power3
      mu_plot_growth[i] =  h1 * (age_plot[i] ^ (h2 - (h3 / age_plot[i])));
    }
    if (model_id == 11) {   // weibull3
      mu_plot_growth[i] =  h1 * (1 - exp(-h2 * (age_plot[i] ^ h3)));
    }
  }
  for (i in 1:n_plot)
    size_plot[i] = exp(mu_plot_growth[i]);
  for (i in 1:n_plot) {
    if (model_id == 1) {   // archibold
      mu_plot_agr[i] =  -h1 * (h3 ^ age_plot[i]) * log(h3) / ((h2 + (h3 ^ age_plot[i])) ^ 2);
    }
    if (model_id == 2) {   // hillslope
      mu_plot_agr[i] =  mu_plot_growth[i] * (h2 * (exp(-h2 * (age_plot[i] - h3))) / (1 + exp(-h2 * (age_plot[i] - h3))));
    }
    if (model_id == 3) {   // hillslope_log
      mu_plot_agr[i] =  mu_plot_growth[i] * (h2 * (exp(-h2 * (log(age_plot[i]) - h3))) / (1 + exp(-h2 * (log(age_plot[i]) - h3))));
    }
    if (model_id == 4) {   // expo
      mu_plot_agr[i] =  h2 / age_plot[i];
    }
    if (model_id == 5) {   // koblog
      mu_plot_agr[i] =  h1 / (h2 + age_plot[i]);
    }
    if (model_id == 6) {   // logistic3
      mu_plot_agr[i] =  h2 * h1 / (2 * cosh(h3 - h2 * age_plot[i]) + 2);
    }
    if (model_id == 7) {   // monod
      mu_plot_agr[i] =  h1 * (h2 / ((h2 + age_plot[i]) ^ 2));
    }
    if (model_id == 8) {   // neg_exp
        mu_plot_agr[i] =  h1 * h2 * exp(-h2 * age_plot[i]);
    }
    if (model_id == 9) {   // power2
      mu_plot_agr[i] =  h1 * h2 * (age_plot[i] ^ (h2 - 1));
    }
    if (model_id == 10) {   // power3
      mu_plot_agr[i] =  mu_plot_growth[i] * (((h2 - (h3 / age_plot[i])) / age_plot[i]) + (h3 * log(age_plot[i])) / (age_plot[i] ^ 2));
    }
    if (model_id == 11) {   // weibull3
      mu_plot_agr[i] =  h1 * h2 * h3 * (age_plot[i] ^ (h3 - 1)) * exp(-h2 * (age_plot[i] ^ h3));
    }
  }
  for (i in 1:n_plot)
    size_plot_agr[i] = exp(mu_plot_agr[i]);
  for (i in 1:n_pred) {
    if (model_id == 1) {   // archibold
      mu_pred[i] =  h1_holdout / (h2_holdout + (h3_holdout ^ age_holdout[i, 1]));
    }
    if (model_id == 2) {   // hillslope
      mu_pred[i] =  h1_holdout / (1 + exp(-h2_holdout * (age_holdout[i, 1] - h3_holdout)));
    }
    if (model_id == 3) {   // hillslope_log
      mu_pred[i] =  h1_holdout / (1 + exp(-h2_holdout * (log(age_holdout[i, 1]) - h3_holdout)));
    }
    if (model_id == 4) {   // expo
      mu_pred[i] =  h1_holdout + (h2_holdout * log(age_holdout[i, 1]));
    }
    if (model_id == 5) {   // koblog
      mu_pred[i] =  h1_holdout * log(1 + (age_holdout[i, 1] / h2_holdout));
    }
    if (model_id == 6) {   // logistic3
      mu_pred[i] =  h1_holdout / (1 + exp(-h2_holdout * age_holdout[i, 1] + h3_holdout));
    }
    if (model_id == 7) {   // monod
      mu_pred[i] =  h1_holdout * (age_holdout[i, 1] / (h2_holdout + age_holdout[i, 1]));
    }
    if (model_id == 8) {   // neg_exp
      mu_pred[i] =  h1_holdout * (1 - exp(-h2_holdout * age_holdout[i, 1]));
    }
    if (model_id == 9) {   // power2
      mu_pred[i] =  h1_holdout * (age_holdout[i, 1] ^ h2_holdout);
    }
    if (model_id == 10) {   // power3
      mu_pred[i] =  h1_holdout * (age_holdout[i, 1] ^ (h2_holdout - (h3_holdout / age_holdout[i, 1])));
    }
    if (model_id == 11) {   // weibull3
      mu_pred[i] =  h1_holdout * (1 - exp(-h2_holdout * (age_holdout[i, 1] ^ h3_holdout)));
    }
  }
  for (i in 1:n_pred)
    size_pred[i] = exp(mu_pred[i]);
  for (i in 1:n)
    log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

