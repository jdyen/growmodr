data {
  int<lower=0> model_id;
  int<lower=0> n;
  int<lower=0> n_pred;
  int<lower=0> n_block;
  int<lower=0> n_block_pred;
  vector<lower=0>[n] size_data;
  vector[n] age;
  int<lower=0, upper=n_block> block_data[n];
  int<lower=0, upper=n_block_pred> block_holdout[n_pred];
  int<lower=0> n_plot;
  vector[n_plot] age_plot;
  matrix[n_pred, 1] age_holdout;
}
      
parameters {
  real<lower=0> sigma_obs;
  real<lower=0> block_sd1;
  real<lower=0> block_sd2;
  real<lower=0> block_sd3;
  real psi_mean1;
  real psi_mean2;
  real psi_mean3;
  real<lower=0> sd_h1;
  real<lower=0> sd_h2;
  real<lower=0> sd_h3;
  vector<lower=0>[n_block] h1;
  vector<lower=0>[n_block] h2;
  vector<lower=0>[n_block] h3;
  vector<lower=0>[n_block_pred] h1_holdout;
  vector<lower=0>[n_block_pred] h2_holdout;
  vector<lower=0>[n_block_pred] h3_holdout;
}
      
transformed parameters {
  vector[n] mu;
  if (model_id == 1) { 
    for (i in 1:n)   // archibold
      mu[i] =  h1[block_data[i]] / (h2[block_data[i]] + (h3[block_data[i]] ^ age[i]));
  }
  if (model_id == 2) { 
    for (i in 1:n)   // hillslope
      mu[i] =  h1[block_data[i]] / (1 + exp(-h2[block_data[i]] * (age[i] - h3[block_data[i]])));
  }
  if (model_id == 3) { 
    for (i in 1:n)   // hillslope_log
      mu[i] =  h1[block_data[i]] / (1 + exp(-h2[block_data[i]] * (log(age[i]) - h3[block_data[i]])));
  }
  if (model_id == 4) { 
    for (i in 1:n)   // expo
      mu[i] =  h1[block_data[i]] + (h2[block_data[i]] * log(age[i]));
  }
  if (model_id == 5) { 
    for (i in 1:n)   // koblog
      mu[i] =  h1[block_data[i]] * log(1 + (age[i] / h2[block_data[i]]));
  }
  if (model_id == 6) { 
    for (i in 1:n)   // logistic3
      mu[i] =  h1[block_data[i]] / (1 + exp(-h2[block_data[i]] * age[i] + h3[block_data[i]]));
  }
  if (model_id == 7) { 
    for (i in 1:n)   // monod
      mu[i] =  h1[block_data[i]] * (age[i] / (h2[block_data[i]] + age[i]));
  }
  if (model_id == 8) { 
    for (i in 1:n)   // neg_exp
      mu[i] =  h1[block_data[i]] * (1 - exp(-h2[block_data[i]] * age[i]));
  }
  if (model_id == 9) { 
    for (i in 1:n)   // power2
      mu[i] =  h1[block_data[i]] * (age[i] ^ h2[block_data[i]]);
  }
  if (model_id == 10) { 
    for (i in 1:n)   // power3
      mu[i] =  h1[block_data[i]] * (age[i] ^ (h2[block_data[i]] - (h3[block_data[i]] / age[i])));
  }
  if (model_id == 11) { 
    for (i in 1:n)   // weibull3
      mu[i] =  h1[block_data[i]] * (1 - exp(-h2[block_data[i]] * (age[i] ^ h3[block_data[i]])));
  }
}
      
model {
  log(size_data) ~ normal(mu, sigma_obs);
  sigma_obs ~ normal(0, 2);
  psi1 ~ normal(psi_mean1, block_sd1);
  psi2 ~ normal(psi_mean2, block_sd2);
  psi3 ~ normal(psi_mean3, block_sd3);
  block_sd1 ~ normal(0.0, 2.0);
  block_sd2 ~ normal(0.0, 2.0);
  block_sd3 ~ normal(0.0, 2.0);
  psi_mean1 ~ normal(0.0, 2.0);
  psi_mean2 ~ normal(0.0, 2.0);
  psi_mean3 ~ normal(0.0, 2.0);
  h1 ~ normal(psi1, sd_h1);
  h2 ~ normal(psi2, sd_h2);
  h3 ~ normal(psi3, sd_h3);
  h1_holdout ~ normal(psi1_pred, sd_h1);
  h2_holdout ~ normal(psi2_pred, sd_h2);
  h3_holdout ~ normal(psi3_pred, sd_h3);
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

  for (i in 1:n_plot) {
    for (j in 1:n_block) {
      if (model_id == 1) {   // archibold
        mu_plot_growth[i, j] =  h1[j] / (h2[j] + (h3[j] ^ age_plot[i]));
      }
      if (model_id == 2) {   // hillslope
        mu_plot_growth[i, j] =  h1[j] / (1 + exp(-h2[j] * (age_plot[i] - h3[j])));
      }
      if (model_id == 3) {   // hillslope_log
        mu_plot_growth[i, j] =  h1[j] / (1 + exp(-h2[j] * (log(age_plot[i]) - h3[j])));
      }
      if (model_id == 4) {   // expo
        mu_plot_growth[i, j] =  h1[j] + (h2[j] * log(age_plot[i]));
      }
      if (model_id == 5) {   // koblog
        mu_plot_growth[i, j] =  h1[j] * log(1 + (age_plot[i] / h2[j]));
      }
      if (model_id == 6) {   // logistic3
        mu_plot_growth[i, j] =  h1[j] / (1 + exp(-h2[j] * age_plot[i] + h3[j]));
      }
      if (model_id == 7) {   // monod
        mu_plot_growth[i, j] =  h1[j] * (age_plot[i] / (h2[j] + age_plot[i]));
      }
      if (model_id == 8) {   // neg_exp
        mu_plot_growth[i, j] =  h1[j] * (1 - exp(-h2[j] * age_plot[i]));
      }
      if (model_id == 9) {   // power2
        mu_plot_growth[i, j] =  h1[j] * (age_plot[i] ^ h2[j]);
      }
      if (model_id == 10) {   // power3
        mu_plot_growth[i, j] =  h1[j] * (age_plot[i] ^ (h2[j] - (h3[j] / age_plot[i])));
      }
      if (model_id == 11) {   // weibull3
        mu_plot_growth[i, j] =  h1[j] * (1 - exp(-h2[j] * (age_plot[i] ^ h3[j])));
      }
    }
  }
  for (i in 1:n_plot)
    for (j in 1:n_block)
      size_plot[i, j] = exp(mu_plot_growth[i, j]);
  for (i in 1:n_plot) {
    for (j in 1:n_block) {
      if (model_id == 1) {   // archibold
        mu_plot_agr[i, j] =  -h1[j] * (h3[j] ^ age_plot[i]) * log(h3[j]) / ((h2[j] + (h3[j] ^ age_plot[i])) ^ 2);
      }
      if (model_id == 2) {   // hillslope
        mu_plot_agr[i, j] =  mu_plot_growth[i, j] * (h2[j] * (exp(-h2[j] * (age_plot[i] - h3[j]))) / (1 + exp(-h2[j] * (age_plot[i] - h3[j]))));
      }
      if (model_id == 3) {   // hillslope_log
        mu_plot_agr[i, j] =  mu_plot_growth[i, j] * (h2[j] * (exp(-h2[j] * (log(age_plot[i]) - h3[j]))) / (1 + exp(-h2[j] * (log(age_plot[i]) - h3[j]))));
      }
      if (model_id == 4) {   // expo
        mu_plot_agr[i, j] =  h2[j] / age_plot[i];
      }
      if (model_id == 5) {   // koblog
        mu_plot_agr[i, j] =  h1[j] / (h2[j] + age_plot[i]);
      }
      if (model_id == 6) {   // logistic3
        mu_plot_agr[i, j] =  h2[j] * h1[j] / (2 * cosh(h3[j] - h2[j] * age_plot[i]) + 2);
      }
      if (model_id == 7) {   // monod
        mu_plot_agr[i, j] =  h1[j] * (h2[j] / ((h2[j] + age_plot[i]) ^ 2));
      }
      if (model_id == 8) {   // neg_exp
        mu_plot_agr[i, j] =  h1[j] * h2[j] * exp(-h2[j] * age_plot[i]);
      }
      if (model_id == 9) {   // power2
        mu_plot_agr[i, j] =  h1[j] * h2[j] * (age_plot[i] ^ (h2[j] - 1));
      }
      if (model_id == 10) {   // power3
        mu_plot_agr[i, j] =  mu_plot_growth[i, j] * (((h2[j] - (h3[j] / age_plot[i])) / age_plot[i]) + (h3[j] * log(age_plot[i])) / (age_plot[i] ^ 2));
      }
      if (model_id == 11) {   // weibull3
        mu_plot_agr[i, j] =  h1[j] * h2[j] * h3[j] * (age_plot[i] ^ (h3[j] - 1)) * exp(-h2[j] * (age_plot[i] ^ h3[j]));
      }
    }
  }
  for (i in 1:n_plot)
    for (j in 1:n_block)
      size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
  for (i in 1:n_pred) {
    if (model_id == 1) {   // archibold
      mu_pred[i] =  h1_holdout[block_holdout[i]] / (h2_holdout[block_holdout[i]] + (h3_holdout[block_holdout[i]] ^ age_holdout[i, 1]));
    }
    if (model_id == 2) {   // hillslope
      mu_pred[i] =  h1_holdout[block_holdout[i]] / (1 + exp(-h2_holdout[block_holdout[i]] * (age_holdout[i, 1] - h3_holdout[block_holdout[i]])));
    }
    if (model_id == 3) {   // hillslope_log
      mu_pred[i] =  h1_holdout[block_holdout[i]] / (1 + exp(-h2_holdout[block_holdout[i]] * (log(age_holdout[i, 1]) - h3_holdout[block_holdout[i]])));
    }
    if (model_id == 4) {   // expo
      mu_pred[i] =  h1_holdout[block_holdout[i]] + (h2_holdout[block_holdout[i]] * log(age_holdout[i, 1]));
    }
    if (model_id == 5) {   // koblog
      mu_pred[i] =  h1_holdout[block_holdout[i]] * log(1 + (age_holdout[i, 1] / h2_holdout[block_holdout[i]]));
    }
    if (model_id == 6) {   // logistic3
      mu_pred[i] =  h1_holdout[block_holdout[i]] / (1 + exp(-h2_holdout[block_holdout[i]] * age_holdout[i, 1] + h3_holdout[block_holdout[i]]));
    }
    if (model_id == 7) {   // monod
      mu_pred[i] =  h1_holdout[block_holdout[i]] * (age_holdout[i, 1] / (h2_holdout[block_holdout[i]] + age_holdout[i, 1]));
    }
    if (model_id == 8) {   // neg_exp
      mu_pred[i] =  h1_holdout[block_holdout[i]] * (1 - exp(-h2_holdout[block_holdout[i]] * age_holdout[i, 1]));
    }
    if (model_id == 9) {   // power2
      mu_pred[i] =  h1_holdout[block_holdout[i]] * (age_holdout[i, 1] ^ h2_holdout[block_holdout[i]]);
    }
    if (model_id == 10) {   // power3
      mu_pred[i] =  h1_holdout[block_holdout[i]] * (age_holdout[i, 1] ^ (h2_holdout[block_holdout[i]] - (h3_holdout[block_holdout[i]] / age_holdout[i, 1])));
    }
    if (model_id == 11) {   // weibull3
      mu_pred[i] =  h1_holdout[block_holdout[i]] * (1 - exp(-h2_holdout[block_holdout[i]] * (age_holdout[i, 1] ^ h3_holdout[block_holdout[i]])));
    }
  }
  for (i in 1:n_pred)
    size_pred[i] = exp(mu_pred[i]);
  for (i in 1:n)
    log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

