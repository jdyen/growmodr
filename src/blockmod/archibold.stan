data {
        int<lower=0> n;
        int<lower=0> n_block;
        int<lower=0> n_pred;
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
   vector[n_block] psi1;
  vector[n_block] psi2;
  vector[n_block] psi3;
   vector<lower=0>[n_block] h1;
  vector<lower=0>[n_block] h2;
  vector<lower=0>[n_block] h3;
   vector<lower=0>[n_block_pred] h1_holdout;
  vector<lower=0>[n_block_pred] h2_holdout;
  vector<lower=0>[n_block_pred] h3_holdout;
   }
        
        transformed parameters {
        vector[n] mu;
        for (i in 1:n)
        mu[i] =  h1[block_data[i]] / (h2[block_data[i]] + (h3[block_data[i]] ^ age[i])) ;
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
   h1_holdout ~ normal(psi_mean1, sd_h1);
  h2_holdout ~ normal(psi_mean2, sd_h2);
  h3_holdout ~ normal(psi_mean3, sd_h3);
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
        mu_plot_growth[i, j] =  h1[j] / (h2[j] + (h3[j] ^ age_plot[i])) ;
  for (i in 1:n_plot)
        for (j in 1:n_block)
        size_plot[i, j] = exp(mu_plot_growth[i, j]);
        for (i in 1:n_plot)
        for (j in 1:n_block)
        mu_plot_agr[i, j] =  -h1[j] * (h3[j] ^ age_plot[i]) * log(h3[j]) / ((h2[j] + (h3[j] ^ age_plot[i])) ^ 2) ;
  for (i in 1:n_plot)
        for (j in 1:n_block)
        size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
        for (i in 1:n_pred)
          mu_pred[i] =  h1_holdout[block_holdout[i]] / (h2_holdout[block_holdout[i]] + (h3_holdout[block_holdout[i]] ^ age_holdout[i, 1])) ;
  for (i in 1:n_pred)
          size_pred[i] = exp(mu_pred[i]);
        for (i in 1:n)
        log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

