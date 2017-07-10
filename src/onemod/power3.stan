data {
        int<lower=0> n;
        vector<lower=0>[n] size_data;
        vector[n] age;
        int<lower=0> n_plot;
        int<lower=0> n_pred;
        vector[n_plot] age_plot;
        matrix[n_pred, 1] age_holdout;
    }
        
        parameters {
        real<lower=0> sigma_obs;
 real<lower=0> h1;
  real<lower=0> h2;
  real<lower=0> h3;
   }
        
        transformed parameters {
        vector[n] mu;
 real<lower=0> h1_holdout;
real<lower=0> h2_holdout;
real<lower=0> h3_holdout;
   for (i in 1:n)
        mu[i] =  h1 * (age[i] ^ (h2 - (h3 / age[i]))) ;
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
        vector[n_plot] mu_plot_growth;
        vector[n_plot] mu_plot_agr;
        vector[n_pred] mu_pred;
        vector<lower=0>[n_plot] size_plot;
        vector<lower=0>[n_plot] size_plot_agr;
        vector<lower=0>[n_pred] size_pred;
        vector[n] log_lik;
        
        for (i in 1:n_plot)
        mu_plot_growth[i] =  h1 * (age_plot[i] ^ (h2 - (h3 / age_plot[i]))) ;
  for (i in 1:n_plot)
        size_plot[i] = exp(mu_plot_growth[i]);
        for (i in 1:n_plot)
        mu_plot_agr[i] =  mu_plot_growth[i] * (((h2 - (h3 / age_plot[i])) / age_plot[i]) + (h3 * log(age_plot[i])) / (age_plot[i] ^ 2)) ;
  for (i in 1:n_plot)
        size_plot_agr[i] = exp(mu_plot_agr[i]);
        for (i in 1:n_pred)
          mu_pred[i] =  h1_holdout * (age_holdout[i, 1] ^ (h2_holdout - (h3_holdout / age_holdout[i, 1]))) ;
  for (i in 1:n_pred)
          size_pred[i] = exp(mu_pred[i]);
        for (i in 1:n)
        log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

