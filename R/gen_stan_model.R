# could extend with 
#   for (i in 1:num_par) { paste0('psi', i, ';\n')}
gen_mod_file <- function(model) {
  if (model != 'spline') {
    mod.params <- get(paste0(model, '_param_fetch'))()
    three.par <- mod.params$three.par
    mod.mu <- mod.params$mu
    mu.plot <- mod.params$mu.plot
    mu.plot.agr <- mod.params$mu.agr
    cat(
      'data {
      int<lower=0> n;
      int<lower=0> n_sp;
      int<lower=0> n_xh;
      int<lower=0> n_xa;
      vector<lower=0>[n] h;
      vector[n] t;
      int<lower=0, upper=n_sp> sp[n];
      matrix[n_sp, n_xh] x_h;
      matrix[n_sp, n_xa] x_a;\n',
      if (three.par) {
        ' int<lower=0> n_xb;\n'
      },
      if (three.par) {
        ' matrix[n_sp, n_xb] x_b;\n'
      },
      ' int<lower=0> n_plot;
      vector[n_plot] t_plot;
  }
      
      parameters {
      real<lower=0> sigma_obs;
      real<lower=0> sd_h;
      real<lower=0> sd_a;\n',
      if (three.par) {
        ' real<lower=0> sd_b;\n'
      },
      if (three.par) {
        ' vector[n_xb] b2;\n'
      },
      if (three.par) {
        ' vector<lower=0>[n_sp] b;\n'
      },
      ' vector<lower=0>[n_sp] hm;
      vector<lower=0>[n_sp] a;
      vector[n_xh] b0;
      vector[n_xa] b1;
      }
      
      transformed parameters {
      vector[n] mu;
      vector[n_sp] psi_h;
      vector[n_sp] psi_a;\n',
      if (three.par) {
        ' vector[n_sp] psi_b;\n'
      },
      ' for (i in 1:n)
      mu[i] = ', mod.mu, ';\n',
      ' psi_h = x_h * b0;
      psi_a = x_a * b1;\n',
      if (three.par) {
        ' psi_b = x_b * b2;\n'
      },
      '}
      
      model {
      log(h) ~ normal(mu, sigma_obs);
      hm ~ normal(psi_h, sd_h);
      a ~ normal(psi_a, sd_a);\n',
      if (three.par) {
        ' b ~ normal(psi_b, sd_b);\n'
      },
      if (three.par) {
        ' b2 ~ normal(0, 10);\n'
      },
      if (three.par) {
        ' sd_b ~ normal(0, 2);\n'
      },
      ' b0 ~ normal(0, 10);
      b1 ~ normal(0, 10);
      sd_h ~ normal(0, 2);
      sd_a ~ normal(0, 2);
      }
      
      generated quantities {
      matrix[n_plot, n_sp] mu_plot_growth;
      matrix[n_plot, n_sp] mu_plot_agr;
      matrix<lower=0>[n_plot, n_sp] h_plot;
      matrix<lower=0>[n_plot, n_sp] h_plot_agr;
      vector[n] log_lik;
      
      for (i in 1:n_plot)
      for (j in 1:n_sp)
      mu_plot_growth[i, j] = ', mu.plot, ';\n',
      ' for (i in 1:n_plot)
      for (j in 1:n_sp)
      h_plot[i, j] = exp(mu_plot_growth[i, j]);
      for (i in 1:n_plot)
      for (j in 1:n_sp)
      mu_plot_agr[i, j] = ', mu.plot.agr, ';\n',
      ' for (i in 1:n_plot)
      for (j in 1:n_sp)
      h_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
      for (i in 1:n)
      log_lik[i] = normal_lpdf(log(h[i]) | mu[i], sigma_obs);
      }',
  file = (mod.file <- tempfile()))
    } else {
      cat(
        'data {
        int<lower=0> n;
        int<lower=0> n_sp;
        int<lower=0> n_x;
        vector<lower=0>[n] h;
        vector[n] t;
        int<lower=0> n_t;
        int<lower=0> n_k;
        int<lower=0> t_index[n];
        int<lower=0, upper=n_sp> sp[n];
        matrix[n_sp, n_x] x;
        row_vector[n_k] b_spline[n_t];
        int<lower=0> n_plot;
        vector[n_plot] t_plot;
        row_vector[n_k] b_spline_plot[n_plot, n_sp];
        row_vector[n_k] b_spline_deriv[n_plot, n_sp];
        }
        
        parameters {
        real<lower=0> sigma_obs;
        vector<lower=0>[n_k] sd_param;
        vector[n_k] beta[n_sp];
        matrix[n_x, n_k] b_param;
        }
        
        transformed parameters {
        vector[n] mu;
        matrix[n_sp, n_k] psi;
        for (i in 1:n)
        mu[i] = b_spline[t_index[i]] * beta[sp[i]];
        psi = x * b_param;
        }
        
        model {
        log(h) ~ normal(mu, sigma_obs);
        for (j in 1:n_sp)
        beta[j] ~ normal(psi[j], sd_param);
        for (j in 1:n_x)
        b_param[j] ~ normal(0, 1);
        sd_param ~ normal(0, 2);
        sigma_obs ~ normal(0, 2);
        }
        
        generated quantities {
        matrix[n_plot, n_sp] mu_plot_growth;
        matrix[n_plot, n_sp] mu_plot_agr;
        matrix<lower=0>[n_plot, n_sp] h_plot;
        matrix<lower=0>[n_plot, n_sp] h_plot_agr;
        vector[n] log_lik;
        
        for (i in 1:n_plot)
        for (j in 1:n_sp)
        mu_plot_growth[i, j] = b_spline_plot[i, j] * beta[j];
        for (i in 1:n_plot)
        for (j in 1:n_sp)
        h_plot[i, j] = exp(mu_plot_growth[i, j]);
        for (i in 1:n_plot)
        for (j in 1:n_sp)
        mu_plot_agr[i, j] = b_spline_deriv[i, j] * beta[j];
        for (i in 1:n_plot)
        for (j in 1:n_sp)
        h_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
        for (i in 1:n)
        log_lik[i] = normal_lpdf(log(h[i]) | mu[i], sigma_obs);
        }\n',
  file = (mod.file <- tempfile()))
  }
  mod.file
}