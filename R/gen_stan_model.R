# generate stan model file
gen_mod_file <- function(model,
                         spline_params = NULL,
                         mod_file = NULL,
                         include_pred = TRUE,
                         include_block = TRUE) {
  if (include_pred & !include_block) {
    stop('model cannot have predictor variables without a blocking variable.',
         call. = FALSE)
  }
  if (!include_block) {
    if (model != 'spline') {
      mod_par <- get(paste0(model, '_param_fetch'))(include_block = include_block)
      num_param <- mod_par$num_par
      mod_mu <- mod_par$mu
      mu_plot <- mod_par$mu_plot
      mu_plot_agr <- mod_par$mu_agr
      mu_pred <- mod_par$mu_holdout
      h_var <- NULL
      h_prior <- NULL
      h_holdout <- NULL
      h_pred <- NULL
      for (i in 1:num_param) {
        h_var <- paste0(h_var, 'real<lower=0> h', i, ';\n  ')
        h_prior <- paste0(h_prior, 'h', i, ' ~ normal(0.0, 2.0);\n  ')
        h_holdout <- paste0(h_holdout, 'real<lower=0> h', i, '_holdout;\n')
        h_pred <- paste0(h_pred, 'h', i, '_holdout = h', i, ';\n')
      }
      cat(
        'data {
        int<lower=0> n;
        vector<lower=0>[n] size_data;
        vector[n] age;
        int<lower=0> n_plot;
        int<lower=0> n_pred;
        vector[n_plot] age_plot;
        matrix[n_pred, 1] age_holdout;
    }
        
        parameters {
        real<lower=0> sigma_obs;\n',
        h_var,
        '}
        
        transformed parameters {
        vector[n] mu;\n',
        h_holdout,
        '  for (i in 1:n)
        mu[i] = ', mod_mu, ';\n',
        h_pred,
        '}
        
        model {
        log(size_data) ~ normal(mu, sigma_obs);
        sigma_obs ~ normal(0, 2);\n',
        h_prior,
        '}
        
        generated quantities {
        vector[n_plot] mu_plot_growth;
        vector[n_plot] mu_plot_agr;
        vector[n_pred] mu_pred;
        vector<lower=0>[n_plot] size_plot;
        vector<lower=0>[n_plot] size_plot_agr;
        vector<lower=0>[n_pred] size_pred;
        vector[n] log_lik;
        
        for (i in 1:n_plot)
        mu_plot_growth[i] = ', mu_plot, ';\n',
        ' for (i in 1:n_plot)
        size_plot[i] = exp(mu_plot_growth[i]);
        for (i in 1:n_plot)
        mu_plot_agr[i] = ', mu_plot_agr, ';\n',
        ' for (i in 1:n_plot)
        size_plot_agr[i] = exp(mu_plot_agr[i]);
        for (i in 1:n_pred)
          mu_pred[i] = ', mu_pred, ';\n',
        ' for (i in 1:n_pred)
          size_pred[i] = exp(mu_pred[i]);
        for (i in 1:n)
        log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
        }\n\n',
        file = ifelse(is.null(mod_file), mod_file <- paste0(tempfile(), '.stan'), mod_file))
    } else {
      if (!is.null(spline_params)) {
        num_param <- spline_params$n_knots + spline_params$degree
      } else {
        stop('spline_params must be passed to gen_mod_file.',
             call. = FALSE)
      }
      h_var <- NULL
      h_prior <- NULL
      h_holdout <- NULL
      h_pred <- NULL
      mu_var <- 'mu[i] = b_spline[age_index[i]][1] * h1'
      mu_var_plot <- 'mu_plot_growth[i] = b_spline_plot[i, 1][1] * h1'
      mu_var_agr <- 'mu_plot_agr[i] = b_spline_deriv[i, 1][1] * h1'
      mu_pred <- 'mu_pred[i] = b_spline_pred[age_index_pred[i]][1] * h1'
      for (i in 1:num_param) {
        if (i > 1) {
          mu_var <- paste0(mu_var, ' + b_spline[age_index[i]][', i, '] * h', i)
          mu_var_plot <- paste0(mu_var_plot, ' + b_spline_plot[i, 1][', i, '] * h', i)
          mu_var_agr <- paste0(mu_var_agr, ' + b_spline_deriv[i, 1][', i, '] * h', i)
          mu_pred <- paste0(mu_pred, ' + b_spline_pred[age_index_pred[i]][', i, '] * h', i)
        }
        h_var <- paste0(h_var, 'real h', i, ';\n  ')
        h_prior <- paste0(h_prior, 'h', i, '~ normal(0.0, 2.0);\n  ')
      }
      mu_var <- paste0(mu_var, ';\n  ')
      mu_var_plot <- paste0(mu_var_plot, ';\n  ')
      mu_var_agr <- paste0(mu_var_agr, ';\n  ')
      mu_pred <- paste0(mu_pred, ';\n  ')
      cat(
        'data {
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
          real<lower=0> sigma_obs;\n',
          h_var,
        '}
            
        transformed parameters {
          vector[n] mu;
          for (i in 1:n)\n',
              mu_var,
        '}
            
        model {
            log(size_data) ~ normal(mu, sigma_obs);
            sigma_obs ~ normal(0, 2);\n',
        h_prior,
        '}
            
        generated quantities {
            vector[n_plot] mu_plot_growth;
            vector[n_plot] mu_plot_agr;
            vector[n_pred] mu_pred;
            vector<lower=0>[n_plot] size_plot;
            vector<lower=0>[n_plot] size_plot_agr;
            vector<lower=0>[n_pred] size_pred;
            vector[n] log_lik;
            
            for (i in 1:n_plot)\n',
        mu_var_plot,
        'for (i in 1:n_plot)
            size_plot[i] = exp(mu_plot_growth[i]);
            for (i in 1:n_plot)\n',
        mu_var_agr,
        'for (i in 1:n_plot)
            size_plot_agr[i] = exp(mu_plot_agr[i]);
            for (i in 1:n_pred)\n',
        mu_pred,
        'for (i in 1:n_pred)
        size_pred[i] = exp(mu_pred[i]);
        for (i in 1:n)
            log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
            }\n\n',
        file = ifelse(is.null(mod_file), mod_file <- paste0(tempfile(), '.stan'), mod_file))
    }    
  } else {
    if (include_pred) {
      if (model != 'spline') {
        mod_par <- get(paste0(model, '_param_fetch'))()
        num_param <- mod_par$num_par
        mod_mu <- mod_par$mu
        mu_plot <- mod_par$mu_plot
        mu_plot_agr <- mod_par$mu_agr
        mu_pred <- mod_par$mu_holdout
        h_var <- NULL
        sdh_var <- NULL
        nxh_var <- NULL
        x_var <- NULL
        b_var <- NULL
        psi_var <- NULL
        psi_reg_var <- NULL
        b_prior <- NULL
        h_prior <- NULL
        h_var_pred <- NULL
        x_var_pred <- NULL
        psi_var_pred <- NULL
        psi_reg_var_pred <- NULL
        h_prior_pred <- NULL
        sdh_prior <- NULL
        for (i in 1:num_param) {
          h_var <- paste0(h_var, 'vector<lower=0>[n_block] h', i, ';\n  ')
          nxh_var <- paste0(nxh_var, 'int<lower=0> n_x', i, ';\n  ')
          sdh_var <- paste0(sdh_var, 'real<lower=0> sd_h', i, ';\n  ')
          x_var <- paste0(x_var, 'matrix[n_block, n_x', i, '] x', i, ';\n  ')
          b_var <- paste0(b_var, 'vector[n_x', i, '] b', i, ';\n  ')
          psi_var <- paste0(psi_var, 'vector[n_block] psi', i, ';\n  ')
          psi_reg_var <- paste0(psi_reg_var, 'psi', i, ' = x', i, ' * b', i, ';\n  ')
          h_prior <- paste0(h_prior, 'h', i, ' ~ normal(psi', i, ', sd_h', i, ');\n  ')
          b_prior <- paste0(b_prior, 'b', i, ' ~ normal(0.0, 10.0);\n  ')
          h_var_pred <- paste0(h_var_pred, 'vector<lower=0>[n_block_pred] h', i, '_holdout;\n  ')
          x_var_pred <- paste0(x_var_pred, 'matrix[n_block_pred, n_x', i, '] x', i, '_pred;\n  ')
          psi_var_pred <- paste0(psi_var_pred, 'vector[n_block_pred] psi', i, '_pred;\n  ')
          psi_reg_var_pred <- paste0(psi_reg_var_pred, 'psi', i, '_pred = x', i, '_pred * b', i, ';\n  ')
          h_prior_pred <- paste0(h_prior_pred, 'h', i, '_holdout ~ normal(psi', i, '_pred, sd_h', i, ');\n  ')
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
        }
        cat(
          'data {
      int<lower=0> n;
      int<lower=0> n_pred;
      int<lower=0> n_block;
      int<lower=0> n_block_pred;\n',
          nxh_var,
          x_var,
          x_var_pred,
          'vector<lower=0>[n] size_data;
      vector[n] age;
      matrix[n_pred, 1] age_holdout;
      int<lower=0, upper=n_block> block_data[n];
      int<lower=0, upper=n_block_pred> block_holdout[n_pred];
      int<lower=0> n_plot;
      vector[n_plot] age_plot;
  }
      
      parameters {
      real<lower=0> sigma_obs;\n',
          sdh_var,
          h_var,
          h_var_pred,
          b_var,
          '}
      
      transformed parameters {
      vector[n] mu;\n',
          psi_var,
          psi_var_pred,
          'for (i in 1:n)
      mu[i] = ', mod_mu, ';\n',
          psi_reg_var,
          psi_reg_var_pred,
          '}
      
      model {
      log(size_data) ~ normal(mu, sigma_obs);
      sigma_obs ~ normal(0, 2);\n',
          h_prior,
          h_prior_pred,
          b_prior,
          sdh_prior,
          '}
      
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
      mu_plot_growth[i, j] = ', mu_plot, ';\n',
          ' for (i in 1:n_plot)
      for (j in 1:n_block)
      size_plot[i, j] = exp(mu_plot_growth[i, j]);
      for (i in 1:n_plot)
      for (j in 1:n_block)
      mu_plot_agr[i, j] = ', mu_plot_agr, ';\n',
          ' for (i in 1:n_plot)
      for (j in 1:n_block)
      size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
      for (i in 1:n_pred)
          mu_pred[i] = ', mu_pred, ';\n',
          ' for (i in 1:n_pred)
          size_pred[i] = exp(mu_pred[i]);
          for (i in 1:n)
      log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
      }\n\n',
          file = ifelse(is.null(mod_file), mod_file <- paste0(tempfile(), '.stan'), mod_file))
      } else {
        if (!is.null(spline_params)) {
          num_param <- spline_params$n_knots + spline_params$degree
        } else {
          stop('spline_params must be passed to gen_mod_file.',
               call. = FALSE)
        }
        h_var <- NULL
        sdh_var <- NULL
        nxh_var <- NULL
        x_var <- NULL
        b_var <- NULL
        psi_var <- NULL
        psi_reg_var <- NULL
        b_prior <- NULL
        h_prior <- NULL
        sdh_prior <- NULL
        h_var_pred <- NULL
        x_var_pred <- NULL
        psi_var_pred <- NULL
        psi_reg_var_pred <- NULL
        h_prior_pred <- NULL
        mu_var <- 'mu[i] = b_spline[age_index[i]][1] * h1[block_data[i]]'
        mu_var_plot <- 'mu_plot_growth[i, j] = b_spline_plot[i, j][1] * h1[j]'
        mu_var_agr <- 'mu_plot_agr[i, j] = b_spline_deriv[i, j][1] * h1[j]'
        mu_pred <- 'mu_pred[i] = b_spline_pred[age_index_pred[i]][1] * h1_holdout[block_holdout[i]]'
        for (i in 1:num_param) {
          if (i > 1) {
            mu_var <- paste0(mu_var, ' + b_spline[age_index[i]][', i, '] * h', i, '[block_data[i]]')
            mu_var_plot <- paste0(mu_var_plot, ' + b_spline_plot[i, j][', i, '] * h', i, '[j]')
            mu_var_agr <- paste0(mu_var_agr, ' + b_spline_deriv[i, j][', i, '] * h', i, '[j]')
            mu_pred <- paste0(mu_pred, ' + b_spline_pred[age_index_pred[i]][', i, '] * h', i, '_holdout[block_holdout[i]]')
          }
          h_var <- paste0(h_var, 'vector[n_block] h', i, ';\n  ')
          nxh_var <- paste0(nxh_var, '  int<lower=0> n_x', i, ';\n')
          sdh_var <- paste0(sdh_var, 'real<lower=0> sd_h', i, ';\n  ')
          x_var <- paste0(x_var, 'matrix[n_block, n_x', i, '] x', i, ';\n  ')
          b_var <- paste0(b_var, 'vector[n_x', i, '] b', i, ';\n  ')
          psi_var <- paste0(psi_var, 'vector[n_block] psi', i, ';\n  ')
          psi_reg_var <- paste0(psi_reg_var, 'psi', i, ' = x', i, ' * b', i, ';\n  ')
          h_prior <- paste0(h_prior, 'for (j in 1:n_block)\n h', i, '[j] ~ normal(psi', i, '[j], sd_h', i, ');\n  ')
          b_prior <- paste0(b_prior, 'for (j in 1:n_x', i, ')\n b', i, '[j] ~ normal(0.0, 1.0);\n  ')
          h_var_pred <- paste0(h_var_pred, 'vector[n_block_pred] h', i, '_holdout;\n  ')
          h_prior_pred <- paste0(h_prior_pred, 'for (j in 1:n_block_pred)\n h', i, '_holdout[j] ~ normal(psi', i, '_pred[j], sd_h', i, ');\n  ')
          x_var_pred <- paste0(x_var_pred, 'matrix[n_block_pred, n_x', i, '] x', i, '_pred;\n  ')
          psi_var_pred <- paste0(psi_var_pred, 'vector[n_block_pred] psi', i, '_pred;\n  ')
          psi_reg_var_pred <- paste0(psi_reg_var_pred, 'psi', i, '_pred = x', i, '_pred * b', i, ';\n  ')
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
        }
        mu_var <- paste0(mu_var, ';\n  ')
        mu_var_plot <- paste0(mu_var_plot, ';\n  ')
        mu_var_agr <- paste0(mu_var_agr, ';\n  ')
        mu_pred <- paste0(mu_pred, ';\n  ')
        cat(
          'data {
  int<lower=0> n;
  int<lower=0> n_pred;
  int<lower=0> n_block;
  int<lower=0> n_block_pred;\n',
          nxh_var,
          '  vector<lower=0>[n] size_data;
  int<lower=0> n_age;
  int<lower=0> n_age_pred;
  int<lower=0> n_k;
  int<lower=0> age_index[n];
  int<lower=0> age_index_pred[n_pred];
  int<lower=0, upper=n_block> block_data[n];
  int<lower=0, upper=n_block_pred> block_holdout[n_pred];\n',
          x_var,
          x_var_pred,
          'row_vector[n_k] b_spline[n_age];
        row_vector[n_k] b_spline_pred[n_age_pred];
        int<lower=0> n_plot;
        vector[n_plot] age_plot;
        row_vector[n_k] b_spline_plot[n_plot, n_block];
        row_vector[n_k] b_spline_deriv[n_plot, n_block];
        }
        
        parameters {
        real<lower=0> sigma_obs;\n',
          sdh_var,
          h_var,
          h_var_pred,
          b_var,
          '}
        
        transformed parameters {
        vector[n] mu;\n',
          psi_var,
          psi_var_pred,
          'for (i in 1:n)\n',
          mu_var,
          psi_reg_var,
          psi_reg_var_pred,
          '}
        
        model {
        log(size_data) ~ normal(mu, sigma_obs);
        sigma_obs ~ normal(0, 2);\n',
          h_prior,
          h_prior_pred,
          b_prior,
          sdh_prior,
          '}
        
        generated quantities {
        matrix[n_plot, n_block] mu_plot_growth;
        matrix[n_plot, n_block] mu_plot_agr;
        vector[n_pred] mu_pred;
        matrix<lower=0>[n_plot, n_block] size_plot;
        matrix<lower=0>[n_plot, n_block] size_plot_agr;
        vector<lower=0>[n_pred] size_pred;
        vector[n] log_lik;
        
        for (i in 1:n_plot)
        for (j in 1:n_block)\n',
          mu_var_plot,
          'for (i in 1:n_plot)
        for (j in 1:n_block)
        size_plot[i, j] = exp(mu_plot_growth[i, j]);
        for (i in 1:n_plot)
        for (j in 1:n_block)\n',
          mu_var_agr,
          'for (i in 1:n_plot)
        for (j in 1:n_block)
        size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
        for (i in 1:n_pred)\n',
          mu_pred,
        ' for (i in 1:n_pred)
          size_pred[i] = exp(mu_pred[i]);
        for (i in 1:n)
        log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
        }\n\n',
          file = ifelse(is.null(mod_file), mod_file <- paste0(tempfile(), '.stan'), mod_file))
      }
    } else {
      if (model != 'spline') {
        mod_par <- get(paste0(model, '_param_fetch'))()
        num_param <- mod_par$num_par
        mod_mu <- mod_par$mu
        mu_plot <- mod_par$mu_plot
        mu_plot_agr <- mod_par$mu_agr
        mu_pred <- mod_par$mu_holdout
        h_var <- NULL
        sdh_var <- NULL
        psi_var <- NULL
        psi_reg_var <- NULL
        b_prior <- NULL
        h_prior <- NULL
        sdh_prior <- NULL
        block_sd <- NULL
        psi_mean <- NULL
        block_sd_prior <- NULL
        psi_mean_prior <- NULL
        h_var_pred <- NULL
        h_prior_pred <- NULL
        for (i in 1:num_param) {
          h_var <- paste0(h_var, 'vector<lower=0>[n_block] h', i, ';\n  ')
          sdh_var <- paste0(sdh_var, 'real<lower=0> sd_h', i, ';\n  ')
          psi_var <- paste0(psi_var, 'vector[n_block] psi', i, ';\n  ')
          h_prior <- paste0(h_prior, 'h', i, ' ~ normal(psi', i, ', sd_h', i, ');\n  ')
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
          psi_reg_var <- paste0(psi_reg_var, 'psi', i, ' ~ normal(psi_mean', i, ', block_sd', i, ');\n  ')
          block_sd <- paste0(block_sd, 'real<lower=0> block_sd', i, ';\n  ')
          psi_mean <- paste0(psi_mean, 'real psi_mean', i, ';\n  ')
          psi_mean_prior <- paste0(psi_mean_prior, 'psi_mean', i, ' ~ normal(0.0, 2.0);\n  ')
          block_sd_prior <- paste0(block_sd_prior, 'block_sd', i, ' ~ normal(0.0, 2.0);\n  ')
          h_var_pred <- paste0(h_var_pred, 'vector<lower=0>[n_block_pred] h', i, '_holdout;\n  ')
          h_prior_pred <- paste0(h_prior_pred, 'h', i, '_holdout ~ normal(psi_mean', i, ', sd_h', i, ');\n  ')
        }
        cat(
          'data {
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
        real<lower=0> sigma_obs;\n',
          block_sd,
          psi_mean,
          sdh_var,
          psi_var,
          h_var,
          h_var_pred,
          '}
        
        transformed parameters {
        vector[n] mu;
        for (i in 1:n)
        mu[i] = ', mod_mu, ';\n',
          '}
        
        model {
        log(size_data) ~ normal(mu, sigma_obs);
        sigma_obs ~ normal(0, 2);\n',
          psi_reg_var,
          block_sd_prior,
          psi_mean_prior,
          h_prior,
          h_prior_pred,
          sdh_prior,
          '}
        
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
        mu_plot_growth[i, j] = ', mu_plot, ';\n',
          ' for (i in 1:n_plot)
        for (j in 1:n_block)
        size_plot[i, j] = exp(mu_plot_growth[i, j]);
        for (i in 1:n_plot)
        for (j in 1:n_block)
        mu_plot_agr[i, j] = ', mu_plot_agr, ';\n',
          ' for (i in 1:n_plot)
        for (j in 1:n_block)
        size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
        for (i in 1:n_pred)
          mu_pred[i] = ', mu_pred, ';\n',
        ' for (i in 1:n_pred)
          size_pred[i] = exp(mu_pred[i]);
        for (i in 1:n)
        log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
        }\n\n',
          file = ifelse(is.null(mod_file), mod_file <- paste0(tempfile(), '.stan'), mod_file))
      } else {
        if (!is.null(spline_params)) {
          num_param <- spline_params$n_knots + spline_params$degree
        } else {
          stop('spline_params must be passed to gen_mod_file.',
               call. = FALSE)
        }
        h_var <- NULL
        sdh_var <- NULL
        psi_var <- NULL
        psi_reg_var <- NULL
        h_prior <- NULL
        sdh_prior <- NULL
        block_sd <- NULL
        psi_mean <- NULL
        block_sd_prior <- NULL
        psi_mean_prior <- NULL
        h_var_pred <- NULL
        h_prior_pred <- NULL
        mu_var <- 'mu[i] = b_spline[age_index[i]][1] * h1[block_data[i]]'
        mu_var_plot <- 'mu_plot_growth[i, j] = b_spline_plot[i, j][1] * h1[j]'
        mu_var_agr <- 'mu_plot_agr[i, j] = b_spline_deriv[i, j][1] * h1[j]'
        mu_pred <- 'mu_pred[i] = b_spline_pred[age_index_pred[i]][1] * h1_holdout[block_holdout[i]]'
        for (i in 1:num_param) {
          if (i > 1) {
            mu_var <- paste0(mu_var, ' + b_spline[age_index[i]][', i, '] * h', i, '[block_data[i]]')
            mu_var_plot <- paste0(mu_var_plot, ' + b_spline_plot[i, j][', i, '] * h', i, '[j]')
            mu_var_agr <- paste0(mu_var_agr, ' + b_spline_deriv[i, j][', i, '] * h', i, '[j]')
            mu_pred <- paste0(mu_pred, ' + b_spline_pred[age_index_pred[i]][', i, '] * h', i, '_holdout[block_holdout[i]]')
          }
          h_var <- paste0(h_var, 'vector[n_block] h', i, ';\n  ')
          sdh_var <- paste0(sdh_var, 'real<lower=0> sd_h', i, ';\n  ')
          psi_var <- paste0(psi_var, 'vector[n_block] psi', i, ';\n  ')
          h_prior <- paste0(h_prior, 'for (j in 1:n_block)\n h', i, '[j] ~ normal(psi', i, '[j], sd_h', i, ');\n  ')
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
          psi_reg_var <- paste0(psi_reg_var, 'psi', i, ' ~ normal(psi_mean', i, ', block_sd', i, ');\n  ')
          block_sd <- paste0(block_sd, 'real<lower=0> block_sd', i, ';\n  ')
          psi_mean <- paste0(psi_mean, 'real psi_mean', i, ';\n  ')
          psi_mean_prior <- paste0(psi_mean_prior, 'psi_mean', i, ' ~ normal(0.0, 2.0);\n  ')
          block_sd_prior <- paste0(block_sd_prior, 'block_sd', i, ' ~ normal(0.0, 2.0);\n  ')
          h_var_pred <- paste0(h_var_pred, 'vector[n_block_pred] h', i, '_holdout;\n  ')
          h_prior_pred <- paste0(h_prior_pred, 'for (j in 1:n_block_pred)\n h', i, '_holdout[j] ~ normal(psi_mean', i, ', sd_h', i, ');\n  ')
        }
        mu_var <- paste0(mu_var, ';\n  ')
        mu_var_plot <- paste0(mu_var_plot, ';\n  ')
        mu_var_agr <- paste0(mu_var_agr, ';\n  ')
        mu_pred <- paste0(mu_pred, ';\n  ')
        cat(
          'data {
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
            real<lower=0> sigma_obs;\n',
          block_sd,
          psi_mean,
          sdh_var,
          psi_var,
          h_var,
          h_var_pred,
          '}
            
            transformed parameters {
            vector[n] mu;
        for (i in 1:n)\n',
          mu_var,
          '}
            
            model {
            log(size_data) ~ normal(mu, sigma_obs);
            sigma_obs ~ normal(0, 2);\n',
          h_prior,
          h_prior_pred,
          block_sd_prior,
          psi_mean_prior,
          psi_reg_var,
          sdh_prior,
          '}
            
            generated quantities {
            matrix[n_plot, n_block] mu_plot_growth;
            matrix[n_plot, n_block] mu_plot_agr;
            vector[n_pred] mu_pred;
            matrix<lower=0>[n_plot, n_block] size_plot;
            matrix<lower=0>[n_plot, n_block] size_plot_agr;
            vector<lower=0>[n_pred] size_pred;
            vector[n] log_lik;
            
            for (i in 1:n_plot)
            for (j in 1:n_block)\n',
          mu_var_plot,
          'for (i in 1:n_plot)
            for (j in 1:n_block)
            size_plot[i, j] = exp(mu_plot_growth[i, j]);
            for (i in 1:n_plot)
            for (j in 1:n_block)\n',
          mu_var_agr,
          'for (i in 1:n_plot)
            for (j in 1:n_block)
            size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
          for (i in 1:n_pred)\n',
          mu_pred,
          ' for (i in 1:n_pred)
            size_pred[i] = exp(mu_pred[i]);
          for (i in 1:n)
            log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
        }\n\n',
          file = ifelse(is.null(mod_file), mod_file <- paste0(tempfile(), '.stan'), mod_file))
      }
    }
  }
  mod_file
}
