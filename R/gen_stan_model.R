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
      h_var <- NULL
      h_prior <- NULL
      for (i in 1:num_param) {
        h_var <- paste0(h_var, 'real<lower=0> h', i, ';\n  ')
        h_prior <- paste0(h_prior, 'h', i, ' ~ normal(0.0, 2.0);\n  ')
      }
      cat(
        'data {
        int<lower=0> n;
        vector<lower=0>[n] size_data;
        vector[n] age;
        int<lower=0> n_plot;
        vector[n_plot] age_plot;
    }
        
        parameters {
        real<lower=0> sigma_obs;\n',
        h_var,
        '}
        
        transformed parameters {
        vector[n] mu;
        for (i in 1:n)
        mu[i] = ', mod_mu, ';\n',
        '}
        
        model {
        log(size_data) ~ normal(mu, sigma_obs);
        sigma_obs ~ normal(0, 2);\n',
        h_prior,
        '}
        
        generated quantities {
        matrix[n_plot] mu_plot_growth;
        matrix[n_plot] mu_plot_agr;
        matrix<lower=0>[n_plot] size_plot;
        matrix<lower=0>[n_plot] size_plot_agr;
        vector[n] log_lik;
        
        for (i in 1:n_plot)
        mu_plot_growth[i] = ', mu_plot, ';\n',
        ' for (i in 1:n_plot)
        size_plot[i] = exp(mu_plot_growth[i]);
        for (i in 1:n_plot)
        mu_plot_agr[i] = ', mu_plot_agr, ';\n',
        ' for (i in 1:n_plot)
        for (j in 1:n_block)
        size_plot_agr[i] = exp(mu_plot_agr[i]);
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
      mu_var <- 'mu[i] = b_spline[age_index[i]][1] * h1[block_data[i]]'
      mu_var_plot <- 'mu_plot_growth[i] = b_spline_plot[i][1] * h1'
      mu_var_agr <- 'mu_plot_agr[i] = b_spline_deriv[i][1] * h1'
      for (i in 1:num_param) {
        if (i > 1) {
          mu_var <- paste0(mu_var, ' + b_spline[age_index[i]][', i, '] * h', i)
          mu_var_plot <- paste0(mu_var_plot, ' + b_spline_plot[i][', i, '] * h', i)
          mu_var_agr <- paste0(mu_var_agr, ' + b_spline_deriv[i][', i, '] * h', i)
        }
        h_var <- paste0(h_var, 'real h', i, ';\n  ')
        h_prior <- paste0(h_prior, 'h', i, '~ normal(0.0, 2.0);\n  ')
      }
      mu_var <- paste0(mu_var, ';\n  ')
      mu_var_plot <- paste0(mu_var_plot, ';\n  ')
      mu_var_agr <- paste0(mu_var_agr, ';\n  ')
      cat(
        'data {
            int<lower=0> n;
            vector<lower=0>[n] size_data;
            vector[n] age;
            int<lower=0> n_age;
            int<lower=0> n_k;
            int<lower=0> age_index[n];
            row_vector[n_k] b_spline[n_t];
            int<lower=0> n_plot;
            vector[n_plot] age_plot;
            row_vector[n_k] b_spline_plot[n_plot];
            row_vector[n_k] b_spline_deriv[n_plot];
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
            matrix[n_plot] mu_plot_growth;
            matrix[n_plot] mu_plot_agr;
            matrix<lower=0>[n_plot] size_plot;
            matrix<lower=0>[n_plot] size_plot_agr;
            vector[n] log_lik;
            
            for (i in 1:n_plot)\n',
        mu_var_plot,
        'for (i in 1:n_plot)
            size_plot[i] = exp(mu_plot_growth[i]);
            for (i in 1:n_plot)\n',
        mu_var_agr,
        'for (i in 1:n_plot)
            size_plot_agr[i] = exp(mu_plot_agr[i]);
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
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
        }
        cat(
          'data {
      int<lower=0> n;
      int<lower=0> n_block;\n',
          nxh_var,
          x_var,
          'vector<lower=0>[n] size_data;
      vector[n] age;
      int<lower=0, upper=n_block> block_data[n];
      int<lower=0> n_plot;
      vector[n_plot] age_plot;
  }
      
      parameters {
      real<lower=0> sigma_obs;\n',
          sdh_var,
          h_var,
          b_var,
          '}
      
      transformed parameters {
      vector[n] mu;\n',
          psi_var,
          'for (i in 1:n)
      mu[i] = ', mod_mu, ';\n',
          psi_reg_var,
          '}
      
      model {
      log(size_data) ~ normal(mu, sigma_obs);
      sigma_obs ~ normal(0, 2);\n',
          h_prior,
          b_prior,
          sdh_prior,
          '}
      
      generated quantities {
      matrix[n_plot, n_block] mu_plot_growth;
      matrix[n_plot, n_block] mu_plot_agr;
      matrix<lower=0>[n_plot, n_block] size_plot;
      matrix<lower=0>[n_plot, n_block] size_plot_agr;
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
        mu_var <- 'mu[i] = b_spline[age_index[i]][1] * h1[block_data[i]]'
        mu_var_plot <- 'mu_plot_growth[i] = b_spline_plot[i, j][1] * h1[j]'
        mu_var_agr <- 'mu_plot_agr[i] = b_spline_deriv[i, j][1] * h1[j]'
        for (i in 1:num_param) {
          if (i > 1) {
            mu_var <- paste0(mu_var, ' + b_spline[age_index[i]][', i, '] * h', i, '[block_data[i]]')
            mu_var_plot <- paste0(mu_var_plot, ' + b_spline_plot[i, j][', i, '] * h', i, '[j]')
            mu_var_agr <- paste0(mu_var_agr, ' + b_spline_deriv[i, j][', i, '] * h', i, '[j]')
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
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
        }
        mu_var <- paste0(mu_var, ';\n  ')
        mu_var_plot <- paste0(mu_var_plot, ';\n  ')
        mu_var_agr <- paste0(mu_var_agr, ';\n  ')
        cat(
          'data {
  int<lower=0> n;
  int<lower=0> n_block;\n',
          nxh_var,
          '  vector<lower=0>[n] size_data;
  vector[n] age;
  int<lower=0> n_age;
  int<lower=0> n_k;
  int<lower=0> age_index[n];
  int<lower=0, upper=n_block> block_data[n];\n',
          x_var,
          'row_vector[n_k] b_spline[n_t];
        int<lower=0> n_plot;
        vector[n_plot] age_plot;
        row_vector[n_k] b_spline_plot[n_plot, n_block];
        row_vector[n_k] b_spline_deriv[n_plot, n_block];
        }
        
        parameters {
        real<lower=0> sigma_obs;\n',
          sdh_var,
          h_var,
          b_var,
          '}
        
        transformed parameters {
        vector[n] mu;\n',
          psi_var,
          'for (i in 1:n)\n',
          mu_var,
          psi_reg_var,
          '}
        
        model {
        log(size_data) ~ normal(mu, sigma_obs);
        sigma_obs ~ normal(0, 2);\n',
          h_prior,
          b_prior,
          sdh_prior,
          '}
        
        generated quantities {
        matrix[n_plot, n_block] mu_plot_growth;
        matrix[n_plot, n_block] mu_plot_agr;
        matrix<lower=0>[n_plot, n_block] size_plot;
        matrix<lower=0>[n_plot, n_block] size_plot_agr;
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
        for (i in 1:num_param) {
          h_var <- paste0(h_var, 'vector<lower=0>[n_block] h', i, ';\n  ')
          sdh_var <- paste0(sdh_var, 'real<lower=0> sd_h', i, ';\n  ')
          x_var <- paste0(x_var, 'matrix[n_block, n_x', i, '] x', i, ';\n  ')
          psi_var <- paste0(psi_var, 'vector[n_block] psi', i, ';\n  ')
          psi_reg_var <- paste0(psi_reg_var, 'psi', i, ' ~ normal(0.0, block_sd);\n  ')
          h_prior <- paste0(h_prior, 'h', i, ' ~ normal(psi', i, ', sd_h', i, ');\n  ')
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
        }
        cat(
          'data {
        int<lower=0> n;
        int<lower=0> n_block;
        vector<lower=0>[n] size_data;
        vector[n] age;
        int<lower=0, upper=n_block> block_data[n];
        int<lower=0> n_plot;
        vector[n_plot] age_plot;
    }
        
        parameters {
        real<lower=0> block_sd;
        real<lower=0> sigma_obs;\n',
          sdh_var,
          psi_var,
          h_var,
          '}
        
        transformed parameters {
        vector[n] mu;
        for (i in 1:n)
        mu[i] = ', mod_mu, ';\n',
          '}
        
        model {
        log(size_data) ~ normal(mu, sigma_obs);
        block_sd ~ normal(0, 2);
        sigma_obs ~ normal(0, 2);\n',
          psi_reg_var,
          h_prior,
          sdh_prior,
          '}
        
        generated quantities {
        matrix[n_plot, n_block] mu_plot_growth;
        matrix[n_plot, n_block] mu_plot_agr;
        matrix<lower=0>[n_plot, n_block] size_plot;
        matrix<lower=0>[n_plot, n_block] size_plot_agr;
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
        mu_var <- 'mu[i] = b_spline[age_index[i]][1] * h1[block_data[i]]'
        mu_var_plot <- 'mu_plot_growth[i] = b_spline_plot[i, j][1] * h1[j]'
        mu_var_agr <- 'mu_plot_agr[i] = b_spline_deriv[i, j][1] * h1[j]'
        for (i in 1:num_param) {
          if (i > 1) {
            mu_var <- paste0(mu_var, ' + b_spline[age_index[i]][', i, '] * h', i, '[block_data[i]]')
            mu_var_plot <- paste0(mu_var_plot, ' + b_spline_plot[i, j][', i, '] * h', i, '[j]')
            mu_var_agr <- paste0(mu_var_agr, ' + b_spline_deriv[i, j][', i, '] * h', i, '[j]')
          }
          h_var <- paste0(h_var, 'vector[n_block] h', i, ';\n  ')
          sdh_var <- paste0(sdh_var, 'real<lower=0> sd_h', i, ';\n  ')
          psi_var <- paste0(psi_var, 'vector[n_block] psi', i, ';\n  ')
          psi_reg_var <- paste0(psi_reg_var, 'psi', i, ' ~ normal(0.0, block_sd);\n  ')
          h_prior <- paste0(h_prior, 'for (j in 1:n_block)\n h', i, '[j] ~ normal(psi', i, '[j], sd_h', i, ');\n  ')
          sdh_prior <- paste0(sdh_prior, 'sd_h', i, ' ~ normal(0.0, 2.0);\n  ')
        }
        mu_var <- paste0(mu_var, ';\n  ')
        mu_var_plot <- paste0(mu_var_plot, ';\n  ')
        mu_var_agr <- paste0(mu_var_agr, ';\n  ')
        cat(
          'data {
            int<lower=0> n;
            int<lower=0> n_block;
            vector<lower=0>[n] size_data;
            vector[n] age;
            int<lower=0> n_age;
            int<lower=0> n_k;
            int<lower=0> age_index[n];
            int<lower=0, upper=n_block> block_data[n];
            row_vector[n_k] b_spline[n_t];
            int<lower=0> n_plot;
            vector[n_plot] age_plot;
            row_vector[n_k] b_spline_plot[n_plot, n_block];
            row_vector[n_k] b_spline_deriv[n_plot, n_block];
        }
            
            parameters {
            real<lower=0> block_sd;
            real<lower=0> sigma_obs;\n',
          sdh_var,
          psi_var,
          h_var,
          '}
            
            transformed parameters {
            vector[n] mu;
        for (i in 1:n)\n',
          mu_var,
          '}
            
            model {
            log(size_data) ~ normal(mu, sigma_obs);
            block_sd ~ normal(0, 2);
            sigma_obs ~ normal(0, 2);\n',
          h_prior,
          psi_reg_var,
          sdh_prior,
          '}
            
            generated quantities {
            matrix[n_plot, n_block] mu_plot_growth;
            matrix[n_plot, n_block] mu_plot_agr;
            matrix<lower=0>[n_plot, n_block] size_plot;
            matrix<lower=0>[n_plot, n_block] size_plot_agr;
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
            for (i in 1:n)
            log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
            }\n\n',
          file = ifelse(is.null(mod_file), mod_file <- paste0(tempfile(), '.stan'), mod_file))
      }
    }
  }
  mod_file
}