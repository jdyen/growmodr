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
  real<lower=0> block_sd1;
  real<lower=0> block_sd2;
  real<lower=0> block_sd3;
  real<lower=0> block_sd4;
  real<lower=0> block_sd5;
  real<lower=0> block_sd6;
  real<lower=0> block_sd7;
  real<lower=0> block_sd8;
  real<lower=0> block_sd9;
  real<lower=0> block_sd10;
  real<lower=0> block_sd11;
  real<lower=0> block_sd12;
  real<lower=0> block_sd13;
  real<lower=0> block_sd14;
  real<lower=0> block_sd15;
  real<lower=0> block_sd16;
  real<lower=0> block_sd17;
  real<lower=0> block_sd18;
  real psi_mean1;
  real psi_mean2;
  real psi_mean3;
  real psi_mean4;
  real psi_mean5;
  real psi_mean6;
  real psi_mean7;
  real psi_mean8;
  real psi_mean9;
  real psi_mean10;
  real psi_mean11;
  real psi_mean12;
  real psi_mean13;
  real psi_mean14;
  real psi_mean15;
  real psi_mean16;
  real psi_mean17;
  real psi_mean18;
  real<lower=0> sd_h1;
  real<lower=0> sd_h2;
  real<lower=0> sd_h3;
  real<lower=0> sd_h4;
  real<lower=0> sd_h5;
  real<lower=0> sd_h6;
  real<lower=0> sd_h7;
  real<lower=0> sd_h8;
  real<lower=0> sd_h9;
  real<lower=0> sd_h10;
  real<lower=0> sd_h11;
  real<lower=0> sd_h12;
  real<lower=0> sd_h13;
  real<lower=0> sd_h14;
  real<lower=0> sd_h15;
  real<lower=0> sd_h16;
  real<lower=0> sd_h17;
  real<lower=0> sd_h18;
  vector[n_block] psi1;
  vector[n_block] psi2;
  vector[n_block] psi3;
  vector[n_block] psi4;
  vector[n_block] psi5;
  vector[n_block] psi6;
  vector[n_block] psi7;
  vector[n_block] psi8;
  vector[n_block] psi9;
  vector[n_block] psi10;
  vector[n_block] psi11;
  vector[n_block] psi12;
  vector[n_block] psi13;
  vector[n_block] psi14;
  vector[n_block] psi15;
  vector[n_block] psi16;
  vector[n_block] psi17;
  vector[n_block] psi18;
  vector[n_block] h1;
  vector[n_block] h2;
  vector[n_block] h3;
  vector[n_block] h4;
  vector[n_block] h5;
  vector[n_block] h6;
  vector[n_block] h7;
  vector[n_block] h8;
  vector[n_block] h9;
  vector[n_block] h10;
  vector[n_block] h11;
  vector[n_block] h12;
  vector[n_block] h13;
  vector[n_block] h14;
  vector[n_block] h15;
  vector[n_block] h16;
  vector[n_block] h17;
  vector[n_block] h18;
  vector[n_block_pred] h1_holdout;
  vector[n_block_pred] h2_holdout;
  vector[n_block_pred] h3_holdout;
  vector[n_block_pred] h4_holdout;
  vector[n_block_pred] h5_holdout;
  vector[n_block_pred] h6_holdout;
  vector[n_block_pred] h7_holdout;
  vector[n_block_pred] h8_holdout;
  vector[n_block_pred] h9_holdout;
  vector[n_block_pred] h10_holdout;
  vector[n_block_pred] h11_holdout;
  vector[n_block_pred] h12_holdout;
  vector[n_block_pred] h13_holdout;
  vector[n_block_pred] h14_holdout;
  vector[n_block_pred] h15_holdout;
  vector[n_block_pred] h16_holdout;
  vector[n_block_pred] h17_holdout;
  vector[n_block_pred] h18_holdout;
}
            
transformed parameters {
  vector[n] mu;
  for (i in 1:n)
     mu[i] = b_spline[age_index[i]][1] * h1[block_data[i]] + b_spline[age_index[i]][2] * h2[block_data[i]] + b_spline[age_index[i]][3] * h3[block_data[i]] + b_spline[age_index[i]][4] * h4[block_data[i]] + b_spline[age_index[i]][5] * h5[block_data[i]] + b_spline[age_index[i]][6] * h6[block_data[i]] + b_spline[age_index[i]][7] * h7[block_data[i]] + b_spline[age_index[i]][8] * h8[block_data[i]] + b_spline[age_index[i]][9] * h9[block_data[i]] + b_spline[age_index[i]][10] * h10[block_data[i]] + b_spline[age_index[i]][11] * h11[block_data[i]] + b_spline[age_index[i]][12] * h12[block_data[i]] + b_spline[age_index[i]][13] * h13[block_data[i]] + b_spline[age_index[i]][14] * h14[block_data[i]] + b_spline[age_index[i]][15] * h15[block_data[i]] + b_spline[age_index[i]][16] * h16[block_data[i]] + b_spline[age_index[i]][17] * h17[block_data[i]] + b_spline[age_index[i]][18] * h18[block_data[i]];
}
            
model {
  log(size_data) ~ normal(mu, sigma_obs);
  sigma_obs ~ normal(0, 2);
  for (j in 1:n_block)
    h1[j] ~ normal(psi1[j], sd_h1);
  for (j in 1:n_block)
    h2[j] ~ normal(psi2[j], sd_h2);
  for (j in 1:n_block)
    h3[j] ~ normal(psi3[j], sd_h3);
  for (j in 1:n_block)
    h4[j] ~ normal(psi4[j], sd_h4);
  for (j in 1:n_block)
    h5[j] ~ normal(psi5[j], sd_h5);
  for (j in 1:n_block)
    h6[j] ~ normal(psi6[j], sd_h6);
  for (j in 1:n_block)
    h7[j] ~ normal(psi7[j], sd_h7);
  for (j in 1:n_block)
    h8[j] ~ normal(psi8[j], sd_h8);
  for (j in 1:n_block)
    h9[j] ~ normal(psi9[j], sd_h9);
  for (j in 1:n_block)
    h10[j] ~ normal(psi10[j], sd_h10);
  for (j in 1:n_block)
    h11[j] ~ normal(psi11[j], sd_h11);
  for (j in 1:n_block)
    h12[j] ~ normal(psi12[j], sd_h12);
  for (j in 1:n_block)
    h13[j] ~ normal(psi13[j], sd_h13);
  for (j in 1:n_block)
    h14[j] ~ normal(psi14[j], sd_h14);
  for (j in 1:n_block)
    h15[j] ~ normal(psi15[j], sd_h15);
  for (j in 1:n_block)
    h16[j] ~ normal(psi16[j], sd_h16);
  for (j in 1:n_block)
    h17[j] ~ normal(psi17[j], sd_h17);
  for (j in 1:n_block)
    h18[j] ~ normal(psi18[j], sd_h18);
   for (j in 1:n_block_pred)
    h1_holdout[j] ~ normal(psi_mean1, sd_h1);
  for (j in 1:n_block_pred)
    h2_holdout[j] ~ normal(psi_mean2, sd_h2);
  for (j in 1:n_block_pred)
    h3_holdout[j] ~ normal(psi_mean3, sd_h3);
  for (j in 1:n_block_pred)
    h4_holdout[j] ~ normal(psi_mean4, sd_h4);
  for (j in 1:n_block_pred)
    h5_holdout[j] ~ normal(psi_mean5, sd_h5);
  for (j in 1:n_block_pred)
    h6_holdout[j] ~ normal(psi_mean6, sd_h6);
  for (j in 1:n_block_pred)
    h7_holdout[j] ~ normal(psi_mean7, sd_h7);
  for (j in 1:n_block_pred)
    h8_holdout[j] ~ normal(psi_mean8, sd_h8);
  for (j in 1:n_block_pred)
    h9_holdout[j] ~ normal(psi_mean9, sd_h9);
  for (j in 1:n_block_pred)
    h10_holdout[j] ~ normal(psi_mean10, sd_h10);
  for (j in 1:n_block_pred)
    h11_holdout[j] ~ normal(psi_mean11, sd_h11);
  for (j in 1:n_block_pred)
    h12_holdout[j] ~ normal(psi_mean12, sd_h12);
  for (j in 1:n_block_pred)
    h13_holdout[j] ~ normal(psi_mean13, sd_h13);
  for (j in 1:n_block_pred)
    h14_holdout[j] ~ normal(psi_mean14, sd_h14);
  for (j in 1:n_block_pred)
    h15_holdout[j] ~ normal(psi_mean15, sd_h15);
  for (j in 1:n_block_pred)
    h16_holdout[j] ~ normal(psi_mean16, sd_h16);
  for (j in 1:n_block_pred)
    h17_holdout[j] ~ normal(psi_mean17, sd_h17);
  for (j in 1:n_block_pred)
    h18_holdout[j] ~ normal(psi_mean18, sd_h18);
  block_sd1 ~ normal(0.0, 2.0);
  block_sd2 ~ normal(0.0, 2.0);
  block_sd3 ~ normal(0.0, 2.0);
  block_sd4 ~ normal(0.0, 2.0);
  block_sd5 ~ normal(0.0, 2.0);
  block_sd6 ~ normal(0.0, 2.0);
  block_sd7 ~ normal(0.0, 2.0);
  block_sd8 ~ normal(0.0, 2.0);
  block_sd9 ~ normal(0.0, 2.0);
  block_sd10 ~ normal(0.0, 2.0);
  block_sd11 ~ normal(0.0, 2.0);
  block_sd12 ~ normal(0.0, 2.0);
  block_sd13 ~ normal(0.0, 2.0);
  block_sd14 ~ normal(0.0, 2.0);
  block_sd15 ~ normal(0.0, 2.0);
  block_sd16 ~ normal(0.0, 2.0);
  block_sd17 ~ normal(0.0, 2.0);
  block_sd18 ~ normal(0.0, 2.0);
  psi_mean1 ~ normal(0.0, 2.0);
  psi_mean2 ~ normal(0.0, 2.0);
  psi_mean3 ~ normal(0.0, 2.0);
  psi_mean4 ~ normal(0.0, 2.0);
  psi_mean5 ~ normal(0.0, 2.0);
  psi_mean6 ~ normal(0.0, 2.0);
  psi_mean7 ~ normal(0.0, 2.0);
  psi_mean8 ~ normal(0.0, 2.0);
  psi_mean9 ~ normal(0.0, 2.0);
  psi_mean10 ~ normal(0.0, 2.0);
  psi_mean11 ~ normal(0.0, 2.0);
  psi_mean12 ~ normal(0.0, 2.0);
  psi_mean13 ~ normal(0.0, 2.0);
  psi_mean14 ~ normal(0.0, 2.0);
  psi_mean15 ~ normal(0.0, 2.0);
  psi_mean16 ~ normal(0.0, 2.0);
  psi_mean17 ~ normal(0.0, 2.0);
  psi_mean18 ~ normal(0.0, 2.0);
  psi1 ~ normal(psi_mean1, block_sd1);
  psi2 ~ normal(psi_mean2, block_sd2);
  psi3 ~ normal(psi_mean3, block_sd3);
  psi4 ~ normal(psi_mean4, block_sd4);
  psi5 ~ normal(psi_mean5, block_sd5);
  psi6 ~ normal(psi_mean6, block_sd6);
  psi7 ~ normal(psi_mean7, block_sd7);
  psi8 ~ normal(psi_mean8, block_sd8);
  psi9 ~ normal(psi_mean9, block_sd9);
  psi10 ~ normal(psi_mean10, block_sd10);
  psi11 ~ normal(psi_mean11, block_sd11);
  psi12 ~ normal(psi_mean12, block_sd12);
  psi13 ~ normal(psi_mean13, block_sd13);
  psi14 ~ normal(psi_mean14, block_sd14);
  psi15 ~ normal(psi_mean15, block_sd15);
  psi16 ~ normal(psi_mean16, block_sd16);
  psi17 ~ normal(psi_mean17, block_sd17);
  psi18 ~ normal(psi_mean18, block_sd18);
  sd_h1 ~ normal(0.0, 2.0);
  sd_h2 ~ normal(0.0, 2.0);
  sd_h3 ~ normal(0.0, 2.0);
  sd_h4 ~ normal(0.0, 2.0);
  sd_h5 ~ normal(0.0, 2.0);
  sd_h6 ~ normal(0.0, 2.0);
  sd_h7 ~ normal(0.0, 2.0);
  sd_h8 ~ normal(0.0, 2.0);
  sd_h9 ~ normal(0.0, 2.0);
  sd_h10 ~ normal(0.0, 2.0);
  sd_h11 ~ normal(0.0, 2.0);
  sd_h12 ~ normal(0.0, 2.0);
  sd_h13 ~ normal(0.0, 2.0);
  sd_h14 ~ normal(0.0, 2.0);
  sd_h15 ~ normal(0.0, 2.0);
  sd_h16 ~ normal(0.0, 2.0);
  sd_h17 ~ normal(0.0, 2.0);
  sd_h18 ~ normal(0.0, 2.0);
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
       mu_plot_growth[i, j] = b_spline_plot[i, j][1] * h1[j] + b_spline_plot[i, j][2] * h2[j] + b_spline_plot[i, j][3] * h3[j] + b_spline_plot[i, j][4] * h4[j] + b_spline_plot[i, j][5] * h5[j] + b_spline_plot[i, j][6] * h6[j] + b_spline_plot[i, j][7] * h7[j] + b_spline_plot[i, j][8] * h8[j] + b_spline_plot[i, j][9] * h9[j] + b_spline_plot[i, j][10] * h10[j] + b_spline_plot[i, j][11] * h11[j] + b_spline_plot[i, j][12] * h12[j] + b_spline_plot[i, j][13] * h13[j] + b_spline_plot[i, j][14] * h14[j] + b_spline_plot[i, j][15] * h15[j] + b_spline_plot[i, j][16] * h16[j] + b_spline_plot[i, j][17] * h17[j] + b_spline_plot[i, j][18] * h18[j];
   for (i in 1:n_plot)
     for (j in 1:n_block)
       size_plot[i, j] = exp(mu_plot_growth[i, j]);
   for (i in 1:n_plot)
     for (j in 1:n_block)
        mu_plot_agr[i, j] = b_spline_deriv[i, j][1] * h1[j] + b_spline_deriv[i, j][2] * h2[j] + b_spline_deriv[i, j][3] * h3[j] + b_spline_deriv[i, j][4] * h4[j] + b_spline_deriv[i, j][5] * h5[j] + b_spline_deriv[i, j][6] * h6[j] + b_spline_deriv[i, j][7] * h7[j] + b_spline_deriv[i, j][8] * h8[j] + b_spline_deriv[i, j][9] * h9[j] + b_spline_deriv[i, j][10] * h10[j] + b_spline_deriv[i, j][11] * h11[j] + b_spline_deriv[i, j][12] * h12[j] + b_spline_deriv[i, j][13] * h13[j] + b_spline_deriv[i, j][14] * h14[j] + b_spline_deriv[i, j][15] * h15[j] + b_spline_deriv[i, j][16] * h16[j] + b_spline_deriv[i, j][17] * h17[j] + b_spline_deriv[i, j][18] * h18[j];
   for (i in 1:n_plot)
     for (j in 1:n_block)
       size_plot_agr[i, j] = exp(mu_plot_agr[i, j]);
   for (i in 1:n_pred)
     mu_pred[i] = b_spline_pred[age_index_pred[i]][1] * h1_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][2] * h2_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][3] * h3_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][4] * h4_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][5] * h5_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][6] * h6_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][7] * h7_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][8] * h8_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][9] * h9_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][10] * h10_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][11] * h11_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][12] * h12_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][13] * h13_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][14] * h14_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][15] * h15_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][16] * h16_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][17] * h17_holdout[block_holdout[i]] + b_spline_pred[age_index_pred[i]][18] * h18_holdout[block_holdout[i]];
   for (i in 1:n_pred)
     size_pred[i] = exp(mu_pred[i]);
   for (i in 1:n)
     log_lik[i] = normal_lpdf(log(size_data[i]) | mu[i], sigma_obs);
}

