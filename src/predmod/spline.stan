data {
  int<lower=0> n;
  int<lower=0> n_pred;
  int<lower=0> n_block;
  int<lower=0> n_block_pred;
  int<lower=0> n_x1;
  int<lower=0> n_x2;
  int<lower=0> n_x3;
  int<lower=0> n_x4;
  int<lower=0> n_x5;
  int<lower=0> n_x6;
  int<lower=0> n_x7;
  int<lower=0> n_x8;
  int<lower=0> n_x9;
  int<lower=0> n_x10;
  int<lower=0> n_x11;
  int<lower=0> n_x12;
  int<lower=0> n_x13;
  int<lower=0> n_x14;
  int<lower=0> n_x15;
  int<lower=0> n_x16;
  int<lower=0> n_x17;
  int<lower=0> n_x18;
  vector<lower=0>[n] size_data;
  int<lower=0> n_age;
  int<lower=0> n_age_pred;
  int<lower=0> n_k;
  int<lower=0> age_index[n];
  int<lower=0> age_index_pred[n_pred];
  int<lower=0, upper=n_block> block_data[n];
  int<lower=0, upper=n_block_pred> block_holdout[n_pred];
  matrix[n_block, n_x1] x1;
  matrix[n_block, n_x2] x2;
  matrix[n_block, n_x3] x3;
  matrix[n_block, n_x4] x4;
  matrix[n_block, n_x5] x5;
  matrix[n_block, n_x6] x6;
  matrix[n_block, n_x7] x7;
  matrix[n_block, n_x8] x8;
  matrix[n_block, n_x9] x9;
  matrix[n_block, n_x10] x10;
  matrix[n_block, n_x11] x11;
  matrix[n_block, n_x12] x12;
  matrix[n_block, n_x13] x13;
  matrix[n_block, n_x14] x14;
  matrix[n_block, n_x15] x15;
  matrix[n_block, n_x16] x16;
  matrix[n_block, n_x17] x17;
  matrix[n_block, n_x18] x18;
  matrix[n_block_pred, n_x1] x1_pred;
  matrix[n_block_pred, n_x2] x2_pred;
  matrix[n_block_pred, n_x3] x3_pred;
  matrix[n_block_pred, n_x4] x4_pred;
  matrix[n_block_pred, n_x5] x5_pred;
  matrix[n_block_pred, n_x6] x6_pred;
  matrix[n_block_pred, n_x7] x7_pred;
  matrix[n_block_pred, n_x8] x8_pred;
  matrix[n_block_pred, n_x9] x9_pred;
  matrix[n_block_pred, n_x10] x10_pred;
  matrix[n_block_pred, n_x11] x11_pred;
  matrix[n_block_pred, n_x12] x12_pred;
  matrix[n_block_pred, n_x13] x13_pred;
  matrix[n_block_pred, n_x14] x14_pred;
  matrix[n_block_pred, n_x15] x15_pred;
  matrix[n_block_pred, n_x16] x16_pred;
  matrix[n_block_pred, n_x17] x17_pred;
  matrix[n_block_pred, n_x18] x18_pred;
  row_vector[n_k] b_spline[n_age];
  row_vector[n_k] b_spline_pred[n_age_pred];
  int<lower=0> n_plot;
  vector[n_plot] age_plot;
  row_vector[n_k] b_spline_plot[n_plot, n_block];
  row_vector[n_k] b_spline_deriv[n_plot, n_block];
}
        
parameters {
  real<lower=0> sigma_obs;
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
  vector[n_x1] b1;
  vector[n_x2] b2;
  vector[n_x3] b3;
  vector[n_x4] b4;
  vector[n_x5] b5;
  vector[n_x6] b6;
  vector[n_x7] b7;
  vector[n_x8] b8;
  vector[n_x9] b9;
  vector[n_x10] b10;
  vector[n_x11] b11;
  vector[n_x12] b12;
  vector[n_x13] b13;
  vector[n_x14] b14;
  vector[n_x15] b15;
  vector[n_x16] b16;
  vector[n_x17] b17;
  vector[n_x18] b18;
}
        
transformed parameters {
  vector[n] mu;
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
  vector[n_block_pred] psi1_pred;
  vector[n_block_pred] psi2_pred;
  vector[n_block_pred] psi3_pred;
  vector[n_block_pred] psi4_pred;
  vector[n_block_pred] psi5_pred;
  vector[n_block_pred] psi6_pred;
  vector[n_block_pred] psi7_pred;
  vector[n_block_pred] psi8_pred;
  vector[n_block_pred] psi9_pred;
  vector[n_block_pred] psi10_pred;
  vector[n_block_pred] psi11_pred;
  vector[n_block_pred] psi12_pred;
  vector[n_block_pred] psi13_pred;
  vector[n_block_pred] psi14_pred;
  vector[n_block_pred] psi15_pred;
  vector[n_block_pred] psi16_pred;
  vector[n_block_pred] psi17_pred;
  vector[n_block_pred] psi18_pred;
  for (i in 1:n)
    mu[i] = b_spline[age_index[i]][1] * h1[block_data[i]] + b_spline[age_index[i]][2] * h2[block_data[i]] + b_spline[age_index[i]][3] * h3[block_data[i]] + b_spline[age_index[i]][4] * h4[block_data[i]] + b_spline[age_index[i]][5] * h5[block_data[i]] + b_spline[age_index[i]][6] * h6[block_data[i]] + b_spline[age_index[i]][7] * h7[block_data[i]] + b_spline[age_index[i]][8] * h8[block_data[i]] + b_spline[age_index[i]][9] * h9[block_data[i]] + b_spline[age_index[i]][10] * h10[block_data[i]] + b_spline[age_index[i]][11] * h11[block_data[i]] + b_spline[age_index[i]][12] * h12[block_data[i]] + b_spline[age_index[i]][13] * h13[block_data[i]] + b_spline[age_index[i]][14] * h14[block_data[i]] + b_spline[age_index[i]][15] * h15[block_data[i]] + b_spline[age_index[i]][16] * h16[block_data[i]] + b_spline[age_index[i]][17] * h17[block_data[i]] + b_spline[age_index[i]][18] * h18[block_data[i]];
  psi1 = x1 * b1;
  psi2 = x2 * b2;
  psi3 = x3 * b3;
  psi4 = x4 * b4;
  psi5 = x5 * b5;
  psi6 = x6 * b6;
  psi7 = x7 * b7;
  psi8 = x8 * b8;
  psi9 = x9 * b9;
  psi10 = x10 * b10;
  psi11 = x11 * b11;
  psi12 = x12 * b12;
  psi13 = x13 * b13;
  psi14 = x14 * b14;
  psi15 = x15 * b15;
  psi16 = x16 * b16;
  psi17 = x17 * b17;
  psi18 = x18 * b18;
  psi1_pred = x1_pred * b1;
  psi2_pred = x2_pred * b2;
  psi3_pred = x3_pred * b3;
  psi4_pred = x4_pred * b4;
  psi5_pred = x5_pred * b5;
  psi6_pred = x6_pred * b6;
  psi7_pred = x7_pred * b7;
  psi8_pred = x8_pred * b8;
  psi9_pred = x9_pred * b9;
  psi10_pred = x10_pred * b10;
  psi11_pred = x11_pred * b11;
  psi12_pred = x12_pred * b12;
  psi13_pred = x13_pred * b13;
  psi14_pred = x14_pred * b14;
  psi15_pred = x15_pred * b15;
  psi16_pred = x16_pred * b16;
  psi17_pred = x17_pred * b17;
  psi18_pred = x18_pred * b18;
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
    h1_holdout[j] ~ normal(psi1_pred[j], sd_h1);
  for (j in 1:n_block_pred)
    h2_holdout[j] ~ normal(psi2_pred[j], sd_h2);
  for (j in 1:n_block_pred)
    h3_holdout[j] ~ normal(psi3_pred[j], sd_h3);
  for (j in 1:n_block_pred)
    h4_holdout[j] ~ normal(psi4_pred[j], sd_h4);
  for (j in 1:n_block_pred)
    h5_holdout[j] ~ normal(psi5_pred[j], sd_h5);
  for (j in 1:n_block_pred)
    h6_holdout[j] ~ normal(psi6_pred[j], sd_h6);
  for (j in 1:n_block_pred)
    h7_holdout[j] ~ normal(psi7_pred[j], sd_h7);
  for (j in 1:n_block_pred)
    h8_holdout[j] ~ normal(psi8_pred[j], sd_h8);
  for (j in 1:n_block_pred)
    h9_holdout[j] ~ normal(psi9_pred[j], sd_h9);
  for (j in 1:n_block_pred)
    h10_holdout[j] ~ normal(psi10_pred[j], sd_h10);
  for (j in 1:n_block_pred)
    h11_holdout[j] ~ normal(psi11_pred[j], sd_h11);
  for (j in 1:n_block_pred)
    h12_holdout[j] ~ normal(psi12_pred[j], sd_h12);
  for (j in 1:n_block_pred)
    h13_holdout[j] ~ normal(psi13_pred[j], sd_h13);
  for (j in 1:n_block_pred)
    h14_holdout[j] ~ normal(psi14_pred[j], sd_h14);
  for (j in 1:n_block_pred)
    h15_holdout[j] ~ normal(psi15_pred[j], sd_h15);
  for (j in 1:n_block_pred)
    h16_holdout[j] ~ normal(psi16_pred[j], sd_h16);
  for (j in 1:n_block_pred)
    h17_holdout[j] ~ normal(psi17_pred[j], sd_h17);
  for (j in 1:n_block_pred)
    h18_holdout[j] ~ normal(psi18_pred[j], sd_h18);
  for (j in 1:n_x1)
    b1[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x2)
    b2[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x3)
    b3[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x4)
    b4[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x5)
    b5[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x6)
    b6[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x7)
    b7[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x8)
    b8[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x9)
    b9[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x10)
    b10[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x11)
    b11[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x12)
    b12[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x13)
    b13[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x14)
    b14[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x15)
    b15[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x16)
    b16[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x17)
    b17[j] ~ normal(0.0, 1.0);
  for (j in 1:n_x18)
    b18[j] ~ normal(0.0, 1.0);
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

