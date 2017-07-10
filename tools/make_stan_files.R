mod_list <- c('hillslope', 'hillslope_log', 'power2', 'expo',
              'monod', 'neg_exp', 'koblog', 'power3',
              'logistic3', 'archibold', 'weibull3',
              'spline')
spline_params = list(degree = 8,
                     n_knots = 10,
                     spline_type = 'ispline')
for (i in seq_along(mod_list)) {
  gen_mod_file(model = mod_list[i],
               spline_params = spline_params,
               mod_file = paste0('./src/predmod/', mod_list[i], '.stan'),
               include_pred = TRUE,
               include_block = TRUE)
}
for (i in seq_along(mod_list)) {
  gen_mod_file(model = mod_list[i],
               spline_params = spline_params,
               mod_file = paste0('./src/blockmod/', mod_list[i], '.stan'),
               include_pred = FALSE,
               include_block = TRUE)
}
for (i in seq_along(mod_list)) {
  gen_mod_file(model = mod_list[i],
               spline_params = spline_params,
               mod_file = paste0('./src/onemod/', mod_list[i], '.stan'),
               include_pred = FALSE,
               include_block = FALSE)
}