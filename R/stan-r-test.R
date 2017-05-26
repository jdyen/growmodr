# clear workspace
rm(list = ls(all = TRUE))

# load libraries
library(rstan)
library(loo)
library(splines)
library(parallel)

# set working directory
setwd('~/Dropbox/GrowthModels/')

# source functions
source('./stan-code/stan-summary-fun.R')
source('./stan-code/stan-gen-model.R')
source('./stan-code/stan-cv-code.R')

# load data
load("FT_traitsNoResp.rda")
load("FalsterSpTraits_SEEDERS.rda")
load("MuirData.rda")

data.test <- FT_traitsNoResp
if (any(is.na(data.test$Ht))) {
  data.test <- data.test[-which(is.na(data.test$Ht)), ]
}
data.ft <- with(data.test, data.frame(SPP = SPP,
                                      Ht = Ht,
                                      Yrs = Yrs,
                                      SD_MASS = SD_MASS,
                                      SLA = SLA,
                                      N = N,
                                      SD = SD))
data.falster <- with(FalsterSpTraits_SEEDERS, data.frame(SPP = SPP,
                                                         Ht = Ht,
                                                         Yrs = Yrs,
                                                         SD_MASS = SD_MASS,
                                                         SLA = SLA,
                                                         N = n_mass,
                                                         SD = SD_25))
data.falster$SD[which(is.na(data.falster$SD))] <- mean(data.falster$SD, na.rm = TRUE)
data.muir <- with(MuirData, data.frame(SPP = SPP,
                                       Ht = Ht,
                                       Yrs = Yrs,
                                       SD_MASS = SD_MASS,
                                       SLA = SLA,
                                       N = n_mass,
                                       SD = rep(0, nrow(MuirData))))


mod.list <- c('hillslope',
              'hillslope_log',
              'power2',
              'expo',
              'monod',
              'neg_exp',
              'koblog',
              'power3',
              'logistic3',
              'archibold',
              'weibull3',
              'spline')

# set stan settings
n.iter <- 5000
n.chains <- 4
n.cv <- NULL
n.its.cv <- 5000
n.chains.cv <- 4
sp.deg <- 7
sp.nk <- 8

par.run <- FALSE

# fit stan model
data.to.use <- data.ft

mod.list2 <- c(mod.list, mod.list[-length(mod.list)])
all_traits <- c(rep(FALSE, length(mod.list)), rep(TRUE, (length(mod.list) - 1)))
if (par.run & (Sys.info()['sysname'] == 'Darwin')) {
  mod.ft <- mclapply(1:(2 * length(mod.list) - 1),
                     stan_growth_mod_int,
                     data = data.to.use,
                     mod.list = mod.list2,
                     all_traits = all_traits,
                     validate = TRUE,
                     n.iter = n.iter,
                     n.chains = n.chains,
                     spline_params = list(degree = sp.deg, n_knots = sp.nk),
                     mc.cores = parallel::detectCores())
} else {
  mod.ft <- lapply(1:(2 * length(mod.list) - 1),
                   stan_growth_mod_int,
                   data = data.to.use,
                   mod.list = mod.list2,
                   all_traits = all_traits,
                   validate = TRUE,
                   n.iter = n.iter,
                   n.chains = n.chains,
                   spline_params = list(degree = sp.deg, n_knots = sp.nk))
}
save(mod.ft, file = '~/Dropbox/Post-doc\ stuff/random-stuff/mod_ft_full.R')

# cross validate stan model
mod.cv.ft <- vector('list', length = (length(mod.list) + (length(mod.list) - 1)))
for (i in 1:(length(mod.list) - 1)) {
  print(paste0('Starting cross validation for ', mod.list[i], ' model'))
  mod.cv.ft[[i]] <- stan_growth_model_cv(data.to.use,
                                         model = mod.list[i],
                                         all_traits = FALSE,
                                         n.cv = n.cv,
                                         n.iter = n.its.cv,
                                         n.chains = n.chains.cv,
                                         cores = parallel::detectCores())
  mod.cv.ft[[i + length(mod.list)]] <- stan_growth_model_cv(data.to.use,
                                                            model = mod.list[i],
                                                            all_traits = TRUE,
                                                            n.cv = n.cv,
                                                            n.iter = n.its.cv,
                                                            n.chains = n.chains.cv,
                                                            cores = parallel::detectCores())
}
mod.cv.ft[[length(mod.list)]] <- stan_growth_model_cv(data.to.use,
                                                      model = mod.list[length(mod.list)],
                                                      all_traits = FALSE,
                                                      n.cv = n.cv,
                                                      n.iter = n.its.cv,
                                                      n.chains = n.chains.cv,
                                                      spline_params = list(degree = sp.deg, n_knots = sp.nk),
                                                      cores = parallel::detectCores())
save(mod.cv.ft, file = '~/Dropbox/Post-doc\ stuff/random-stuff/mod_ft_cv.R')

# fit stan model
data.to.use <- data.falster
if (par.run & (Sys.info()['sysname'] == 'Darwin')) {
  mod.falster <- mclapply(1:(2 * length(mod.list) - 1),
                          stan_growth_mod_int,
                          data = data.to.use,
                          mod.list = mod.list2,
                          all_traits = all_traits,
                          validate = TRUE,
                          n.iter = n.iter,
                          n.chains = n.chains,
                          spline_params = list(degree = sp.deg, n_knots = sp.nk),
                          mc.cores = parallel::detectCores())
} else {
  mod.falster <- lapply(1:(2 * length(mod.list) - 1),
                        stan_growth_mod_int,
                        data = data.to.use,
                        mod.list = mod.list2,
                        all_traits = all_traits,
                        validate = TRUE,
                        n.iter = n.iter,
                        n.chains = n.chains,
                        spline_params = list(degree = sp.deg, n_knots = sp.nk))
}
save(mod.falster, file = '~/Dropbox/Post-doc\ stuff/random-stuff/mod_falster_full.R')

# cross validate stan model
mod.cv.falster <- vector('list', length = (length(mod.list) + (length(mod.list) - 1)))
for (i in 1:(length(mod.list) - 1)) {
  print(paste0('Starting cross validation for ', mod.list[i], ' model'))
  mod.cv.falster[[i]] <- stan_growth_model_cv(data.to.use,
                                              model = mod.list[i],
                                              all_traits = FALSE,
                                              n.cv = n.cv,
                                              n.iter = n.its.cv,
                                              n.chains = n.chains.cv,
                                              cores = parallel::detectCores())
  mod.cv.falster[[i + length(mod.list)]] <- stan_growth_model_cv(data.to.use,
                                                                 model = mod.list[i],
                                                                 all_traits = TRUE,
                                                                 n.cv = n.cv,
                                                                 n.iter = n.its.cv,
                                                                 n.chains = n.chains.cv,
                                                                 cores = parallel::detectCores())
}
mod.cv.falster[[length(mod.list)]] <- stan_growth_model_cv(data.to.use,
                                                           model = mod.list[length(mod.list)],
                                                           all_traits = FALSE,
                                                           n.cv = n.cv,
                                                           n.iter = n.its.cv,
                                                           n.chains = n.chains.cv,
                                                           spline_params = list(degree = sp.deg, n_knots = sp.nk),
                                                           cores = parallel::detectCores())
save(mod.cv.falster, file = '~/Dropbox/Post-doc\ stuff/random-stuff/mod_falster_cv.R')

# fit stan model
data.to.use <- data.muir
if (par.run & (Sys.info()['sysname'] == 'Darwin')) {
  mod.muir <- mclapply(1:(2 * length(mod.list) - 1),
                       stan_growth_mod_int,
                       data = data.to.use,
                       mod.list = mod.list2,
                       all_traits = all_traits,
                       validate = TRUE,
                       n.iter = n.iter,
                       n.chains = n.chains,
                       spline_params = list(degree = sp.deg, n_knots = sp.nk),
                       mc.cores = parallel::detectCores())
} else {
  mod.muir <- lapply(1:(2 * length(mod.list) - 1),
                     stan_growth_mod_int,
                     data = data.to.use,
                     mod.list = mod.list2,
                     all_traits = all_traits,
                     validate = TRUE,
                     n.iter = n.iter,
                     n.chains = n.chains,
                     spline_params = list(degree = sp.deg, n_knots = sp.nk))
}
save(mod.muir, file = '~/Dropbox/Post-doc\ stuff/random-stuff/mod_muir_full.R')

# cross validate stan model
mod.cv.muir <- vector('list', length = (length(mod.list) + (length(mod.list) - 1)))
for (i in 1:(length(mod.list) - 1)) {
#for (i in 10:11) {
  print(paste0('Starting cross validation for ', mod.list[i], ' model'))
  mod.cv.muir[[i]] <- stan_growth_model_cv(data.to.use,
                                           model = mod.list[i],
                                           all_traits = FALSE,
                                           n.cv = n.cv,
                                           n.iter = n.its.cv,
                                           n.chains = n.chains.cv,
                                           cores = parallel::detectCores())
  mod.cv.muir[[i + length(mod.list)]] <- stan_growth_model_cv(data.to.use,
                                                              model = mod.list[i],
                                                              all_traits = TRUE,
                                                              n.cv = n.cv,
                                                              n.iter = n.its.cv,
                                                              n.chains = n.chains.cv,
                                                              cores = parallel::detectCores())
}
mod.cv.muir[[length(mod.list)]] <- stan_growth_model_cv(data.to.use,
                                                        model = mod.list[length(mod.list)],
                                                        all_traits = FALSE,
                                                        n.cv = n.cv,
                                                        n.iter = n.its.cv,
                                                        n.chains = n.chains.cv,
                                                        spline_params = list(degree = sp.deg, n_knots = sp.nk),
                                                        cores = parallel::detectCores())
save(mod.cv.muir, file = '~/Dropbox/Post-doc\ stuff/random-stuff/mod_muir_cv.R')

# summarise outputs
if (FALSE) {
  for (i in 1:12) {
    par(mfrow = c(4, 4))
    plot.index <- i
    out.plot <- plot_stan_growth(mod[[plot.index]]$stan_model, data = mod[[plot.index]]$data_set)
  }
}

if (FALSE) {
  model.val <- do.call('rbind', sapply(mod.cv, function(x) unlist(x)))
  model.val <- apply(model.val, c(1, 2), round, 3)
  model.fit.naive <- t(sapply(mod, function(x) c(r2 = x$r2,
                                                 rmsd = x$rmsd,
                                                 md = x$md,
                                                 loo = x$loo$elpd_loo,
                                                 waic = x$waic$elpd_waic)))
  model.fit.naive <- apply(model.fit.naive, c(1, 2), round, 3)
  model.val.stats <- cbind(r2_naive = round(model.fit.naive[, 1], 3),
                           r2_cv = model.val[, 1],
                           rmsd_naive = model.fit.naive[, 2],
                           rmsd_cv = model.val[, 2],
                           md_naive = model.fit.naive[, 3],
                           md_cv = model.val[, 3],
                           elpd_loo_naive = model.fit.naive[, 4],
                           elpd_cv = model.val[, 4],
                           waic_naive = model.fit.naive[, 5])
  rownames(model.val.stats) <- paste(mod.list, c(rep('trait_subset', 11), rep('all_traits', 12)), sep = '_')
}