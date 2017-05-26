# summarise fitted growth curve models
rm(list = ls(all = TRUE))

setwd('~/Dropbox/GrowthModels/')

source('./stan-code/stan-summary-fun.R')

load('../Post-doc stuff/random-stuff/mod_ft_full.R')
load('../Post-doc stuff/random-stuff/mod_ft_cv.R')
# summarise outputs
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
mod <- mod.ft
mod.cv <- mod.cv.ft
for (i in 1:12) {
  par(mfrow = c(4, 4))
  pdf(file = paste0('./outputs/ft_', mod.list[i], '_fitted.pdf'), height = 10, width = 10)
  out.plot <- plot_stan_growth(mod[[i]]$stan_model, data = mod[[i]]$data_set)
  dev.off()
}

model.val <- do.call('rbind', lapply(mod.cv, function(x) unlist(x)))
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
write.csv(model.val.stats, file = './outputs/val_stats_ft.csv')

rm('mod', 'mod.cv', 'mod.ft', 'mod.cv.ft')

load('../Post-doc stuff/random-stuff/mod_falster_full.R')
load('../Post-doc stuff/random-stuff/mod_falster_cv.R')
mod <- mod.falster
mod.cv <- mod.cv.falster
for (i in 1:12) {
  par(mfrow = c(4, 4))
  pdf(file = paste0('./outputs/falster_', mod.list[i], '_fitted.pdf'), height = 10, width = 10)
  out.plot <- plot_stan_growth(mod[[i]]$stan_model, data = mod[[i]]$data_set)
  dev.off()
}

model.val <- do.call('rbind', lapply(mod.cv, function(x) unlist(x)))
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
write.csv(model.val.stats, file = './outputs/val_stats_falster.csv')

rm('mod', 'mod.cv', 'mod.falster', 'mod.cv.falster')

load('../Post-doc stuff/random-stuff/mod_muir_full.R')
load('../Post-doc stuff/random-stuff/mod_muir_cv.R')
mod <- mod.muir
mod.cv <- mod.cv.muir
for (i in 1:12) {
  par(mfrow = c(4, 4))
  pdf(file = paste0('./outputs/muir_', mod.list[i], '_fitted.pdf'), height = 10, width = 10)
  out.plot <- plot_stan_growth(mod[[i]]$stan_model, data = mod[[i]]$data_set)
  dev.off()
}

model.val <- do.call('rbind', lapply(mod.cv, function(x) unlist(x)))
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
write.csv(model.val.stats, file = './outputs/val_stats_muir.csv')
