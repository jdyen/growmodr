summary_fun <- function(x) {
  out <- c(mean(x),
           sd(x),
           min(x),
           quantile(x, p = c(0.025, 0.25, 0.5, 0.75, 0.975)),
           max(x))
  names(out) <- c('mean', 'sd', 'min', '2.5%', '25%', '50%', '75%', '97.5%', 'max')
  out
}

stan_model_fit <- function(mod) {
  out <- c(r2 = mod$r2,
           rmsd = mod$rmsd,
           md = mod$md,
           loo = mod$loo$elpd_loo,
           waic = mod$waic$elpd_waic)
  out
}

stan_model_diagnose <- function(mod) {
  out <- summary(mod)$summary[, c('n_eff', 'Rhat')]
  out
}

stan_summary_fun <- function(mod,
                             param = NULL,
                             burnin = NULL,
                             full_output = FALSE) {
  out.tmp <- mod@sim$samples
  n.iter <- length(out.tmp[[1]][[1]])
  if (is.null(burnin)) {
    n.burnin <- round(n.iter / 2)
  }
  if (is.null(param)) {
    param <- 'alpha'
  }
  list.id <- grep(param, names(out.tmp[[1]]))
  out <- lapply(out.tmp,
                function(x) sapply(x[list.id],
                                   function(y) y[(n.burnin + 1):n.iter]))
  out <- do.call('rbind', out)
  mcmc.summary <- t(apply(out, 2, summary_fun))
  if (full_output) {
    out
  } else {
    mcmc.summary
  }
}

plot_stan_growth <- function(mod, data = NULL,
                             data_set_label = NULL,
                             species_to_plot = 'all') {
  if (class(mod) != 'stanfit') {
    tmp <- mod[grep('h_plot\\[', rownames(mod)), ]
  } else {
    tmp <- stan_summary_fun(mod, param = 'h_plot\\[')
  }
  n.plot <- length(grep(',1\\]', rownames(tmp)))
  n.sp <- length(grep('\\[1,', rownames(tmp)))
  plot.data <- array(NA, dim = c(n.plot, 4, n.sp))
  if (species_to_plot == 'all') {
    for (i in 1:n.sp) {
      row.id <- grep(paste0(',', i, '\\]'), rownames(tmp))
      plot.data[, , i] <- tmp[row.id, c('mean', '2.5%', '50%', '97.5%')]
      if (!is.null(data)) {
        x.plot <- data$t_plot
        y.lim <- c(min(c(plot.data[, , i], data$h[which(data$sp == i)])),
                   max(c(plot.data[, , i], data$h[which(data$sp == i)])))
      } else {
        x.plot <- 1:dim(plot.data)[1]
        y.lim <- c(min(plot.data[, , i]), max(plot.data[, , i]))
      }
      plot(plot.data[, 1, i] ~ x.plot,
           type = 'l',
           las = 1,
           bty = 'l',
           xlab = 'Time',
           ylab = 'Height',
           ylim = y.lim)
      polygon(c(x.plot, rev(x.plot)),
              c(plot.data[, 2, i], rev(plot.data[, 4, i])),
              col = 'grey65',
              border = NA)
      lines(plot.data[, 1, i] ~ x.plot, lty = 1, lwd = 1.2)
      lines(plot.data[, 3, i] ~ x.plot, lty = 2, lwd = 1.2)
      if (!is.null(data)) {
        points(data$h[which(data$sp == i)] ~ data$t[which(data$sp == i)],
               pch = 16)
      }
      mtext(paste0('Species ', i), side = 3, line = 1, adj = 0)
      if (!is.null(data_set_label)) {
        mtext(data_set_label, side = 3, line = 1, adj = 1)
      }
    }
  } else {
    i <- species_to_plot
    row.id <- grep(paste0(',', i, '\\]'), rownames(tmp))
    plot.data[, , i] <- tmp[row.id, c('mean', '2.5%', '50%', '97.5%')]
    if (!is.null(data)) {
      x.plot <- data$t_plot
      y.lim <- c(min(c(plot.data[, , i], data$h[which(data$sp == i)])),
                 max(c(plot.data[, , i], data$h[which(data$sp == i)])))
    } else {
      x.plot <- 1:dim(plot.data)[1]
      y.lim <- c(min(plot.data[, , i]), max(plot.data[, , i]))
    }
    plot(plot.data[, 1, i] ~ x.plot,
         type = 'l',
         las = 1,
         bty = 'l',
         xlab = '',
         ylab = '',
         ylim = y.lim)
    mtext('Time', side = 1, line = 2.5, adj = 0.5, cex = 0.9)
    mtext('Height (cm)', side = 2, line = 2.5, adj = 0.5, cex = 0.9)
    polygon(c(x.plot, rev(x.plot)),
            c(plot.data[, 2, i], rev(plot.data[, 4, i])),
            col = 'grey65',
            border = NA)
    lines(plot.data[, 1, i] ~ x.plot, lty = 1, lwd = 1.2)
    lines(plot.data[, 3, i] ~ x.plot, lty = 2, lwd = 1.2)
    if (!is.null(data)) {
      points(data$h[which(data$sp == i)] ~ data$t[which(data$sp == i)],
             pch = 16)
    }
    if (!is.null(data_set_label)) {
      mtext(data_set_label, side = 3, line = 1, adj = 1)
    }
  }
}