# test growmod fit to multiple models at once
library(growmod)
SEED <- 12345
set.seed(SEED)
ITER <- 10
CHAINS <- 2

SW <- suppressWarnings

data_test <- growmod_sim()

expect_growmod <- function(x) expect_s3_class(x, 'growmod')
expect_growmod_names <- function(x) expect_named(x, c('fitted',
                                                      'r2',
                                                      'rmsd',
                                                      'md',
                                                      'loo',
                                                      'waic',
                                                      'stan_summary',
                                                      'data_set',
                                                      'predictors',
                                                      'model',
                                                      'stanmod',
                                                      'spline_params',
                                                      'stan_cores',
                                                      'n_iter',
                                                      'n_burnin',
                                                      'n_thin',
                                                      'n_chains',
                                                      'formula',
                                                      'call'))

context('cross validate growmod models')
test_that("growmod.formula works for multiple models with blocks and predictors", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = c('hillslope',
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
                               'spline'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1[[1]])
  expect_growmod_names(mod1[[1]])
  expect_growmod(mod1[[5]])
  expect_growmod_names(mod1[[5]])
  expect_growmod(mod1[[11]])
  expect_growmod_names(mod1[[11]])
})

test_that("growmod.formula works for multiple models with blocks but no predictors", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = c('hillslope',
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
                               'spline'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1[[1]])
  expect_growmod_names(mod1[[1]])
  expect_growmod(mod1[[5]])
  expect_growmod_names(mod1[[5]])
  expect_growmod(mod1[[11]])
  expect_growmod_names(mod1[[11]])
})

test_that("growmod.formula works for multiple models with no blocks or predictors", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = c('hillslope',
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
                               'spline'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1[[1]])
  expect_growmod_names(mod1[[1]])
  expect_growmod(mod1[[5]])
  expect_growmod_names(mod1[[5]])
  expect_growmod(mod1[[11]])
  expect_growmod_names(mod1[[11]])
})
