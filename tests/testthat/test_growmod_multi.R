# test growmod fit to multiple models at once
library(growmodr)
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
test_that("growmod.formula works for multiple models with blocks and predictors as list", {
  data_test2 <- data_test
  data_test2$predictors <- lapply(1:3, function(x) data_test2$predictors)
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test2,
                     model = c('hillslope',
                               'hillslope_log',
                               'power3',
                               'hillslope'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  SW(mod1a <- growmod(size ~ (index | block / predictors),
                     data = data_test2,
                     model = c('hillslope',
                               'hillslope_log',
                               'hillslope'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1[[1]])
  expect_growmod_names(mod1[[1]])
  expect_growmod(mod1[[3]])
  expect_growmod_names(mod1[[3]])
  expect_growmod(mod1a[[3]])
  expect_growmod_names(mod1a[[3]])
  data_test2$predictors <- 1:10
  expect_error(growmod(size ~ (index | block / predictors),
                       data = data_test2,
                       model = c('hillslope',
                                 'hillslope_log',
                                 'power3'),
                       n_iter = ITER,
                       n_chains = CHAINS))
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
