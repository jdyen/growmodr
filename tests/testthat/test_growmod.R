# model testing for the growmod.formula and growmod.default functions when fitting
#   a single model

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

context('growmod (full model with blocks and predictors)')
test_that("growmod.formula returns a complete growmod object for hillslope models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for hillslope_log models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'hillslope_log',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for power2 models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'power2',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for expo models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'expo',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for monod models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'monod',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for neg_exp models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'neg_exp',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for koblog models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'koblog',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for power3 models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'power3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for logistic3 models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'logistic3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a growmod object for hillslope models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'archibold',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for weibull3 models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'weibull3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for spline models", {
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'spline',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("spline models work with different spline models", {
  expect_error(SW(mod1 <- growmod(size ~ (index | block / predictors),
                                  data = data_test,
                                  model = 'spline',
                                  spline_params = c(degree = 8,
                                                    n_knots = 10,
                                                    spline_type = 'jspline'),
                                  n_iter = ITER,
                                  n_chains = CHAINS)))
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'spline',
                     spline_params = c(degree = 8,
                                       n_knots = 10,
                                       spline_type = 'bspline'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula works when predictors are defined in a list", {
  data_test2 <- data_test
  data_test2$predictors <- lapply(1:3, function(x) data_test2$predictors)
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test2,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("spline model warns when predictors are provided as a list", {
  data_test2 <- data_test
  data_test2$predictors <- lapply(1:18, function(x) data_test2$predictors)
  expect_warning(mod1 <- growmod(size ~ (index | block / predictors),
                                 data = data_test2,
                                 model = 'spline',
                                 n_iter = ITER,
                                 n_chains = CHAINS))
})
test_that("spline model warns when spline_params are inappropriate", {
  expect_warning(mod1 <- growmod(size ~ (index | block / predictors),
                                 data = data_test,
                                 model = 'spline',
                                 spline_params = list(degree = 10,
                                                      n_knots = 25,
                                                      spline_type = 'ispline'),
                                 n_iter = ITER,
                                 n_chains = CHAINS))
})
test_that("spline model works when spline_params are incomplete", {
  expect_warning(mod1 <- growmod(size ~ (index | block / predictors),
                                 data = data_test,
                                 model = 'spline',
                                 spline_params = list(spline_type = 'bspline'),
                                 n_iter = ITER,
                                 n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula warns when predictors are provided per observation", {
  data_test2 <- data_test
  data_test2$predictors <- data_test2$predictors[sample(1:nrow(data_test2$predictors),
                                                        size = length(data_test2$size),
                                                        replace = TRUE), ]
  expect_warning(SW(mod1 <- growmod(size ~ (index | block / predictors),
                                    data = data_test2,
                                    model = 'hillslope',
                                    n_iter = ITER,
                                    n_chains = CHAINS)))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula errors when predictors do not match blocks or observations", {
  data_test2 <- data_test
  data_test2$predictors <- data_test2$predictors[sample(1:nrow(data_test2$predictors),
                                                        size = (length(data_test2$size) - 4),
                                                        replace = TRUE), ]
  expect_error(SW(mod1 <- growmod(size ~ (index | block / predictors),
                                  data = data_test2,
                                  model = 'hillslope',
                                  n_iter = ITER,
                                  n_chains = CHAINS)))
})

context('growmod (model with blocks but no predictors)')
test_that("growmod.formula returns a complete growmod object for hillslope models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for hillslope_log models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'hillslope_log',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for power2 models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'power2',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for expo models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'expo',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for monod models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'monod',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for neg_exp models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'neg_exp',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for koblog models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'koblog',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for power3 models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'power3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for logistic3 models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'logistic3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a growmod object for hillslope models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'archibold',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for weibull3 models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'weibull3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for spline models", {
  SW(mod1 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'spline',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})


context('growmod (model with no blocks and no predictors)')
test_that("growmod.formula returns a complete growmod object for hillslope models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for hillslope_log models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'hillslope_log',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for power2 models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'power2',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for expo models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'expo',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for monod models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'monod',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for neg_exp models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'neg_exp',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for koblog models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'koblog',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for power3 models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'power3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for logistic3 models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'logistic3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a growmod object for hillslope models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'archibold',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for weibull3 models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'weibull3',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula returns a complete growmod object for spline models", {
  SW(mod1 <- growmod(size ~ index,
                     data = data_test,
                     model = 'spline',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})

context('growmod.default (hillslope only)')
test_that("growmod.default returns a complete growmod object for hillslope models", {
  SW(mod1 <- growmod(x = data_test$size,
                     index = data_test$index,
                     block = data_test$block,
                     predictors = data_test$predictors,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_burnin = floor(ITER / 2),
                     n_thin = 1,
                     n_chains = CHAINS,
                     stan_cores = 1,
                     spline_params = list(degree = 8,
                                          n_knots = 10,
                                          spline_type = 'ispline')))
  SW(mod2 <- growmod(x = data_test$size,
                     index = data_test$index,
                     block = data_test$block,
                     predictors = NULL,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_burnin = floor(ITER / 2),
                     n_thin = 1,
                     n_chains = CHAINS,
                     stan_cores = 1,
                     spline_params = list(degree = 8,
                                          n_knots = 10,
                                          spline_type = 'ispline')))
  SW(mod3 <- growmod(x = data_test$size,
                     index = data_test$index,
                     block = NULL,
                     predictors = NULL,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_burnin = floor(ITER / 2),
                     n_thin = 1,
                     n_chains = CHAINS,
                     stan_cores = 1,
                     spline_params = list(degree = 8,
                                          n_knots = 10,
                                          spline_type = 'ispline')))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
  expect_growmod(mod2)
  expect_growmod_names(mod2)
  expect_growmod(mod3)
  expect_growmod_names(mod3)
})
