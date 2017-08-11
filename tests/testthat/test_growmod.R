# model testing for the growmod.formula and growmod.default functions when fitting
#   a single model

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
test_that("growmod.formula handles NAs in size data for hillslope models", {
  size_tmp <- data_test$size
  size_tmp[c(1, 5, 10)] <- NA
  SW(mod1 <- growmod(size_tmp ~ (index | block / predictors),
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula handles NAs in index data for hillslope models", {
  index_tmp <- data_test$index
  index_tmp[c(1, 5, 10)] <- NA
  SW(mod1 <- growmod(size ~ (index_tmp | block / predictors),
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula handles NAs in block data for hillslope models", {
  data_test2 <- data_test
  data_test2$block[c(1, 5, 10)] <- NA
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test2,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula finds predictors in parent env", {
  data_test2 <- data_test
  pred_tmp <- data_test2$predictors
  data_test2$predictors <- NULL
  SW(mod1 <- growmod(size ~ (index | block / pred_tmp),
                     data = data_test2,
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
test_that("growmod.formula returns a growmod object for archibold models", {
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
  expect_warning(mod1 <- growmod(size ~ (index | block / predictors),
                                 data = data_test,
                                 model = 'spline',
                                 spline_params = list(degree = 8,
                                                      n_knots = 10,
                                                      spline_type = 'jspline'),
                                 n_iter = ITER,
                                 n_chains = CHAINS))
  expect_warning(mod1 <- growmod(size ~ index,
                                 data = data_test,
                                 model = 'spline',
                                 spline_params = list(degree = 8,
                                                      n_knots = 10,
                                                      spline_type = 'jspline'),
                                 n_iter = ITER,
                                 n_chains = CHAINS))
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'spline',
                     spline_params = list(degree = 8,
                                          n_knots = 10,
                                          spline_type = 'bspline'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  SW(mod2 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'spline',
                     spline_params = list(degree = 8,
                                          n_knots = 10,
                                          spline_type = 'bspline'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  SW(mod3 <- growmod(size ~ index,
                     data = data_test,
                     model = 'spline',
                     spline_params = list(degree = 8,
                                          n_knots = 10,
                                          spline_type = 'bspline'),
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
  expect_growmod(mod2)
  expect_growmod_names(mod2)
  expect_growmod(mod3)
  expect_growmod_names(mod3)
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
test_that("growmod.formula works when predictors are defined in a list with one row per observation", {
  data_test2 <- data_test
  data_test2$predictors <- lapply(1:3, function(x) data_test2$predictors)
  for (i in seq_along(data_test2$predictors)) {
    data_test2$predictors[[i]] <- data_test2$predictors[[i]][sample(1:nrow(data_test2$predictors[[i]]),
                                                                    size = length(data_test2$size),
                                                                    replace = TRUE), ]
  }
  expect_warning(mod1 <- growmod(size ~ (index | block / predictors),
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
test_that("spline model works when spline_params are incomplete test number 2", {
  expect_warning(mod1 <- growmod(size ~ (index | block / predictors),
                                 data = data_test,
                                 model = 'spline',
                                 spline_params = list(spline_type = NULL),
                                 n_iter = ITER,
                                 n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula errors when predictors are defined in a list with the wrong number of parameters", {
  data_test2 <- data_test
  data_test2$predictors <- lapply(1:4, function(x) data_test2$predictors)
  expect_error(mod1 <- growmod(size ~ (index | block / predictors),
                               data = data_test2,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
})
test_that("growmod.formula errors when predictors are defined in a list with the wrong number of observations in one element", {
  data_test2 <- data_test
  data_test2$predictors <- lapply(1:3, function(x) data_test2$predictors)
  data_test2$predictors[[1]] <- rbind(data_test2$predictors[[1]], data_test2$predictors[[1]])
  expect_error(mod1 <- growmod(size ~ (index | block / predictors),
                               data = data_test2,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
})
test_that("growmod.formula warns when predictors are provided per observation", {
  data_test2 <- data_test
  data_test2$predictors <- data_test2$predictors[sample(1:nrow(data_test2$predictors),
                                                        size = length(data_test2$size),
                                                        replace = TRUE), ]
  expect_warning(mod1 <- growmod(size ~ (index | block / predictors),
                                 data = data_test2,
                                 model = 'hillslope',
                                 n_iter = ITER,
                                 n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula errors when predictors do not match blocks or observations", {
  data_test2 <- data_test
  data_test2$predictors <- data_test2$predictors[sample(1:nrow(data_test2$predictors),
                                                        size = (length(data_test2$size) - 4),
                                                        replace = TRUE), ]
  expect_error(mod1 <- growmod(size ~ (index | block / predictors),
                                  data = data_test2,
                                  model = 'hillslope',
                                  n_iter = ITER,
                                  n_chains = CHAINS))
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
test_that("growmod.formula returns a growmod object for archibold models", {
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
test_that("growmod.formula returns a complete growmod object when data are in parent.frame", {
  size2 <- data_test$size
  index2 <- data_test$index
  block2 <- data_test$block
  predictors2 <- data_test$predictors
  SW(mod1 <- growmod(size2 ~ (index2 | block2 / predictors2),
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
})
test_that("growmod.formula errors when dimensions do not match", {
  data_test2 <- data_test
  data_test2$index <- c(data_test2$index, 1, 2, 3)
  expect_error(mod1 <- growmod(size ~ (index | block),
                               data = data_test2,
                               model = 'spline',
                               n_iter = ITER,
                               n_chains = CHAINS))
  data_test2 <- data_test
  data_test2$block <- c(data_test2$block, 1, 2, 3)
  expect_error(mod1 <- growmod(size ~ (index | block),
                               data = data_test2,
                               model = 'spline',
                               n_iter = ITER,
                               n_chains = CHAINS))
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
test_that("growmod.formula returns a growmod object for archibold models", {
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
  expect_error(mod3a <- growmod(x = cbind(data_test$size, data_test$size),
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
  data_test2 <- data_test
  data_test2$predictors <- lapply(1:3, function(x) data_test2$predictors)
  SW(mod4 <- growmod(x = data_test2$size,
                     index = data_test2$index,
                     block = data_test2$block,
                     predictors = data_test2$predictors,
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
  expect_growmod(mod4)
  expect_growmod_names(mod4)
})
context('growmod.default (spline only)')
test_that("growmod.default returns a complete growmod object for spline models", {
  SW(mod1 <- growmod(x = data_test$size,
                     index = data_test$index,
                     block = data_test$block,
                     predictors = data_test$predictors,
                     model = 'spline',
                     n_iter = ITER,
                     n_burnin = floor(ITER / 2),
                     n_thin = 1,
                     n_chains = CHAINS,
                     stan_cores = 1,
                     spline_params = list(degree = 10,
                                          n_knots = 25,
                                          spline_type = NULL)))
  SW(mod1a <- growmod(x = data_test$size,
                      index = data_test$index,
                      block = data_test$block,
                      predictors = data_test$predictors,
                      model = 'spline',
                      n_iter = ITER,
                      n_burnin = floor(ITER / 2),
                      n_thin = 1,
                      n_chains = CHAINS,
                      stan_cores = 1,
                      spline_params = list(degree = NULL,
                                           n_knots = NULL,
                                           spline_type = NULL)))
  expect_error(mod2 <- growmod(x = data_test$size,
                               index = c(data_test$index, 1, 2, 3),
                               block = data_test$block,
                               predictors = NULL,
                               model = 'spline',
                               n_iter = ITER,
                               n_burnin = floor(ITER / 2),
                               n_thin = 1,
                               n_chains = CHAINS,
                               stan_cores = 1,
                               spline_params = list(degree = 8,
                                                    n_knots = 10,
                                                    spline_type = 'ispline')))
  expect_error(mod3 <- growmod(x = data_test$size,
                               index = data_test$index,
                               block = c(data_test$block, 1, 2, 3),
                               predictors = NULL,
                               model = 'spline',
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
  expect_growmod(mod1a)
  expect_growmod_names(mod1a)
})
test_that("growmod.formula errors on incorrect parameters", {
  size <- data_test$size
  index <- data_test$index
  block <- data_test$block
  predictors <- data_test$predictors
  expect_error(mod1 <- growmod(treesize ~ (index | block / predictors),
                               data = data_test,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
  expect_error(mod1 <- growmod(treesize ~ (index | block / predictors),
                               data = NULL,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
  expect_error(mod1 <- growmod(size ~ (treeindex | block / predictors),
                               data = data_test,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
  expect_error(mod1 <- growmod(size ~ (treeindex | block / predictors),
                               data = NULL,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = NULL,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS))
  expect_growmod(mod1)
  expect_growmod_names(mod1)
  expect_error(mod1 <- growmod(size ~ (index | block / treepredictors),
                               data = data_test,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
  expect_error(mod1 <- growmod(size ~ (index | block / treepredictors),
                               data = NULL,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
  expect_error(mod1 <- growmod(size ~ (index | treeblock / predictors),
                               data = data_test,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
  expect_error(mod1 <- growmod(size ~ (index | treeblock / predictors),
                               data = NULL,
                               model = 'hillslope',
                               n_iter = ITER,
                               n_chains = CHAINS))
})
