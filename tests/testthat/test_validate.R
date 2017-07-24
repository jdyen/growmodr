# test the validate functions
library(growmod)
SEED <- 12345
set.seed(SEED)
ITER <- 10
CHAINS <- 2

SW <- suppressWarnings

data_test <- growmod_sim()

capture.output(
  SW(mod1 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS)),
  data_test_1pred <- data_test,
  data_test_1pred$predictors <- data_test_1pred$predictors[, 1],
  SW(mod1pred <- growmod(size ~ (index | block / predictors),
                         data = data_test_1pred,
                         model = 'hillslope',
                         n_iter = ITER,
                         n_chains = CHAINS)),
  data_test_1pred_list <- data_test_1pred,
  data_test_1pred_list$predictors <- lapply(1:3, function(x) data_test_1pred$predictors),
  SW(mod1pred_list <- growmod(size ~ (index | block / predictors),
                              data = data_test_1pred_list,
                              model = 'hillslope',
                              n_iter = ITER,
                              n_chains = CHAINS)),
  SW(mod2 <- growmod(size ~ (index | block),
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS)),
  SW(mod3 <- growmod(size ~ (index | block / predictors),
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS)),
  SW(mod4 <- growmod(size ~ index,
                     data = data_test,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS)),
  SW(mod5 <- growmod(size ~ index,
                     data = data_test,
                     model = 'spline',
                     n_iter = ITER,
                     n_chains = CHAINS,
                     spline_params = list(spline_type = 'ispline'))),
  SW(mod6 <- growmod(size ~ index,
                     data = data_test,
                     model = 'spline',
                     n_iter = ITER,
                     n_chains = CHAINS,
                     spline_params = list(spline_type = 'bspline'))),
  data_test2 <- data_test,
  data_test2$predictors <- lapply(1:3, function(x) data_test2$predictors),
  SW(mod7 <- growmod(size ~ (index | block / predictors),
                     data = data_test2,
                     model = 'hillslope',
                     n_iter = ITER,
                     n_chains = CHAINS)),
  SW(mod_multi <- growmod(size ~ (index | block / predictors),
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
)

expect_valmod <- function(x) expect_s3_class(x, 'growmod_cv')
expect_valmod_names <- function(x) expect_named(x, c('size_real',
                                                     'size_pred',
                                                     'r2',
                                                     'rmsd',
                                                     'md',
                                                     'model',
                                                     'val_type'))

context('cross validate growmod models')
test_that("validate.growmod returns a complete growmod_cv object with loo cv", {
  SW(mod_cv1 <- validate(mod1, n_cv = 'loo'))
  SW(mod_cv1pred <- validate(mod1pred, n_cv = 'loo'))
  SW(mod_cv1pred_list <- validate(mod1pred_list, n_cv = 'loo'))
  SW(mod_cv2 <- validate(mod2, n_cv = 'loo'))
  SW(mod_cv2a <- validate(mod2, n_cv = mod2$data_set$n))
  SW(mod_cv3 <- validate(mod3, n_cv = 'loo'))
  SW(mod_cv3a <- validate(mod4, n_cv = 'loo'))
  SW(mod_cv1a <- validate(mod7, n_cv = 'loo'))
  expect_valmod(mod_cv1)
  expect_valmod_names(mod_cv1)
  expect_valmod(mod_cv2)
  expect_valmod_names(mod_cv2)
  expect_valmod(mod_cv2a)
  expect_valmod_names(mod_cv2a)
  expect_valmod(mod_cv3)
  expect_valmod_names(mod_cv3)
  expect_valmod(mod_cv3a)
  expect_valmod_names(mod_cv3a)
  expect_valmod(mod_cv1a)
  expect_valmod_names(mod_cv1a)
  expect_valmod(mod_cv1pred)
  expect_valmod_names(mod_cv1pred)
  expect_valmod(mod_cv1pred_list)
  expect_valmod_names(mod_cv1pred_list)
})
test_that("validate.growmod returns a complete growmod_cv when stanmodel is not defined", {
  mod1_tmp <- mod1
  mod1_tmp$stanmod <- NULL
  SW(mod_cv1a <- validate(mod1_tmp, n_cv = 'loo'))
  mod5_tmp <- mod5
  mod5_tmp$stanmod <- NULL
  SW(mod_cv5a <- validate(mod5_tmp, n_cv = 'loo'))
  expect_valmod(mod_cv1a)
  expect_valmod_names(mod_cv1a)
  expect_valmod(mod_cv5a)
  expect_valmod_names(mod_cv5a)
})
test_that("validate.growmod_multi returns a complete growmod_cv_multi object", {
  SW(mod_cv_multi <- validate(mod_multi, n_cv = 'loo'))
  expect_s3_class(mod_cv_multi, 'growmod_cv_multi')
  expect_valmod(mod_cv_multi[[1]])
  expect_valmod(mod_cv_multi[[5]])
  expect_valmod_names(mod_cv_multi[[1]])
  expect_valmod_names(mod_cv_multi[[5]])
})
test_that("validate.growmod returns a complete growmod_cv object with k-fold cv (k < num_blocks)", {
  SW(mod_cv4 <- validate(mod1, n_cv = 2))
  SW(mod_cv4a <- validate(mod4, n_cv = 2))
  expect_valmod(mod_cv4)
  expect_valmod_names(mod_cv4)
  expect_valmod(mod_cv4a)
  expect_valmod_names(mod_cv4a)
})
test_that("validate.growmod errors for non-numeric n_cv", {
  expect_error(mod_cv4 <- validate(mod1, n_cv = 'a'))
})
test_that("validate.growmod returns a complete growmod_cv object with k-fold cv (k > num_blocks)", {
  SW(mod_cv5 <- validate(mod1, n_cv = 10))
  expect_valmod(mod_cv5)
  expect_valmod_names(mod_cv5)
  SW(mod_cv5a <- validate(mod4, n_cv = 200))
  expect_valmod(mod_cv5a)
  expect_valmod_names(mod_cv5a)
})
test_that("validate.growmod returns a complete growmod_cv object with a holdout data set", {
  ntest <- 20
  data_tmp <- list(size = sample(data_test$size, size = ntest, replace = TRUE),
                   block = sample(data_test$block, size = ntest, replace = TRUE),
                   index = sample(data_test$index, size = ntest, replace = TRUE))
  data_tmp$predictors <- data_test$predictors[unique(data_tmp$block), ]
  SW(mod_cv6 <- validate(mod1, test_data = data_tmp))
  SW(mod_cv6c <- validate(mod2, test_data = data_tmp))
  SW(mod_cv6d <- validate(mod4, test_data = data_tmp))
  SW(mod_cv6a <- validate(mod5, test_data = data_tmp))
  SW(mod_cv6b <- validate(mod6, test_data = data_tmp))
  data_tmp2 <- data_tmp
  data_tmp2$predictors <- lapply(1:18, function(x) data_tmp2$predictors)
  SW(mod_cv6e <- validate(mod5, test_data = data_tmp2))
  expect_valmod(mod_cv6)
  expect_valmod_names(mod_cv6)
  expect_valmod(mod_cv6a)
  expect_valmod_names(mod_cv6a)
  expect_valmod(mod_cv6b)
  expect_valmod_names(mod_cv6b)
  expect_valmod(mod_cv6c)
  expect_valmod_names(mod_cv6c)
  expect_valmod(mod_cv6d)
  expect_valmod_names(mod_cv6d)
})
test_that("validate.growmod errors on incomplete test data", {
  ntest <- 20
  data_tmp <- list(size = sample(data_test$size, size = ntest, replace = TRUE),
                   block = sample(data_test$block, size = ntest, replace = TRUE),
                   index = sample(data_test$index, size = ntest, replace = TRUE))
  data_tmp$predictors <- data_test$predictors[unique(data_tmp$block), ]
  data_tmp$block <- NULL
  expect_error(mod_cv6 <- validate(mod1, test_data = data_tmp))
  data_tmp <- list(size = sample(data_test$size, size = ntest, replace = TRUE),
                   block = sample(data_test$block, size = ntest, replace = TRUE),
                   index = sample(data_test$index, size = ntest, replace = TRUE))
  data_tmp$size <- NULL
  expect_error(mod_cv6 <- validate(mod1, test_data = data_tmp))
  data_tmp <- list(size = sample(data_test$size, size = ntest, replace = TRUE),
                   block = sample(data_test$block, size = ntest, replace = TRUE),
                   index = sample(data_test$index, size = ntest, replace = TRUE))
  data_tmp$index <- NULL
  expect_error(mod_cv6 <- validate(mod1, test_data = data_tmp))
  data_tmp <- list(size = sample(data_test$size, size = ntest, replace = TRUE),
                   block = sample(data_test$block, size = ntest, replace = TRUE),
                   index = sample(data_test$index, size = ntest, replace = TRUE))
  data_tmp$index <- c(data_tmp$index, 1, 2, 3)
  expect_error(mod_cv6 <- validate(mod1, test_data = data_tmp))
})
test_that("validate.growmod_multi returns a complete growmod_cv object with a holdout data set", {
  ntest <- 20
  data_tmp <- list(size = sample(data_test$size, size = ntest, replace = TRUE),
                   block = sample(data_test$block, size = ntest, replace = TRUE),
                   index = sample(data_test$index, size = ntest, replace = TRUE))
  data_tmp$predictors <- data_test$predictors[unique(data_tmp$block), ]
  SW(mod_cv_multi2 <- validate(mod_multi, test_data = data_tmp))
  expect_valmod(mod_cv_multi2[[4]])
  expect_valmod_names(mod_cv_multi2[[4]])
})

test_that("validate.formula returns a complete growmod_cv object with loo", {
  SW(mod_cv7 <- validate(size ~ (index | block / predictors),
                         data = data_test,
                         model = 'hillslope',
                         n_cv = 'loo',
                         n_iter = ITER,
                         n_chains = CHAINS))
  expect_valmod(mod_cv7)
  expect_valmod_names(mod_cv7)
})
test_that("validate.formula returns a complete growmod_cv object with a holdout data set", {
  ntest <- 20
  data_tmp <- list(size = sample(data_test$size, size = ntest, replace = TRUE),
                   block = sample(data_test$block, size = ntest, replace = TRUE),
                   index = sample(data_test$index, size = ntest, replace = TRUE))
  data_tmp$predictors <- data_test$predictors[unique(data_tmp$block), ]
  SW(mod_cv7a <- validate(size ~ (index | block / predictors),
                         data = data_test,
                         model = 'hillslope',
                         test_data = data_tmp,
                         n_iter = ITER,
                         n_chains = CHAINS))
  expect_valmod(mod_cv7a)
  expect_valmod_names(mod_cv7a)
})

test_that("validate.growmod errors if n_cv and test_data not specified", {
  expect_error(mod_cv8 <- validate(mod1, n_cv = NULL, test_data = NULL))
  expect_error(mod_cv8a <- validate(mod_multi, n_cv = NULL, test_data = NULL))
  expect_error(SW(mod_cv9 <- validate(size ~ (index | block / predictors),
                                      data = data_test,
                                      model = 'hillslope',
                                      n_cv = NULL,
                                      test_data = NULL,
                                      n_iter = ITER,
                                      n_chains = CHAINS)))
  expect_error(mod_cv8a <- validate(mod1, n_cv = 1, test_data = NULL))
})
