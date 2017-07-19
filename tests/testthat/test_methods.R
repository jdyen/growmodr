# test methods for growmod S3 class
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
  SW(mod_multi <- growmod(size ~ (index | block / predictors),
                          data = data_test,
                          model = c('hillslope',
                                    'power2'),
                          n_iter = ITER,
                          n_chains = CHAINS)),
  SW(mod_cv <- validate(mod1, n_cv = 'loo')),
  SW(mod_cv_multi <- validate(mod_multi, n_cv = 'loo'))
)

context("methods for growmod objects")

test_that("growmod extractor methods work correctly", {
  expect_equal(fitted(mod1), mod1$fitted)
  expect_equal(residuals(mod1), c(mod1$data_set$size_data - mod1$fitted))
  expect_equal(fitted(mod2), mod2$fitted)
  expect_equal(residuals(mod2), c(mod2$data_set$size_data - mod2$fitted))
  expect_equal(fitted(mod3), mod3$fitted)
  expect_equal(residuals(mod3), c(mod3$data_set$size_data - mod3$fitted))
  expect_equivalent(fitted(mod_multi), lapply(mod_multi, function(x) x$fitted))
  expect_equivalent(residuals(mod_multi), lapply(mod_multi,
                                                 function(x) c(x$data_set$size_data - x$fitted)))
  expect_equal(fitted(mod_cv), mod_cv$size_pred)
  expect_equal(residuals(mod_cv), c(mod_cv$size_real - mod_cv$size_pred))
  expect_equivalent(fitted(mod_cv_multi), lapply(mod_cv_multi,
                                                 function(x) x$size_pred))
  expect_equivalent(residuals(mod_cv_multi), lapply(mod_cv_multi,
                                                    function(x) c(x$size_real - x$size_pred)))
})

test_that("print, summary and compare methods work correctly", {
  expect_output(print(mod1), "hillslope")
  expect_output(print(mod2), "hillslope")
  expect_output(print(mod3), "hillslope")
  expect_output(print(mod_multi), "power2")
  expect_output(summary(mod1), "summary statistics")
  expect_output(summary(mod2), "summary statistics")
  expect_output(summary(mod3), "summary statistics")
  expect_output(summary(mod_multi), "models were fitted")
  expect_length(compare(mod1, mod2, mod3), 15)
  expect_length(compare(mod_multi), 10)
  expect_output(print(mod_cv), "hillslope")
  expect_output(print(mod_cv_multi), "power2")
  expect_output(summary(mod_cv), "model was validated")
  expect_output(summary(mod_cv_multi), "models were validated")
  expect_length(compare(mod_cv, mod_cv), 6)
  expect_length(compare(mod_cv_multi), 6)
})

test_that("plot methods work correctly", {
  expect_silent(plot(mod1))
  expect_silent(plot(mod2))
  expect_silent(plot(mod3))
  expect_warning(plot(mod_multi))
  expect_silent(plot(mod_cv))
  expect_silent(plot(mod_cv_multi))
})
