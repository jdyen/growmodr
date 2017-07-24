# test the growmod_sim function
library(growmod)
SEED <- 12345
set.seed(SEED)

SW <- suppressWarnings

capture.output(
  SW(data_test1 <- growmod_sim(n = 100,
                               nblock = 5,
                               age_range = c(0, 50),
                               include_predictors = TRUE,
                               true_model = 'hillslope')),
  SW(data_test2 <- growmod_sim(n = 100,
                               nblock = 5,
                               age_range = c(0, 50),
                               include_predictors = FALSE,
                               true_model = 'hillslope')),
  SW(data_test3 <- growmod_sim(n = 200,
                               nblock = 5,
                               age_range = c(0, 50),
                               include_predictors = TRUE,
                               true_model = 'hillslope')),
  SW(data_test4 <- growmod_sim(n = 200,
                               nblock = 20,
                               age_range = c(0, 50),
                               include_predictors = TRUE,
                               true_model = 'hillslope'))
)

context("test growmod_sim")
test_that("growmod_sim returns the correct outputs", {
  expect_named(data_test1, c("size",
                             "block",
                             "index",
                             "predictors",
                             "coefs"))
  expect_named(data_test2, c("size",
                             "block",
                             "index",
                             "predictors",
                             "coefs"))
  expect_named(data_test3, c("size",
                             "block",
                             "index",
                             "predictors",
                             "coefs"))
  expect_named(data_test4, c("size",
                             "block",
                             "index",
                             "predictors",
                             "coefs"))
})
test_that("growmod_sim returns predictors if required or null if not", {
  expect_null(data_test2$predictors)
  expect_null(data_test2$coefs)
  expect_is(data_test1$predictors, "matrix")
  expect_is(data_test1$coefs, "matrix")
})
test_that("growmod_sim corrects the number of obs per block", {
  SW(data_test1 <- growmod_sim(n = 100,
                               nblock = 3,
                               age_range = c(0, 50),
                               include_predictors = TRUE,
                               true_model = 'hillslope'))
})
