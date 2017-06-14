# simulate growth data

#' @name growmod_sim
#' @title Simulate growth curve data from a chosen growth curve
#' @description As above
#' @export
#' 
#' @param n number of simulated growth curves
#' @param nblock number of blocks (e.g. species) to include in simulated data
#' @param age_range range of ages for simulated data
#' @param include_predictors logical; should growth curve parameters depend on predictor
#'      variables that are specific to each block?
#' @param true_model the true growth curve model to use when simulating growth curves
#' 
#' @details Does XYZ
#' 
#' @return list containing simulated data
#' 
#' @examples 
#' \dontrun {
#'   sim_data <- growth_data_sim(n = 100,
#'                               nblock = 5,
#'                               age_range = c(0, 1),
#'                               include_predictors = TRUE,
#'                               true_model = 'hillslope')
#' }
#'
growmod_sim <- function(n = 100,
                        nblock = 5,
                        age_range = c(0, 20),
                        include_predictors = TRUE,
                        true_model = 'hillslope') {
  # simulate data
  block_size <- rep(floor(n / nblock), nblock)
  if (sum(block_size) != n) {
    block_size[nblock] <- n - sum(block_size[1:(length(block_size) - 1)])
  }
  sim_data <- NULL
  block_id <- NULL
  index <- NULL
  if (include_predictors) {
    npred <- 2
    predictors <- matrix(rnorm((npred * nblock), sd = 0.5), ncol = npred)
    true_coefs <- matrix(rnorm((3 * npred)), ncol = 3)
    h1_set <- abs(predictors %*% true_coefs[, 1]) + 10
    h2_set <- abs(predictors %*% true_coefs[, 2]) + 0.5
    h3_set <- abs(predictors %*% true_coefs[, 3]) + 0.5
  } else {
    h1_set <- runif(nblock, min = 0.5, max = 1.5) + 10
    h2_set <- runif(nblock, min = 0.5, max = 1.5)
    h3_set <- runif(nblock, min = 0.5, max = 1.5)
    predictors <- NULL
    true_coefs <- NULL
  }
  for (i in seq(along = block_size)) {
    sim_data_tmp <- sim_growth_curve(block_size[i],
                                     model = true_model,
                                     age_range = age_range,
                                     h1 = h1_set[i],
                                     h2 = h2_set[i],
                                     h3 = h3_set[i])
    sim_data <- c(sim_data, sim_data_tmp$size)
    index <- c(index, sim_data_tmp$index)
    block_id <- c(block_id, rep(i, block_size[i]))
  }
  out <- list(size = ifelse(sim_data > 0.1, sim_data, 0.1),
              block = block_id,
              index = index,
              predictors = predictors,
              coefs = true_coefs)
  out
}

sim_growth_curve <- function(n, model, age_range, h1, h2, h3) {
  eqn_set <- switch(model, 
                    'hillslope' = function(x, h1, h2, h3) h1 / (1 + exp(-h2 * (x - h3))),
                    'hillslope_log' = function(x, h1, h2, h3) h1 / (1 + exp(-h2 * (log(x) - h3))),
                    'power2' = function(x, h1, h2, h3) h1 * (x ** h2),
                    'expo' = function(x, h1, h2, h3) h1 + (h2 * log(x)),
                    'monod' = function(x, h1, h2, h3) h1 * (x / (h2 + x)),
                    'neg_exp' = function(x, h1, h2, h3) h1 * (1 - exp(-h2 * x)),
                    'koblog' = function(x, h1, h2, h3) h1 * log(1 + (x / h2)),
                    'power3' = function(x, h1, h2, h3) h1 * (x ** (h2 - (h3 / x))),
                    'logistic3' = function(x, h1, h2, h3) h1 / (1 + exp(-h2 * x + h3)),
                    'archibold' = function(x, h1, h2, h3) h1 / (h2 + (h3 ** x)),
                    'weibull3' = function(x, h1, h2, h3) h1 * (1 - exp(-h2 * (x ** h3))))
  x_vals <- seq(age_range[1], age_range[2], length = (5 * n))
  out_tmp <- eqn_set(x_vals, h1, h2, h3)
  index_id <- sample(1:length(x_vals), size = n, replace = TRUE)
  size_out <- out_tmp[index_id] + rnorm(n, sd = (sd(out_tmp) / 2))
  x_out <- x_vals[index_id]
  out <- list(size = size_out,
              index = x_out)
  out
  
}