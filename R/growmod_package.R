#' growmod: Growth Curve Modelling and Validation with Stan
#' @name growmod-package
#' @docType package
#' @aliases growmod-package
#'
#' @useDynLib growmod, .registration = TRUE
#'
#' @author Jian Yen, \email{jdl.yen@gmail.com}
#' @author Freya Thomas
#' 
#' @description R package to fit growth models to data on size at age. Supports
#'        multiple model types, including standard growth-curve fitting, growth
#'        curves that differ among blocks in a data set (e.g. different growth
#'        curves for each species in a data set), and hierarchical models of growth
#'        curves where curve parameters depend on a predictor variable (e.g., a
#'        species's traits). Includes methods for model validation, comparison,
#'        and plotting.
#' 
#' @examples
#' \dontrun{
#'   # simulate some data
#'   data_sim <- growmod_sim(n = 100,
#'                           nblock = 5,
#'                           age_range = c(0, 50),
#'                           include_predictors = TRUE,
#'                           true_model = 'hillslope')
#'
#'   # fit the correct model
#'   mod1 <- growmod(size ~ (index | block / predictors),
#'                   data = data_sim,
#'                   model = 'hillslope',
#'                   n_iter = 1000,
#'                   n_burnin = 500,
#'                   n_chains = 2,
#'                   stan_cores = 1)
#'
#'   # plot the fitted model
#'   plot(mod1)
#'   
#'   # summarise the fitted model
#'   summary(mod1)
#'   
#'   # print the fitted model
#'   print(mod1)
#'
#'   # cross validate the fitted model
#'   mod1_cv <- validate(mod1)
#'                    
#'   # fit an incorrect model
#'   mod2 <- growmod(size ~ (index | block / predictors),
#'                   data = data_sim,
#'                   model = 'koblog',
#'                   n_iter = 1000,
#'                   n_burnin = 500,
#'                   n_chains = 2,
#'                   stan_cores = 1)
#'
#'   # compare the fitted models using summary measures of fit
#'   compare(mod1, mod2)
#'   
#'   # compare the fitted models using cross validation
#'   mod2_cv <- validate(mod2)
#'   compare(mod1_cv, mod2_cv)
#'   
#'   # example of multiple models fitted in one call
#'   mod_multi <- growmod(size ~ (index | block / predictors),
#'                        data = data_sim,
#'                        model = c('hillslope',
#'                                  'koblog'),
#'                        n_iter = 1000,
#'                        n_burnin = 500,
#'                        n_chains = 2,
#'                        stan_cores = 1)
#'   
#' }
NULL
