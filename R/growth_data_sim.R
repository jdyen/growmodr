# simulate growth data

#' @name growth_data_sim
#' @title Simulate growth curve data from a chosen growth curve
#' @description As above
#' @export
#' 
#' @param n
#' @param nblock
#' @param age_range
#' @param include_predictors
#' @param true_model
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
growth_data_sim <- function(n = 100,
                            nblock = 5,
                            age_range = c(0, 1),
                            include_predictors = TRUE,
                            true_model = 'hillslope') {
  # simulate data
  sim_data <- NULL
  sim_data
}