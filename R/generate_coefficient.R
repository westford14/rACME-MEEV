#' Generate updated validity coefficient using Fisher Z Transformation
#'
#' Using the Fisher Z Transformation
#' (https://en.wikipedia.org/wiki/Fisher_transformation), generate new
#' validity coefficients based on proposed upper and lower boundaries.
#' This new validity coefficient is based on transformed upper and lower
#' boundaries as well as a fitted normal distribution to the se new
#' proposed upper and lower boundaries.
#'
#' @param n the samples for the normal distribution
#' @param lower_bound numeric the lower boundary of the validity coefficient
#' @param upper_bound numeric the upper boudnary of the validity coefficient
#' @param interval numeric the confidence interval to use (default 0.95)
#' @return Validity coefficient
#' @export
#'
#' @examples
#' n <- 1000
#' coef_upper <- 0.9
#' coef_lower <- 0.3
#' ci <- 0.95
#' generate_coefficient(n, coef_lower, coef_upper, ci)
generate_coefficient <- function(
  n,
  lower_bound,
  upper_bound,
  interval = 0.95
) {
  stopifnot(lower_bound < upper_bound)
  stopifnot(interval > 0 && interval < 1)
  stopifnot(lower_bound > 0 && lower_bound < 1)
  stopifnot(upper_bound > 0 && upper_bound < 1)
  stopifnot(n > 0)

  normal_dist <- normalize_coefficients(n, lower_bound, upper_bound, interval)
  rho <- (exp(2 * mean(normal_dist)) - 1) / (exp(2 * mean(normal_dist)) + 1)
  rho
}