#' Normalize the upper and lower bounds of a validity coefficient
#'
#' Using the Fisher Z Transformation, normalize the upper and lower
#' boundary of a validity coefficient to generate a Normal distribution
#' based on the mean and standard deviation. The default behavior is
#' to assume a 95% CI, but this can also be specified via the `interval`
#' parameter if 99% or 90% CI is preferred.
#'
#' @param n the samples for the normal distribution
#' @param lower_bound numeric the lower boundary of the validity coefficient
#' @param upper_bound numeric the upper boudnary of the validity coefficient
#' @param interval numeric the confidence interval to use (default 0.95)
#' @return Normal distribution
#' @export
#'
#' @examples
#' n <- 1000
#' coef_upper <- 0.9
#' coef_lower <- 0.3
#' ci <- 0.95
#' normalize_coefficients(n, coef_lower, coef_upper, ci)
normalize_coefficients <- function(
    n,
    lower_bound,
    upper_bound,
    interval = 0.95) {
  stopifnot(lower_bound < upper_bound)
  stopifnot(interval > 0 && interval < 1)
  stopifnot(lower_bound > 0 && lower_bound < 1)
  stopifnot(upper_bound > 0 && upper_bound < 1)
  stopifnot(n > 0)

  cv <- qnorm((1 - interval) / 2, lower.tail = FALSE)
  z_lower <- fisher_z_transform(lower_bound)
  z_upper <- fisher_z_transform(upper_bound)
  mu <- 0.5 * (z_lower + z_upper)
  sd <- (0.5 * (z_upper - z_lower)) / cv
  rnorm(n, mu, sd)
}
