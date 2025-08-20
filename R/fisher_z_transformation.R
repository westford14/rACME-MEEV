#' Perform the Fisher Z Transformation
#'
#' Take a validity coefficient and perform the
#' Fisher Z Transformation (https://en.wikipedia.org/wiki/Fisher_transformation)
#' to approximate a normal distribution
#'
#' @param coefficient numeric The validity coefficient to transform.
#' @return Transformed validity coefficient
#' @export
#'
#' @examples
#' coef <- 0.3
#' fisher_z_transform(coef)
fisher_z_transform <- function(coefficient) {
  stopifnot(is.numeric(coefficient))
  stopifnot(coefficient < 1)
  stopifnot(coefficient > -1)

  z_coef <- 0.5 * (log(1 + coefficient) - log(1 - coefficient))
  z_coef
}
