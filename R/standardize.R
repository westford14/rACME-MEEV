#' Standardize the data of 1-D vector
#'
#' Perform standardization of data on a 2-D dataframe
#' type object. Standardization in this case refers
#' to (x - mean(x)) / sd(x) where X is a 1-dimensional
#' vector.
#'
#' @param data vector The vector to standardize
#' @return List with the original standard deviation, mean,
#'         and the standardized data
#' @export
#'
#' @examples
#' data <- c(1, 2, 3, 4)
#' standardize_with_return(data)
standardize_with_return <- function(data) {
  stopifnot((is.vector(data) & !is.list(data)))
  stopifnot((is.vector(data) & length(data) > 1))
  stopifnot(!is.na(as.numeric(data)))

  mean_orig <- mean(data)
  sd_orig <- sd(data)
  std_data <- (data - mean(data)) / sd(data)
  list(
    std_data = std_data,
    mean_orig = mean_orig,
    sd_orig = sd_orig
  )
}
