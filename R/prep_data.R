#' Data Preparation and Formatting for Modelling
#'
#' Create the needed list that gets based to the JAGS
#' backend for modelling. This should be a standard
#' data.frame object that can then be standardized and
#' reshaped into a format appropriate for modeling.
#' Errors will be thrown if the data is not a class,
#' or inherit from, a data.frame or if the specified
#' columns do not exist in the data.frame.
#'
#' @param data data.frame The data to format
#' @param columns vector The columns to target
#' @return List with all the appropriate things needed
#'         for modelling with JAGS
#' @export
#' @examples 
#' data <- data.frame(
#'  list(
#'    "BMI" = rnorm(100, mean = 0, sd = 1),
#'    "fruit" = rnorm(100, mean = 0, sd = 1),
#'    "veg" = rnorm(100, mean = 0, sd = 1),
#'    "tobacco" = rnorm(100, mean = 0, sd = 1)
#'  )
#' )
#' create_modelling_data(data, c("BMI", "fruit", "veg", "tobacco"))
create_modelling_data <- function(data, columns) {
  stopifnot(is.data.frame(data))
  stopifnot(all(columns %in% names(data)))

  subset_data <- data[, columns]
  means <- list()
  sds <- list()
  std_data_full <- list()
  for (col in columns) {
    std_data <- standardize_with_return(subset_data[, col])
    means[[col]] <- std_data$mean_orig
    sds[[col]] <- std_data$sd_orig
    std_data_full[[col]] <- std_data$std_data
  }

  model_data <- data.frame(std_data_full)
  list(
    model_data = model_data,
    n = nrow(model_data),
    p = ncol(model_data),
    sd_orig = data.frame(sds),
    mean_orig = data.frame(means),
    zdf = ncol(model_data),
    zRmat = diag(x = 1, nrow = ncol(model_data))
  )
}
