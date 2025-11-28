#' Pipeline used for running a model start to finish.
#'
#' This is an unexported function that can run the entire
#' pre-model, attenuation-contamination matrix, and model
#' fitting from start to finish.
#'
#' @param data data.frame The input data
#' @param formula character The formula for the multivariate model
#' @param parameters vector The validity coefficients
#' @param columns vector The columns of the covariates
#' @param stan bool If you would like to run with the Stan backend
#' @param seed numeric The random seed to use
#' @return list The output parameters
#' @export
#'
#' @examples
#' data <- data.frame(
#'  list(
#'    "BMI" = rnorm(100, mean = 0, sd = 1),
#'    "fruit" = rnorm(100, mean = 0, sd = 1),
#'    "veg" = rnorm(100, mean = 0, sd = 1),
#'    "tobacco" = rnorm(100, mean = 0, sd = 1)
#'  )
#' )
#' parameters <- list(
#'   fruit = c(0.3, 0.55, 0.8),
#'   veg = c(0.25, 0.5, 0.75),
#'   tobacco = c(0.4, 0.55, 0.7)
#' )
#' grid <- expand.grid(parameters)
#' param_grid <- list()
#' for (i in seq_len(nrow(grid))) {
#'   name <- paste0("iteration_", i)
#'   param_grid[[name]] <- list(
#'     parameters = grid[i, ]
#'   )
#' }
#' output <- pipeline(
#'    data,
#'    "BMI ~ fruit + veg + tobacco",
#'    as.numeric(param_grid[[i]][["parameters"]]),
#'    c("fruit", "veg", "tobacco")
#' )
#' 
pipeline <- function(
    data,
    formula,
    parameters,
    columns,
    stan = FALSE,
    seed = 42) {
  if (stan) {
    output <- acme_model(data, columns, seed = seed, stan = TRUE)
    lambda <- attenuation_matrix(
      output,
      columns,
      parameters,
      stan = TRUE
    )
    model_output <- multivariate_model(
      formula = formula,
      data = data,
      columns = columns,
      a_c_matrix = lambda$matrix,
      sds = lambda$sds,
      variances = lambda$variances,
      univariate = TRUE,
      seed = seed
    )
  } else {
    output <- acme_model(data, columns, seed = seed)
    lambda <- attenuation_matrix(output, columns, parameters)
    model_output <- multivariate_model(
      formula = formula,
      data = data,
      columns = columns,
      a_c_matrix = lambda$matrix,
      sds = lambda$sds,
      variances = lambda$variances,
      univariate = TRUE,
      seed = seed
    )
  }
  multivariate <- apply(model_output$multivariate, 2, function(x) {
    c(mean = mean(x), sd = sd(x), median = median(x))
  })
  multivariate <- t(multivariate)
  return(list(
    "output" = multivariate,
    "parameters" = parameters
  ))
}
