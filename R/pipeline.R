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
      formula = ,
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
      formula,
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
