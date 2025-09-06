#' Run a sensitivity analysis on the error adjustment
#'
#' Create a sensitivity analysis based on a grid of validity
#' coefficient parameters. The input of the parameter space
#' should be a list with each key being the name of one of
#' the covariates to vary and a vector of the parameter space
#' to test the sensitivity. Be very careful because all
#' combinations of the parameters will be tested so you
#' can very easily run this for too long. Also please note that
#' the parameters should be be bound between 0 and 1, the
#' theoretical limits of the validity coefficients. An example of
#' this parameter grid is:
#'
#' params <- list(
#'  fruit = c(0.1, 0.2, 0.3),
#'  veg = c(0.1, 0.2, 0.3),
#'  tobacco = c(0.1, 0.2, 0.3)
#' )
#'
#' But, again please note that this will then fit 3 * 3 * 3
#' different models so the run time here can explode. Also
#' parallel computation will be utilized here, but it is
#' much more difficult to debug should errors arise. All this to
#' say is: buyer beware.
#'
#' @param parameters list As described above
#' @param data data.frame The data to use for the sensitivity analysis
#' @param formula character The formula for the model
#' @param columns vector The columns within the data for the covariates
#' @param stan boolean Whether or not to run with the Stan backend
#' @param seed numeric The random seed to set
#' @return list with the means and the SDs of the parameters
#' @export
#' @import parallelly
#' @import snow
#' @import utils
#' @import foreach
#' @import doSNOW
#' @import dplyr
#' @importFrom foreach %dopar%
sensitivity_analysis <- function(
    parameters,
    data,
    formula,
    columns,
    stan = FALSE,
    seed = 42) {
  cores <- as.integer(parallelly::availableCores() * 0.75)
  if (cores < 1) {
    cores <- 1
  }

  grid <- expand.grid(parameters)
  cl <- snow::makeCluster(cores, outfile = "")
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(max = nrow(grid), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  param_grid <- list()
  for (i in seq_len(nrow(grid))) {
    name <- paste0("iteration_", i)
    param_grid[[name]] <- list(
      parameters = grid[i, ]
    )
  }

  all_output <- foreach::foreach(
    i = seq_along(param_grid),
    .combine = c,
    .packages = c("MCMCpack", "rjags", "rstan", "stats", "utils"),
    .options.snow = opts,
    .export = c(
      "multivariate_model",
      "attenuation_matrix",
      "acme_model",
      "create_modelling_data",
      "create_stan_model_string",
      "create_model_string"
    )
  ) %dopar% {
    output <- pipeline(
      data,
      formula,
      as.numeric(param_grid[[i]][["parameters"]]),
      columns,
      stan,
      seed
    )
    return(list(list(
      "output" = output[["output"]],
      "parameters" = output[["parameters"]]
    )))
  }

  snow::stopCluster(cl)

  colnames <- paste(columns, "_param", sep = "")

  mean_frame <- dplyr::bind_rows(
    lapply(
      all_output,
      function(x) {
        as.data.frame(
          t(c(x$output[, "mean"], as.character(x$parameters)))
        )
      }
    )
  )
  colnames(mean_frame) <- c(columns, colnames)

  sd_frame <- dplyr::bind_rows(
    lapply(
      all_output,
      function(x) {
        as.data.frame(
          t(c(x$output[, "sd"], as.character(x$parameters)))
        )
      }
    )
  )
  colnames(sd_frame) <- c(columns, colnames)
  return(list(
    means = data.frame(mean_frame),
    sds = data.frame(sd_frame)
  ))
}
