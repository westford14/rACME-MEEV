#' Run a simulations of the measurement error model
#'
#' Create a simulation analysis based on a set of parameters
#' with varied data. The input to this function will be a dataframe
#' and a set of parameters that will then be used to generate
#' synthetic forms of the data. You can also specify how many
#' different "draws" of the data you want to pull from. Parallel
#' computing will be used, so again be judicious with how many
#' draws you want to pull.
#'
#' As with the sensitivity analysis, be very careful when using
#' this function as parallel computing is expensive and difficult
#' to debug. So, ultimately, buyer beware.
#'
#' @param parameters vector The validity parameters to use
#' @param data data.frame The data to use for the simulation analysis
#' @param target character The output data column to use
#' @param formula character The formula for the model
#' @param columns vector The columns within the data for the covariates
#' @param stan boolean Whether or not to run with the Stan backend
#' @param seed numeric The random seed to set
#' @param draws numeric The number of simulations to run
#' @return list with the means and the SDs of the parameters
#' @export
#' @import parallelly
#' @import snow
#' @import utils
#' @import foreach
#' @import doSNOW
#' @import dplyr
#' @import stats
#' @import fitdistrplus
#' @importFrom foreach %dopar%
simulation_study <- function(
    parameters,
    data,
    target,
    formula,
    columns,
    stan = FALSE,
    seed = 42,
    draws = 20) {
  cores <- as.integer(parallelly::availableCores() * 0.75)
  if (cores < 1) {
    cores <- 1
  }

  dists <- list()
  for (col in columns) {
    fit_g <- fitdistrplus::fitdist(data[[col]], "gamma")
    shape <- fit_g[["estimate"]][["shape"]]
    rate <- fit_g[["estimate"]][["rate"]]
    dists[[col]] <- list(shape = shape, rate = rate)
  }
  fit_n <- fitdistrplus::fitdist(data[[target]], "lnorm")
  dists[[target]] <- list(
    meanlog = fit_n[["estimate"]][["meanlog"]],
    sdlog = fit_n[["estimate"]][["sdlog"]]
  )

  cl <- snow::makeCluster(cores, outfile = "")
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(max = draws, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  total_data <- nrow(data)
  frame_grid <- list()
  temp_seed <- 100
  for (i in 1:draws) {
    name <- paste0("iteration_", i)

    column_data <- list()
    for (col in columns) {
      dist <- dists[[col]]
      set.seed(temp_seed + i)
      column_data[[col]] <- stats::rgamma(
        total_data,
        shape = dist[["shape"]],
        rate = dist[["rate"]]
      )
    }

    sample <- data.frame(column_data)
    set.seed(temp_seed + i)
    sample[[target]] <- stats::rlnorm(
      total_data,
      meanlog = dists[[target]][["meanlog"]],
      sdlog = dists[[target]][["sdlog"]]
    )

    frame_grid[[name]] <- list(
      data = sample
    )
  }

  all_output <- foreach::foreach(
    i = seq_along(frame_grid),
    .combine = c,
    .packages = c("MCMCpack", "rjags", "rstan", "stats", "utils"),
    .options.snow = opts,
    .export = c(
      "multivariate_model",
      "attenuation_matrix",
      "acme_model",
      "create_modelling_data",
      "create_stan_model_string",
      "create_model_string",
      "pipeline"
    )
  ) %dopar% {
    output <- pipeline(
      frame_grid[[i]][["data"]],
      formula,
      parameters,
      columns,
      stan,
      seed
    )
    return(list(list(
      "output" = output[["output"]]
    )))
  }

  snow::stopCluster(cl)

  mean_frame <- data.frame(dplyr::bind_rows(
    lapply(
      all_output,
      function(x) x$output[, "mean"]
    )
  ))

  sd_frame <- data.frame(dplyr::bind_rows(
    lapply(
      all_output,
      function(x) x$output[, "sd"]
    )
  ))
  return(list(
    means = mean_frame,
    sds = sd_frame
  ))
}
