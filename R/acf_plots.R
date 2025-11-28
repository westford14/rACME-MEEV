#' Create Auto Correlation Plots for Models
#'
#' Create auto correlation plots for the models to test if there
#' is any high autocorrelation in the sampling space, which would
#' imply that sampler should be adjusted or the data needs to be
#' adjusted.
#'
#' @param input list List of the model output
#' @param columns vector The target columns to assess
#' @param pre_model bool If you are passing the pre-model
#' @param stan bool If you are using the stan model
#' @return plotted objects
#' @export
#' @import stats
#' @import tools
#' @import bayesplot
#' @importFrom ggpubr mean_se_
#'
#' @examples
#' columns <- c("fruit", "veg", "tobacco")
#' fruit_v_coef <- generate_coefficient(100, 0.3, 0.8, 0.95)
#' veg_v_coef <- generate_coefficient(100, 0.25, 0.75, 0.95)
#' tob_v_coef <- generate_coefficient(100, 0.4, 0.7, 0.95)
#' validity_coefficients <- c(fruit_v_coef, veg_v_coef, tob_v_coef)
#' data <- data.frame(
#'  list(
#'    "BMI" = rnorm(100, mean = 0, sd = 1),
#'    "fruit" = rnorm(100, mean = 0, sd = 1),
#'    "veg" = rnorm(100, mean = 0, sd = 1),
#'    "tobacco" = rnorm(100, mean = 0, sd = 1)
#'  )
#' )
#' output <- acme_model(data, columns)
#' lambda <- attenuation_matrix(
#'   output,
#'   columns,
#'   validity_coefficients,
#' )
#' model_output <- multivariate_model(
#'   "BMI ~ fruit + veg + tobacco",
#'   data = data,
#'   columns = columns,
#'   a_c_matrix = lambda$matrix,
#'   sds = lambda$sds,
#'   variances = lambda$variances,
#'   univariate = TRUE
#' )
#' acf_plots(model_output$naive, columns)
acf_plots <- function(input, columns, pre_model = FALSE, stan = FALSE) {
  if (pre_model) {
    if (stan) {
      model_columns <- names(input)
      addition <- "Stan"
    } else {
      model_columns <- colnames(input[[1]])
      addition <- "JAGS"
    }
    target_columns <- model_columns[grepl("sigma", model_columns)]

    plots <- list()
    for (i in seq_along(target_columns)) {
      plot <- bayesplot::mcmc_acf_bar(
        input,
        pars = target_columns[[i]]
      ) + ggplot2::labs(
        title = paste0(
          "ACF for SD for observed ",
          tools::toTitleCase(columns[i]),
          " (",
          addition,
          ")"
        )
      )
      plots[[columns[i]]] <- plot
    }

    rows <- length(columns)
    cols <- 1
    return(plot_a_list(plots, rows, cols))
  } else {
    plots <- list()
    for (col in columns) {
      plot <- bayesplot::mcmc_acf_bar(
        input,
        pars = col
      ) + ggplot2::labs(
        title = paste0(
          "ACF for Naive Regression ",
          tools::toTitleCase(col)
        )
      )
      plots[[col]] <- plot
    }

    rows <- length(columns)
    cols <- 1
    return(plot_a_list(plots, rows, cols))
  }
}
