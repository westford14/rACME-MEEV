#' Diagnostics for Models
#'
#' Using the previously fit models, add the diagnostics for
#' assessing the adjusted covariates based on the naive,
#' univariate, and multivariate models.
#'
#' @param model_output list List of the model output
#' @param columns vector The target columns to assess
#' @return plotted objects
#' @export
#' @import ggplot2
#' @import reshape2
#' @import tools
#' @import stats
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr sym
#' @importFrom dplyr %>%
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
#' plot_covariates(model_output, columns)
plot_covariates <- function(model_output, columns) {
  plots <- list()
  for (col in columns) {
    columns <- list()
    temp <- model_output$multivariate[, col]
    columns[["multivariate"]] <- temp

    temp <- model_output$naive[, col]
    columns[["naive"]] <- temp

    new_data <- data.frame(do.call("cbind", columns))
    dens <- stats::density(new_data$multivariate)
    plot_data <- reshape2::melt(new_data)
    mu <- plot_data %>%
      dplyr::group_by(!!dplyr::sym("variable")) %>%
      dplyr::summarise(grp.mean = mean(!!dplyr::sym("value"))
    )

    plot <- (
      ggplot2::ggplot(plot_data, aes_string(x = "value", color = "variable"))
      +
        ggplot2::geom_density()
        +
        ggplot2::geom_vline(
          mu,
          mapping = aes_string(xintercept = "grp.mean", color = "variable"),
          linetype = "dashed"
        )
        +
        ggplot2::labs(
          title = paste(tools::toTitleCase(col), "Consumption"),
          x = paste("N =", nrow(new_data), "Bandwidth =", round(dens[["bw"]], 5)),
          y = "Density",
          color = "Method\n"
        )
        +
        ggplot2::scale_color_manual(
          labels = c(
            bquote(hat(beta)[bold(X)[.(col)]]),
            bquote(hat(beta)[bold(W)[.(col)]])
          ),
          values = c("blue", "red")
        )
    )
    plots[[col]] <- plot
  }
  plots
}
