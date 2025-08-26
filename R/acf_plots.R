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
#' @import rstan
#' @import graphics
#' @importFrom ggpubr mean_se_
acf_plots <- function(input, columns, pre_model = FALSE, stan = FALSE) {
  if (length(columns) == 1) {
    graphics::par(mfrow = c(1, 1))
  } else if (length(columns) <= 4) {
    graphics::par(mfrow = c(2, 2))
  } else {
    graphics::par(mfrow = c(ceiling(length(columns) / 2), 2))
  }
  if (pre_model) {
    if (stan) {
      model_columns <- names(input)
      target_columns <- model_columns[grepl("sigma", model_columns)]

      plots <- list()
      for (i in seq_along(target_columns)) {
        plot <- rstan::stan_ac(
          input,
          pars = target_columns[[i]]
        ) + ggplot2::labs(
          title = paste0(
            "ACF for SD for observed ",
            tools::toTitleCase(columns[i])
          )
        )
        plots[[columns[i]]] <- plot
      }

      rows <- length(columns)
      cols <- 1
      return(plot_a_list(plots, rows, cols))
    } else {
      model_columns <- colnames(input[[1]])
      target_columns <- model_columns[grepl("sigma", model_columns)]
      for (i in seq_along(target_columns)) {
        print(stats::acf(
          input[[1]][, target_columns[i]],
          main = paste0(
            "ACF for SD for observed ",
            tools::toTitleCase(columns[i])
          )
        ))
      }
    }
  } else {
    for (col in columns) {
      print(stats::acf(
        input[, col],
        main = paste0(
          "ACF for Naive Regression: ",
          tools::toTitleCase(col)
        )
      ))
    }
  }
}
