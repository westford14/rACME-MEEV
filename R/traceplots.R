#' Create Traceplots for the Parameters
#'
#' Create sampler traceplots to run diagnostics on whether or
#' not the sampler converged when creating the posterior. Columns
#' should always be in the order that they appear in the model.
#'
#' @param input list List of the model output
#' @param columns vector The target columns to assess
#' @param pre_model bool If you are passing the pre-model
#' @param stan bool If the model is stan based
#' @return plotted objects
#' @export
#' @importFrom rstan traceplot
#' @importFrom coda traceplot
#' @import tools
#' @import ggplot2
#' @import graphics
traceplots <- function(input, columns, pre_model = FALSE, stan = FALSE) {
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
        plot <- rstan::traceplot(
          input,
          pars = c(target_columns[[i]])
        ) + ggplot2::labs(
          title = paste0(
            "SD for observed ",
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
        print(coda::traceplot(
          input[[1]][, target_columns[i]],
          main = paste0(
            "SD for observed ",
            tools::toTitleCase(columns[i])
          )
        ))
      }
    }
  } else {
    for (col in columns) {
      print(coda::traceplot(
        input[, col],
        main = toTitleCase(col)
      ))
    }
  }
}
