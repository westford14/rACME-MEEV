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
#' @import tools
#' @import ggplot2
#' @import bayesplot
traceplots <- function(input, columns, pre_model = FALSE, stan = FALSE) {
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
      plot <- bayesplot::mcmc_trace(
        input,
        pars = target_columns[[i]]
      ) + ggplot2::labs(
        title = paste0(
          "SD for Observed ",
          tools::toTitleCase(columns[[i]]),
          " (",
          addition,
          ")"
        )
      )
      plots[[target_columns[i]]] <- plot
    }
    rows <- length(target_columns)
    cols <- 1
    return(plot_a_list(plots, rows, cols))
  } else {
    plots <- list()
    for (col in columns) {
      plot <- bayesplot::mcmc_trace(
        input,
        pars = col
      ) + ggplot2::labs(title = paste0("Traceplot ", tools::toTitleCase(col)))
      plots[[col]] <- plot
    }
    rows <- length(columns)
    cols <- 1
    return(plot_a_list(plots, rows, cols))
  }
}
