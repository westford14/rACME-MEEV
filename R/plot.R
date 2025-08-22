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
#' @import plyr
#' @import tools
plot_covariates <- function(model_output, columns) {
  plots <- list()
  for (col in columns) {
    columns <- list()
    temp <- model_output$multivariate[, col]
    columns[["multivariate"]] <- temp

    temp <- model_output$naive[, col]
    columns[["naive"]] <- temp

    new_data <- data.frame(do.call("cbind", columns))
    dens <- density(new_data$multivariate)
    plot_data <- melt(new_data)
    mu <- ddply(plot_data, "variable", summarise, grp.mean = mean(value))

    plot <- (
      ggplot(plot_data, aes(x = value, color = variable))
      +
        geom_density()
        +
        geom_vline(
          mu,
          mapping = aes(xintercept = grp.mean, color = variable),
          linetype = "dashed"
        )
        +
        labs(
          title = paste(toTitleCase(col), "Consumption"),
          x = paste("N =", nrow(new_data), "Bandwidth =", round(dens[["bw"]], 5)),
          y = "Density",
          color = "Method\n"
        )
        +
        scale_color_manual(
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
