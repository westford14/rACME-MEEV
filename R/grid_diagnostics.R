#' Custom Function to Use Patchwork
#'
#' Uses patchwork to make gridding easier for plotting the
#' traceplots and acf plots for the Stan pre-model.
#'
#' @param plot_list list List of plots
#' @param rows numeric Number of rows
#' @param columns numeric Number of columns
#'
#' @import patchwork
plot_a_list <- function(
    plot_list,
    rows,
    columns) {
  patchwork::wrap_plots(
    plot_list,
    nrow = rows,
    ncol = columns
  )
}
