#' Plotting code for the simulation results
#'
#' Creates density plots of the means and SD of the results
#' from the simulation study. These will be facet grid
#' ggplot2 objects.
#'
#' @param data list(data.frame) The output from the simulation
#' @return list The plot objects
#' @export
#' @import ggplot2
#' @import reshape2
plot_simulation <- function(data) {
  means <- reshape2::melt(data[["means"]])
  sds <- reshape2::melt(data[["sds"]])

  mt_m <- ggplot(means, aes(x = value, color = variable)) +
    geom_density()
  fg_m <- mt_m + facet_grid(vars(means[["variable"]]), scales = "free")

  mt_sd <- ggplot(sds, aes(x = value, color = variable)) +
    geom_density()
  fg_sd <- mt_sd + facet_grid(vars(sds[["variable"]]), scales = "free")

  list(
    means = fg_m,
    sds = fg_sd
  )
}
