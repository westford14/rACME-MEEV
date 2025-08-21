#' Create Attenuation Contamination Matrix
#'
#' From the output JAGS model, create the needed attenuation
#' contamination matrix for further analysis.
#'
#' @param model_output list List of the mcmc summary and covariance matrix
#' @param columns vector Vector of the column names that are being assessed
#' @param validity_coefficients vector Vector of the validity coefficients
#' @return attenuation-contamination matrix
#' @import gdata
#' @export
attenuation_matrix <- function(model_output, columns, validity_coefficients) {
  stopifnot(is.list(model_output))
  stopifnot(length(columns) == length(validity_coefficients))

  cov_matrix <- model_output$covariance_matrix$statistics
  combinations <- combn(seq_along(columns), 2)

  sds <- list()
  counter <- 1
  for (row in rownames(cov_matrix)) {
    if (grepl("sigma", row, fixed = TRUE)) {
      sds[[columns[counter]]] <- cov_matrix[row, "Mean"]
      counter <- counter + 1
    }
  }

  correlations <- list()
  covariances <- list()
  variances <- list()
  for (column in seq_len(ncol(combinations))) {
    first <- combinations[, column][1]
    second <- combinations[, column][2]

    col_name <- paste0(
      "rho[",
      first,
      ",",
      second,
      "]"
    )
    rho <- cov_matrix[col_name, "Mean"]
    correlations[[columns[column]]] <- rho

    name <- paste0("cov-", columns[first], "-", columns[second])
    covar <- sds[[columns[first]]] * sds[[columns[second]]] * rho
    covariances[[name]] <- covar
    variances[[columns[column]]] <- (
      (validity_coefficients[column])^2 * (sds[[column]])^2
    )
  }

  covariances_between <- list()
  for (column in seq_len(ncol(combinations))) {
    first <- combinations[, column][1]
    second <- combinations[, column][2]

    name <- paste0("cov-", columns[first], "-", columns[second])
    first_name <- columns[first]
    second_name <- columns[second]
    cov <- (
      covariances[[column]] - correlations[[column]] * sds[[first_name]]
        * sds[[second_name]]
        * sqrt(
          (1 - (validity_coefficients[first])^2)
          * (1 - (validity_coefficients[second])^2)
        )
    )

    covariances_between[[name]] <- cov
  }

  t_mat <- matrix(
    data = NA,
    nrow = length(columns),
    ncol = length(columns),
    dim = list(columns, columns)
  )
  diag(t_mat) <- unlist(variances, use.names = FALSE)
  t_mat[upper.tri(t_mat)] <- unlist(covariances_between, use.names = FALSE)
  t_mat <- t(t_mat)
  t_mat[upper.tri(t_mat)] <- unlist(covariances_between, use.names = FALSE)

  q_mat <- matrix(
    data = NA,
    nrow = length(columns),
    ncol = length(columns),
    dim = list(columns, columns)
  )
  diag(q_mat) <- unlist(sds, use.names = FALSE)^2
  q_mat[upper.tri(q_mat)] <- unlist(covariances, use.names = FALSE)
  q_mat <- t(q_mat)
  q_mat[upper.tri(q_mat)] <- unlist(covariances, use.names = FALSE)

  lambda <- t_mat %*% solve(q_mat)
  lambda
}
