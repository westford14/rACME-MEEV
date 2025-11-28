#' Create Attenuation Contamination Matrix
#'
#' From the output JAGS model, create the needed attenuation
#' contamination matrix for further analysis. Please be aware that
#' this is multivariate so a minimum of 3 columns is needed.
#'
#' @param model_output list List of the mcmc summary and covariance matrix
#' @param columns vector Vector of the column names that are being assessed
#' @param validity_coefficients vector Vector of the validity coefficients
#' @param stan boolean If you are passing in a Stan backend pre-model
#' @return List with the attenuation-contamination matrix and the standard
#'              deviations
#' @export
#' @import utils
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
#' attenuation_matrix(
#'   output,
#'   columns,
#'   validity_coefficients,
#' )
attenuation_matrix <- function(
    model_output,
    columns,
    validity_coefficients,
    stan = FALSE) {
  stopifnot(is.list(model_output))
  stopifnot(length(columns) == length(validity_coefficients))
  stopifnot(length(columns) > 2)

  if (stan) {
    cov_matrix <- model_output$covariance_matrix$summary
  } else {
    cov_matrix <- model_output$covariance_matrix$statistics
  }

  combinations <- utils::combn(seq_along(columns), 2)

  sds <- list()
  counter <- 1
  for (row in rownames(cov_matrix)) {
    if (grepl("sigma", row, fixed = TRUE)) {
      if (stan) {
        sds[[columns[counter]]] <- cov_matrix[row, "mean"]
      } else {
        sds[[columns[counter]]] <- cov_matrix[row, "Mean"]
      }
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
    if (stan) {
      rho <- cov_matrix[col_name, "mean"]
    } else {
      rho <- cov_matrix[col_name, "Mean"]
    }
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
    dimnames = list(columns, columns)
  )
  diag(t_mat) <- unlist(variances, use.names = FALSE)
  t_mat[upper.tri(t_mat)] <- unlist(covariances_between, use.names = FALSE)
  t_mat <- t(t_mat)
  t_mat[upper.tri(t_mat)] <- unlist(covariances_between, use.names = FALSE)

  q_mat <- matrix(
    data = NA,
    nrow = length(columns),
    ncol = length(columns),
    dimnames = list(columns, columns)
  )
  diag(q_mat) <- unlist(sds, use.names = FALSE)^2
  q_mat[upper.tri(q_mat)] <- unlist(covariances, use.names = FALSE)
  q_mat <- t(q_mat)
  q_mat[upper.tri(q_mat)] <- unlist(covariances, use.names = FALSE)

  lambda <- t_mat %*% solve(q_mat)
  list(
    matrix = lambda,
    sds = sds,
    variances = variances
  )
}
