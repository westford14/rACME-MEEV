#' Define the Pre-Model Using Stan
#'
#' Create the model string, save it to a temporary folder,
#' and return back the location of the temporary model
#' file for usage by Stan later. This is experimental and has
#' been validated, but caution should be used when utilizing
#' the Stan backend as the pre-model.
#'
#' @return Full path to the model specification
#' @export
#'
#' @examples
#' create_stan_model_string()
create_stan_model_string <- function() {
  model_string <- "
  data {
    int<lower=1> n;
    int<lower=1> p;
    matrix[n, p] model_data;
    matrix[p, p] zRmat;
    real<lower=0> zdf;
    vector[p] sd_orig;
    vector[p] mean_orig;
  }

  parameters {
    vector[p] zMu;
    cov_matrix[p] zInvCovMat;
  }

  transformed parameters {
    matrix[p, p] zCovMat;
    vector[p] zSigma;
    matrix[p, p] zRho;

    // -- Convert invCovMat to SD and correlation --
    zCovMat = inv(zInvCovMat);
    for (varIdx in 1:p) {
      zSigma[varIdx] = sqrt(zCovMat[varIdx, varIdx]);
    }

    for (varIdx1 in 1:p) {
      for (varIdx2 in 1:p) {
        zRho[varIdx1, varIdx2] = zCovMat[varIdx1, varIdx2] / (zSigma[varIdx1] * zSigma[varIdx2]);
      }
    }
  }

  model {
    for (i in 1:n) {
      model_data[i, 1:p] ~ multi_normal(zMu[1:p], zInvCovMat[1:p, 1:p]);
    }

    // --  uninformative prior for zMu --
    for (i in 1:p) {
      zMu[i] ~ normal(0, 1000000);
    }
    zInvCovMat ~ wishart(zdf, zRmat[1:p, 1:p]);
  }

  generated quantities {
    vector[p] mu;
    vector[p] sigma;
    matrix[p, p] rho;

    // -- transform sd and means back to original scale --
    for (varIdx in 1:p) {
      sigma[varIdx] = zSigma[varIdx] * sd_orig[varIdx];
      mu[varIdx] = zMu[varIdx] * sd_orig[varIdx] + mean_orig[varIdx];
    }

    rho = zRho;
  }
  "
  time <- as.integer(as.POSIXct(Sys.time()))
  filename <- paste0(tempdir(), "/", "stan_model_", time, ".stan")
  writeLines(model_string, con = filename)
  filename
}
