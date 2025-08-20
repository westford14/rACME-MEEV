#' Define a Model that is JAGS Usable
#'
#' Create the model string, save it to a temporary folder,
#' and return back the location of the temporary model
#' file for usage by JAGS later.
#'
#' @return Full path to the model specification
#' @export
#'
#' @examples
#' create_model_string()
create_model_string <- function() {
  model_string <- "
  model {
    for (i in 1:n) {
      zx[i, 1:p] ~ dmnorm( zMu[1:p] , zInvCovMat[1:p, 1:p] )
    }

    for (varIdx in 1:p) {
      # -- uninformative prior for zMu -- #
      zMu[varIdx] ~ dnorm(0, 1000000)
    }
    zInvCovMat ~ dwish(zRmat[1:p, 1:p], zdf)

    # -- Convert invCovMat to SD and correlation -- #
    zCovMat <- inverse(zInvCovMat)
    for (varIdx in 1:p) {
      zSigma[varIdx] <- sqrt(zCovMat[varIdx,varIdx])
    }
    for (varIdx1 in 1:p) {
      for (varIdx2 in 1:p) {
        zRho[varIdx1, varIdx2] <- (
          zCovMat[varIdx1, varIdx2] / (zSigma[varIdx1] * zSigma[varIdx2])
        )
      }
    }

    for (varIdx in 1:p) {
      sigma[varIdx] <- zSigma[varIdx] * sdOrig[varIdx]
      mu[varIdx] <- zMu[varIdx] * sdOrig[varIdx] + meanOrig[varIdx]
    }
    for (varIdx1 in 1:p) {
      for (varIdx2 in 1:p) {
        rho[varIdx1, varIdx2] <- zRho[varIdx1, varIdx2]
      }
    }
  }
  "
  time <- as.integer(as.POSIXct(Sys.time()))
  filename <- paste0(tempdir(), "/", "jags_model_", time, ".txt")
  writeLines(model_string, con = filename)
  filename
}
