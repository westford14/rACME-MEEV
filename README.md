
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rACME-MEEV

Adjust for Measurement Error in Multiple Exposures from External
Validation

<!-- badges: start -->

<!-- badges: end -->

How do you adjust for multi-variate measurement error in the absence of
an internal validation study? With `rACME-MEEV` of course! Based off of
the work of Muoka et al., 2020 (1), this library takes the proposed
methodology and exposes it as a clean R package.

## Installation

You can install the development version of rACME-MEEV from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("westford14/rACMEMEEV")
```

## Example

This is a basic example running through a standard workflow of adjusting
survey data in the absenece of a validation study.

``` r
library(rACMEMEEV)

# Read the data
data <- read_csv("path/to/your/data")
columns <- c("fruit", "vegetable", "tobacco")

# Generate the coefficients
fruit_v_coef <- generate_coefficient(1000, fruit_low, fruit_upper, 0.95)
veg_v_coef <- generate_coefficient(1000, veg_low, veg_upper, 0.95)
tob_v_coef <- generate_coefficient(1000, tobacco_low, tobacco_upper, 0.95)

# Compute the pre-model
output <- acme_model(data, columns)

# Solve the attenuation-contamination matrix
validity_coefficients <- c(fruit_v_coef, veg_v_coef, tob_v_coef)
lambda <- attenuation_matrix(output, columns, validity_coefficients)

# Fit the multivariate model
model_output <- multivariate_model(
  "target ~ fruit + veg + tobacco",
  data = dataNCD,
  columns = c("fruit", "veg", "tobacco"),
  jags_model = output$model,
  a_c_matrix = lambda$matrix,
  sds = lambda$sds,
  variances =lambda$variances,
  univariate = TRUE
)

# Plot covariate adjustments
plots <- plot_covariates(model_output, columns)

# Create Traceplots Etc.
acf_plots(model_output$naive, c("fruit", "veg", "tobacco"))
traceplots(output$samples, c("fruit", "veg", "tobacco"))
acf_plots(model_output$naive, c("fruit", "veg", "tobacco"), pre_model = True)
traceplots(output$samples, c("fruit", "veg", "tobacco"), pre_model = True)
```

### Stan

The model has been most thoroughly validated with the JAGS backend, but
there is experimental support for the Stan backend. Please do note that
it is experimental and liable to change. Here is an example of the same
above workflow using the Stan backend.

``` r
library(rACMEMEEV)

# Read the data
data <- read_csv("path/to/your/data")
columns <- c("fruit", "vegetable", "tobacco")

# Generate the coefficients
fruit_v_coef <- generate_coefficient(1000, fruit_low, fruit_upper, 0.95)
veg_v_coef <- generate_coefficient(1000, veg_low, veg_upper, 0.95)
tob_v_coef <- generate_coefficient(1000, tobacco_low, tobacco_upper, 0.95)

# Compute the pre-model
output <- acme_model(data, columns, stan = TRUE)

# Solve the attenuation-contamination matrix
validity_coefficients <- c(fruit_v_coef, veg_v_coef, tob_v_coef)
lambda <- attenuation_matrix(output, columns, validity_coefficients, stan = TRUE)

# Fit the multivariate model
model_output <- multivariate_model(
  "target ~ fruit + veg + tobacco",
  data = dataNCD,
  columns = c("fruit", "veg", "tobacco"),
  jags_model = output$model,
  a_c_matrix = lambda$matrix,
  sds = lambda$sds,
  variances =lambda$variances,
  univariate = TRUE
)

# Plot covariate adjustments
plots <- plot_covariates(model_output, columns)

# Create Traceplots Etc.
acf_plots(model_output$naive, c("fruit", "veg", "tobacco"), pre_model = TRUE, stan = TRUE)
traceplots(output$samples, c("fruit", "veg", "tobacco"), pre_model = TRUE, stan = TRUE)
acf_plots(model_output$naive, c("fruit", "veg", "tobacco"), stan = TRUE)
traceplots(output$samples, c("fruit", "veg", "tobacco"), stan = TRUE)
```

### Development

If you would like continue development of this library or open up
feature requests, you can fork the repo then create a descriptive branch
name while continuing devleopment.

``` bash
git checkout -b your/descriptive/branch/name
```

Once development is complete, please open a pull request making sure to
note the fields provided in the [pull request
template](./github/pull_request_template.md).

There are a couple of key commands that you should keep in mind when
developing this library. First, make sure that `devtools` and `knitr`
are installed into your R workspace. Then typically the workflow will
follow as:

- Make changes to the library
- Run `devtools::check()`
- Run `devtools::install()`
- Test the code to make sure it meets your expectations
- Create all necessary new tests
- Run `devtools::test()`
- Run `devtools::document()`
- Make any necessary changes to the [README.md](./README.md)
- Run `knitr::build_readme()`
- Finally run `make lint` from the command line in the root of the
  repository

Sometimes R will have trouble reading the `.db` that has all of the
namespace exports and will throw an error. So, the easiest way to
rectify this is to uninstall and then reinstall the package.

- `detach("package:rACMEMEEV", unload=TRUE)`
- `devtools::install()`
- `library(rACMEMEEV)`

### Build Errors

If you are building this package from source there are a few platform
specific issues that may arise. This package was developed principally
on R version 4.5.1 on an OSX operation system, so
[RTools](https://cran.r-project.org/bin/windows/Rtools/) may need to be
installed on a Windows machine to get proper builds. If there are any
issues running `devtools::check()` that specifically cite RTools, you
can try to set the `options(buildtools.check = function(action) TRUE )`
option so that it skips the explicit local build. This is not
recommended, but is a way to speed up the development loop if there are
build issues arising.

### Maintainers

- westford14

### Citations

1.  Muoka, A. K., Agogo, G. O., Ngesa, O. O., & Mwambi, H. G. (2020b). A
    Method to adjust for measurement error in multiple exposure
    variables measured with correlated errors in the absence of an
    internal validation study. F1000Research, 9, 1486.
    <https://doi.org/10.12688/f1000research.27892.1>
