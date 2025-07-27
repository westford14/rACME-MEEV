---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rAMAMEV

<!-- badges: start -->
<!-- badges: end -->

How do you adjust for multi-variate measurement error in the absence of an internal
validation study? With `rAMAMEV` of course! Based off of the work of Muoka et al., 2020 (1),
this library takes the proposed methodology and exposes it as a clean R package.

## Installation

You can install the development version of rAMAMEV from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("westford14/rAMAMEV")
```

## Example

This is a basic example which shows you how to solve a common problem:


``` r
library(rAMAMEV)
## basic example code
fisher_z_transform(0.30)
#> [1] 0.3095196
```

### Maintainers

* westford14

### Citations

1. Muoka, A. K., Agogo, G. O., Ngesa, O. O., & Mwambi, H. G. (2020b). A Method to adjust for measurement error in multiple exposure variables measured with correlated errors in the absence of an internal validation study. F1000Research, 9, 1486. https://doi.org/10.12688/f1000research.27892.1
