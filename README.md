
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minter <img src="man/figures/minter_logo.png" align="right" alt="" width="130" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/fdecunta/minter/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fdecunta/minter/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/fdecunta/minter/graph/badge.svg)](https://app.codecov.io/gh/fdecunta/minter)
<!-- badges: end -->

A collection of functions for computing effect sizes of interactions for
meta-analysis.

⚠️ **This package is under active development and not yet stable. Use
with caution.**

## Installation

You can install the development version of minter from
[GitHub](https://github.com/fdecunta/minter):

``` r
devtools::install_github("fdecunta/minter", force = TRUE)
```

## Example

Calculate the standardized mean difference (SMD) for the interaction
between fertilization and CO2 enrichment:

``` r
library(minter)

# Create sample data for a 2x2 factorial experiment
# Control: No fertilization, ambient CO2
# Fertilization: Added nutrients
# CO2: Elevated CO2
# Fert_CO2: Both treatments combined

experiment_data <- data.frame(
  Ctrl_mean = 10.2, Ctrl_sd = 2.1, Ctrl_n = 25,  # Control: No treatment
  Fert_mean = 15.7, Fert_sd = 3.2, Fert_n = 25,  # Fertilization only
  CO2_mean = 12.4, CO2_sd = 2.5, CO2_n = 24,    # Elevated CO2 only
  Fert_CO2_mean = 18.9, Fert_CO2_sd = 3.8, Fert_CO2_n = 26  # Both treatments
)

# Calculate SMD for interaction
result <- SMD_inter(
  data = experiment_data,
  col_names = c("SMD", "SMDv")
  Ctrl_mean = Ctrl_mean,
  Ctrl_sd = Ctrl_sd,
  Ctrl_n = Ctrl_n,
  A_mean = Fert_mean,
  A_sd = Fert_sd,
  A_n = Fert_n,
  B_mean = CO2_mean,
  B_sd = CO2_sd,
  B_n = CO2_n,
  AB_mean = Fert_CO2_mean,
  AB_sd = Fert_CO2_sd,
  AB_n = Fert_CO2_n
)

# View results
result$inter_SMD


# Model
res <- rma.mv(SMD, SMDv, ..., data = experiment_data)
```
