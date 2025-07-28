
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minter <img src="man/figures/minter_logo.png" align="right" width="120"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/fdecunta/minter/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fdecunta/minter/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/fdecunta/minter/graph/badge.svg)](https://app.codecov.io/gh/fdecunta/minter)

<!-- badges: end -->

Factorial designs help us understand synergies and antagonisms between
ecological factors. However, meta-analyses of factorial experiments are
rare in ecology, likely because extracting effect sizes from factorial
data is not straightforward.

`minter` is an R package that simplifies this process by providing
functions to extract different effect sizes from factorial data,
enabling researchers to conduct meta-analyses of interactions between
factors.

## Installation

``` r
devtools::install_github("fdecunta/minter")
```

## Example: Fertilization × Drought Interactions

``` r
library(minter)

# Dummy data from 8 studies examining fertilization × drought on plant biomass
studies <- data.frame(
  study_id = 1:8,
  # Control: No fertilization, well-watered
  Ctrl_mean = c(12.3, 15.7, 10.8, 14.2, 11.9, 13.5, 16.1, 12.7),
  Ctrl_sd   = c(2.1, 3.2, 1.8, 2.7, 2.3, 2.9, 3.5, 2.4),
  Ctrl_n    = c(20, 24, 18, 22, 19, 25, 23, 21),
  # Fertilization only
  Fert_mean = c(18.5, 21.3, 16.2, 19.8, 17.1, 20.4, 22.7, 18.9),
  Fert_sd   = c(3.1, 4.1, 2.7, 3.6, 3.2, 3.8, 4.2, 3.4),
  Fert_n    = c(22, 25, 20, 24, 21, 26, 25, 23),
  # Drought only  
  Drought_mean = c(8.7, 11.2, 7.9, 10.1, 8.3, 9.8, 11.7, 9.4),
  Drought_sd   = c(1.8, 2.4, 1.6, 2.1, 1.9, 2.3, 2.6, 2.0),
  Drought_n    = c(19, 23, 17, 21, 18, 24, 22, 20),
  # Both treatments
  Both_mean = c(14.2, 17.8, 12.9, 16.3, 13.7, 16.1, 18.4, 15.2),
  Both_sd   = c(2.9, 3.7, 2.5, 3.3, 2.8, 3.4, 3.9, 3.1),
  Both_n    = c(21, 26, 19, 23, 20, 27, 24, 22)
)

# Calculate interaction effect: Does fertilization work differently under drought?
interaction_results <- lnRR_inter(
  data = studies,
  Ctrl_mean = "Ctrl_mean", Ctrl_sd = "Ctrl_sd", Ctrl_n = "Ctrl_n",
  A_mean = "Fert_mean", A_sd = "Fert_sd", A_n = "Fert_n", 
  B_mean = "Drought_mean", B_sd = "Drought_sd", B_n = "Drought_n",
  AB_mean = "Both_mean", AB_sd = "Both_sd", AB_n = "Both_n"
)

head(interaction_results)
#>   study_id Ctrl_mean Ctrl_sd ... yi        vi
#> 1        1      12.3     2.1     0.081  0.0069
#> 2        2      15.7     3.2     0.158  0.0068
#> 3        3      10.8     1.8     0.084  0.0073

# Meta-analysis
library(metafor)
res <- rma(yi, vi, ..., data = interaction_results)
```

## Effect Size Measures

-   **lnRR**: Log Response Ratio - proportional effects
-   **SMD**: Standardized Mean Difference - standardized effects  
-   **lnVR**: Log Variability Ratio - effects on variability
-   **lnCVR**: Log Coefficient of Variation Ratio - effects on relative
    variability

## Effect Types

-   **Individual**: Simple treatment vs. control
-   **Main**: Overall effect across levels of another factor
-   **Interaction**: Does one factor’s effect depend on another?
-   **Time**: Treatment × time interactions for repeated measures

## Acknowledgments

We thank Shinichi Nakagawa and Daniel Noble for generously sharing their
unpublished formulas for meta-analysis of interactions, which form the
theoretical foundation of this package.
