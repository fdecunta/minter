# Interaction Effect: Log Coefficient of Variation Ratio

Computes the interaction effect between Factors A and B in factorial
experiments on the coefficient of variation ratio.

## Usage

``` r
lnCVR_inter(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
  A_sd,
  A_n,
  B_mean,
  B_sd,
  B_n,
  AB_mean,
  AB_sd,
  AB_n
)
```

## Arguments

- data:

  Data frame containing the variables used.

- col_names:

  Vector of two strings to name the output columns for the effect size
  and its sampling variance. Default is 'yi' and 'vi'.

- append:

  Logical. Append the results to `data`. Default is TRUE

- Ctrl_mean:

  Mean outcome from the Control treatment

- Ctrl_sd:

  Standard deviation from the control treatment

- Ctrl_n:

  Sample size from the control treatment

- A_mean:

  Mean outcome from the treatment

- A_sd:

  Standard deviation from the treatment

- A_n:

  Sample size from the treatment

- B_mean:

  Mean outcome from the B treatment

- B_sd:

  Standard deviation from the B treatment

- B_n:

  Sample size from the B treatment

- AB_mean:

  Mean outcome from the interaction AxB treatment

- AB_sd:

  Standard deviation from the interaction AxB treatment

- AB_n:

  Sample size from the interaction AxB treatment

## Value

A data frame containing the effect sizes and their sampling variance. By
default, the columns are named `yi` (effect size) and `vi` (sampling
variance). If `append = TRUE`, the results are appended to the input
`data`; otherwise, only the computed effect size columns are returned.

## Details

\$\$lnCVR\_{inter} = \ln\left( \frac{CV\_{AB} / CV\_{B}}{CV\_{A} /
CV\_{Ctrl}} \right) + \frac{1}{2} \left( \frac{1}{2(n\_{AB} - 1)} -
\frac{1}{2(n\_{A} - 1)} + \frac{1}{2(n\_{B} - 1)} -
\frac{1}{2(n\_{Ctrl} - 1)} \right)\$\$

\$\$var(lnCVR\_{inter}) = var(\ln RR\_{inter}) + var(\ln
VR\_{inter})\$\$

This follows the assumption of no correlation between mean and variance.

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
# Interaction effect logCVR (Light x Nutrients)
data <- data.frame(
  study_id = 1:2,
  control_mean = c(7.3, 8.9),
  control_sd = c(1.4, 1.7),
  control_n = c(20, 18),
  light_mean = c(12.8, 14.2),
  light_sd = c(3.1, 3.5),
  light_n = c(19, 20),
  nutrients_mean = c(9.6, 11.1),
  nutrients_sd = c(1.9, 2.2),
  nutrients_n = c(21, 17),
  light_nutrients_mean = c(18.4, 20.7),
  light_nutrients_sd = c(4.8, 5.3),
  light_nutrients_n = c(18, 19)
)

result <- lnCVR_inter(
  data = data,
  Ctrl_mean = "control_mean",
  Ctrl_sd = "control_sd",
  Ctrl_n = "control_n",
  A_mean = "light_mean",
  A_sd = "light_sd",
  A_n = "light_n",
  B_mean = "nutrients_mean",
  B_sd = "nutrients_sd",
  B_n = "nutrients_n",
  AB_mean = "light_nutrients_mean",
  AB_sd = "light_nutrients_sd",
  AB_n = "light_nutrients_n"
)
```
