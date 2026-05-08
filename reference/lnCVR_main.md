# Main Effect: Log Coefficient Of Variation Ration

Computes the main effect of Factor A across levels of Factor B in
factorial experiments on the coefficient of variation.

## Usage

``` r
lnCVR_main(
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

\$\$lnCVR\_{main} = \frac{1}{2} \ln\left( \frac{CV\_{AB} \cdot
CV\_{A}}{CV\_{B} \cdot CV\_{Ctrl}} \right) + \frac{1}{2} \left(
\frac{1}{2(n\_{AB} - 1)} + \frac{1}{2(n\_{A} - 1)} - \frac{1}{2(n\_{B} -
1)} - \frac{1}{2(n\_{Ctrl} - 1)} \right)\$\$

\$\$var(\ln CVR\_{main}) = var(\ln RR\_{main}) + var(\ln VR\_{main})\$\$

This follows the assumption of no correlation between mean and variance.

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
data <- data.frame(
  study_id = 1:2,
  control_mean = c(14.2, 16.8), control_sd = c(2.8, 3.1), control_n = c(16, 14),
  irrigation_mean = c(19.5, 22.1), irrigation_sd = c(5.2, 5.8), irrigation_n = c(15, 16),
  co2_mean = c(16.8, 19.4), co2_sd = c(3.1, 3.6), co2_n = c(17, 13),
  irrigation_co2_mean = c(24.3, 27.9), irrigation_co2_sd = c(6.8, 7.4), irrigation_co2_n = c(14, 15)
)

result <- lnCVR_main(
  data = data,
  Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
  A_mean = "irrigation_mean", A_sd = "irrigation_sd", A_n = "irrigation_n",
  B_mean = "co2_mean", B_sd = "co2_sd", B_n = "co2_n", 
  AB_mean = "irrigation_co2_mean", AB_sd = "irrigation_co2_sd", AB_n = "irrigation_co2_n"
)
```
