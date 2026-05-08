# Interaction effect: Log Variability Ratio

Computes the interaction of Factors A and B measured as the log of the
variability ratio.

## Usage

``` r
lnVR_inter(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd,
  B_n,
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

- Ctrl_sd:

  Standard deviation from the control treatment

- Ctrl_n:

  Sample size from the control treatment

- A_sd:

  Standard deviation from the A treatment

- A_n:

  Sample size from the A treatment

- B_sd:

  Standard deviation from the B treatment

- B_n:

  Sample size from the B treatment

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

\$\$lnVR\_{inter} = \ln\left( \frac{sd\_{AB} / sd\_{B}}{sd\_{A} /
sd\_{Ctrl}} \right) + \frac{1}{2(n\_{AB} - 1)} - \frac{1}{2(n\_{A} -
1)} - \frac{1}{2(n\_{B} - 1)} + \frac{1}{2(n\_{Ctrl} - 1)}\$\$

\$\$var(\lnVR\_{inter}) = \frac{1}{2(n\_{AB} - 1)} + \frac{1}{2(n\_{A} -
1)} + \frac{1}{2(n\_{B} - 1)} + \frac{1}{2(n\_{Ctrl} - 1)}\$\$

See the package vignette for a detailed description of the formula.

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
# Example for interaction effect in 2x2 factorial focusing on variability (Drought x Temperature)
data <- data.frame(
  study_id = 1:2,
  control_sd = c(1.8, 2.1), control_n = c(22, 19),
  drought_sd = c(2.6, 2.9), drought_n = c(20, 21),
  temperature_sd = c(2.0, 2.3), temperature_n = c(21, 18),
  drought_temp_sd = c(3.2, 3.6), drought_temp_n = c(19, 20)
)

result <- lnVR_inter(
  data = data,
  Ctrl_sd = "control_sd", Ctrl_n = "control_n",
  A_sd = "drought_sd", A_n = "drought_n", 
  B_sd = "temperature_sd", B_n = "temperature_n",
  AB_sd = "drought_temp_sd", AB_n = "drought_temp_n"
)
```
