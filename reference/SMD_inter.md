# Interaction effect: Standardized mean difference

Computes the interaction effect between factors A and B in factorial
data.

## Usage

``` r
SMD_inter(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  hedges_correction = TRUE,
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

- hedges_correction:

  Logical. Apply or not Hedges' correction for small-sample bias.
  Default is TRUE

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

See the package vignette for a detailed description of the formula.

## References

Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The interaction
between competition and predation: a meta-analysis of field experiments.
The American Naturalist, 155(4), 435-453.

Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz,
V. A., Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and
interactive effects of enemies and mutualists on plant performance: a
meta‐analysis. Ecology, 88(4), 1021-1029.
https://doi.org/10.1890/06-0442

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
data <- data.frame(
  study_id = 1:2,
  control_mean = c(24.8, 27.2), control_sd = c(4.1, 4.6), control_n = c(18, 16),
  salinity_mean = c(19.3, 21.7), salinity_sd = c(3.8, 4.2), salinity_n = c(17, 18),
  temperature_mean = c(28.9, 31.4), temperature_sd = c(4.7, 5.1), temperature_n = c(19, 15),
  salt_temp_mean = c(15.2, 17.8), salt_temp_sd = c(3.1, 3.5), salt_temp_n = c(16, 17)
)

result <- SMD_inter(
  data = data,
  Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
  A_mean = "salinity_mean", A_sd = "salinity_sd", A_n = "salinity_n",
  B_mean = "temperature_mean", B_sd = "temperature_sd", B_n = "temperature_n",
  AB_mean = "salt_temp_mean", AB_sd = "salt_temp_sd", AB_n = "salt_temp_n"
)
```
