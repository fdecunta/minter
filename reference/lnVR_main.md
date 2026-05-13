# Main Effect: Log of the Variability Ratio

Computes the overral log of the variability ratio for Factor A across
levels of Factor B.

## Usage

``` r
lnVR_main(
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

\$\$lnVR\_{main} = \frac{1}{2} \ln\left( \frac{sd\_{AB} \cdot
sd\_{A}}{sd\_{B} \cdot sd\_{Ctrl}} \right) + \frac{1}{2} \left(
\frac{1}{2(n\_{AB} - 1)} + \frac{1}{2(n\_{A} - 1)} - \frac{1}{2(n\_{B} -
1)} - \frac{1}{2(n\_{Ctrl} - 1)} \right)\$\$

\$\$var(lnVR\_{main}) = \frac{1}{4} \left( \frac{1}{2(n\_{AB} - 1)} +
\frac{1}{2(n\_{A} - 1)} + \frac{1}{2(n\_{B} - 1)} +
\frac{1}{2(n\_{Ctrl} - 1)} \right)\$\$

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
# Example for main effect in 2x2 factorial focusing on variability (Fire x Grazing)
data <- data.frame(
  study_id = 1:2,
  control_sd = c(2.0, 2.3), control_n = c(20, 18),
  fire_sd = c(2.8, 3.1), fire_n = c(19, 20),
  grazing_sd = c(2.2, 2.5), grazing_n = c(21, 17),
  fire_grazing_sd = c(3.5, 3.8), fire_grazing_n = c(18, 19)
)

result <- lnVR_main(
  data = data,
  Ctrl_sd = "control_sd", Ctrl_n = "control_n",
  A_sd = "fire_sd", A_n = "fire_n",
  B_sd = "grazing_sd", B_n = "grazing_n",
  AB_sd = "fire_grazing_sd", AB_n = "fire_grazing_n"
)
```
