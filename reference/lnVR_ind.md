# Individual effect: Log of Variability Ratio

Computes the Log of the Variability Ratio between a Factor A and the
Control treatment in factorial experiments.

## Usage

``` r
lnVR_ind(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n
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

  Standard deviation from the treatment

- A_n:

  Sample size from the treatment

## Value

A data frame containing the effect sizes and their sampling variance. By
default, the columns are named `yi` (effect size) and `vi` (sampling
variance). If `append = TRUE`, the results are appended to the input
`data`; otherwise, only the computed effect size columns are returned.

## Details

See the package vignette for a detailed description of the formula.

## References

Nakagawa, S., Poulin, R., Mengersen, K., Reinhold, K., Engqvist, L.,
Lagisz, M., & Senior, A. M. (2015). Meta‐analysis of variation:
ecological and evolutionary applications and beyond. Methods in Ecology
and Evolution, 6(2), 143-152.

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
# Example focusing on variability differences (Herbivory effect)
data <- data.frame(
  study_id = 1:3,
  control_sd = c(2.1, 1.8, 2.5),
  control_n = c(20, 22, 18),
  herbivory_sd = c(3.2, 2.9, 3.8),
  herbivory_n = c(21, 20, 19)
)

result <- lnVR_ind(
  data = data,
  Ctrl_sd = "control_sd",
  Ctrl_n = "control_n",
  A_sd = "herbivory_sd", 
  A_n = "herbivory_n"
)
```
