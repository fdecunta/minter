# Individual effect: Log Response Ratio

Computes the individual or simple effect of Factor A over the Control.

## Usage

``` r
lnRR_ind(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  Ctrl_mean,
  Ctrl_sd,
  Ctrl_n,
  A_mean,
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

- Ctrl_mean:

  Mean outcome from the Control treatment

- Ctrl_sd:

  Standard deviation from the control treatment

- Ctrl_n:

  Sample size from the control treatment

- A_mean:

  Mean outcome from the experimental treatment

- A_sd:

  Standard deviation from the experimental treatment

- A_n:

  Sample size from the experimental treatment

## Value

A data frame containing the effect sizes and their sampling variance. By
default, the columns are named `yi` (effect size) and `vi` (sampling
variance). If `append = TRUE`, the results are appended to the input
`data`; otherwise, only the computed effect size columns are returned.

## Details

It is the classic Log Response Ratio (lnRR), which can also be computed
with metafor's `escalc()` function using `measure = "ROM"`.

The log response ratio of Factor A over Control is computed as:

**Formulas:** \$\$lnRR\_{ind} =
\ln\left(\frac{\bar{X}\_A}{\bar{X}\_{Ctrl}}\right)\$\$

\$\$var(lnRR\_{ind}) = \frac{sd_A^2}{n_A\bar{X}\_A^2} +
\frac{sd\_{Ctrl}^2}{n\_{Ctrl}\bar{X}\_{Ctrl}^2}\$\$

## References

Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz,
V. A., Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and
interactive effects of enemies and mutualists on plant performance: a
meta‐analysis. Ecology, 88(4), 1021-1029.
https://doi.org/10.1890/06-0442

Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for
studies with correlated and multi‐group designs. Ecology, 92(11),
2049-2055. https://doi.org/10.1890/11-0423.1

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
data <- data.frame(
  study_id = 1:3,
  control_mean = c(10, 15, 12),
  control_sd = c(2.1, 3.2, 2.8),
  control_n = c(20, 25, 18),
  drought_mean = c(12, 18, 14),
  drought_sd = c(2.3, 3.5, 3.1),
  drought_n = c(22, 24, 20)
)

# Compute individual effect of drought vs control
result <- lnRR_ind(
  data = data,
  Ctrl_mean = "control_mean",
  Ctrl_sd = "control_sd", 
  Ctrl_n = "control_n",
  A_mean = "drought_mean",
  A_sd = "drought_sd",
  A_n = "drought_n"
)
```
