# Main effect: Log Response Ratio

Computes the main effect of Factor A across levels of Factor B,
analogous to the main effect in a factorial ANOVA.

## Usage

``` r
lnRR_main(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  method = "nakagawa",
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

- method:

  Method to compute lnRR. Can be either "nakagawa" or "morris". Default
  is "nakagawa".

- Ctrl_mean:

  Mean outcome from the Control treatment

- Ctrl_sd:

  Standard deviation from the control treatment

- Ctrl_n:

  Sample size from the control treatment

- A_mean:

  Mean outcome from the A treatment

- A_sd:

  Standard deviation from the A treatment

- A_n:

  Sample size from the A treatment

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

Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz,
V. A., Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and
interactive effects of enemies and mutualists on plant performance: a
meta‐analysis. Ecology, 88(4), 1021-1029.
https://doi.org/10.1890/06-0442

Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for
studies with correlated and multi‐group designs. Ecology, 92(11),
2049-2055. https://doi.org/10.1890/11-0423.1

Macartney, E. L., Lagisz, M., & Nakagawa, S. (2022). The relative
benefits of environmental enrichment on learning and memory are greater
when stressed: A meta-analysis of interactions in rodents. Neuroscience
& Biobehavioral Reviews, 135, 104554.
https://doi.org/10.1016/j.neubiorev.2022.104554

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
# Example data for 2x2 factorial design (Fertilization x Warming)
data <- data.frame(
  study_id = 1:2,
  control_mean = c(10, 12), control_sd = c(2.0, 2.5), control_n = c(20, 18),
  fertilization_mean = c(15, 16), fertilization_sd = c(2.2, 2.8), fertilization_n = c(20, 19),
  warming_mean = c(11, 13), warming_sd = c(2.1, 2.6), warming_n = c(21, 17),
  fert_warm_mean = c(17, 19), fert_warm_sd = c(2.4, 3.0), fert_warm_n = c(19, 20)
)

# Compute main effect of fertilization
result <- lnRR_main(
  data = data,
  Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
  A_mean = "fertilization_mean", A_sd = "fertilization_sd", A_n = "fertilization_n",
  B_mean = "warming_mean", B_sd = "warming_sd", B_n = "warming_n",
  AB_mean = "fert_warm_mean", AB_sd = "fert_warm_sd", AB_n = "fert_warm_n"
)
```
