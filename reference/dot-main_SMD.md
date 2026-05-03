# Main effect: Standardized Mean Difference

Computes the main effect of Factor A across levels of Factor B,
analogous to the main effect in a factorial ANOVA.

## Usage

``` r
.main_SMD(
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
  AB_n,
  hedges_correction = TRUE
)
```

## Arguments

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

- hedges_correction:

  Boolean. If TRUE correct for small-sample bias. Default is TRUE.

## Value

A data frame containing the effect sizes and their sampling variance. By
default, the columns are named `yi` (effect size) and `vi` (sampling
variance). If `append = TRUE`, the results are appended to the input
`data`; otherwise, only the computed effect size columns are returned.

## Details

The main SMD of Factor A is computed as:

\$\$d\_{main} = \frac{(\bar{X}\_A + \bar{X}\_{AB}) - (\bar{X}\_{B} +
\bar{X}\_{Ctrl})}{2S\_{pooled}} \cdot J(m)\$\$

With the pooled standard deviation: \$\$S\_{pooled} =
\sqrt{\frac{(n_A-1)sd_A^2 + (n_B-1)sd_B^2 + (n\_{AB}-1)sd\_{AB}^2 +
(n\_{Ctrl}-1)sd\_{Ctrl}^2}{n_A + n_B + n\_{AB} + n\_{Ctrl} - 4}}\$\$

And the Hedges correction as: \$\$J(m) = 1 - \frac{3}{4m-1}\$\$

with: \$\$m = n_A + n_B + n\_{AB} + n\_{Ctrl} - 4\$\$

The sampling variance is computed as: \$\$var(d\_{main}) = \frac{1}{4}
\left(\frac{1}{n_A} + \frac{1}{n_B} + \frac{1}{n\_{AB}} +
\frac{1}{n\_{Ctrl}} + \frac{d\_{main}^2}{2(n_A + n_B + n\_{AB} +
n\_{Ctrl})}\right)\$\$

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
# Main effect of Mycorrhiza in 2x2 factorial design (AMF x Phosphorus)
data <- data.frame(
  study_id = 1:2,
  control_mean = c(12.4, 15.1), control_sd = c(2.8, 3.2), control_n = c(16, 14),
  mycorrhizae_mean = c(18.7, 21.3), mycorrhizae_sd = c(3.4, 3.9), mycorrhizae_n = c(15, 16),
  phosphorus_mean = c(14.9, 17.8), phosphorus_sd = c(3.1, 3.6), phosphorus_n = c(17, 13),
  myco_phos_mean = c(22.1, 25.4), myco_phos_sd = c(4.2, 4.8), myco_phos_n = c(14, 15)
)

result <- SMD_main(
  data = data,
  Ctrl_mean = "control_mean", Ctrl_sd = "control_sd", Ctrl_n = "control_n",
  A_mean = "mycorrhizae_mean", A_sd = "mycorrhizae_sd", A_n = "mycorrhizae_n",
  B_mean = "phosphorus_mean", B_sd = "phosphorus_sd", B_n = "phosphorus_n",
  AB_mean = "myco_phos_mean", AB_sd = "myco_phos_sd", AB_n = "myco_phos_n"
)
```
