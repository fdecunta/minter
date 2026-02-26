# Pooled Standard Deviation for SMD in factorial experiments

Compute the pooled standard deviation for SMD. It computes the pooled SD
for 2 or 4 groups depending on the arguments passed. Simple SMD has only
2 groups, while main and interactions had 4 groups.

## Usage

``` r
.pooled_sd(
  Ctrl_sd,
  Ctrl_n,
  A_sd,
  A_n,
  B_sd = NULL,
  B_n = NULL,
  AB_n = NULL,
  AB_sd = NULL
)
```

## Arguments

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

- AB_n:

  Sample size from the interaction AxB treatment

- AB_sd:

  Standard deviation from the interaction AxB treatment

## References

Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz,
V. A., Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and
interactive effects of enemies and mutualists on plant performance: a
meta‐analysis. Ecology, 88(4), 1021-1029.
https://doi.org/10.1890/06-0442
