# Log Coefficient of Variation Ratio: Interaction Between Experimental Treatment and Time

Log Coefficient of Variation Ratio: Interaction Between Experimental
Treatment and Time

## Usage

``` r
.time_interaction_lnCVR(
  t0_Ctrl_mean,
  t0_Ctrl_sd,
  t1_Ctrl_mean,
  t1_Ctrl_sd,
  Ctrl_n,
  Ctrl_cor,
  t0_Exp_mean,
  t0_Exp_sd,
  t1_Exp_mean,
  t1_Exp_sd,
  Exp_n,
  Exp_cor
)
```

## Arguments

- t0_Ctrl_mean:

  Sample mean from the control group at time 0

- t0_Ctrl_sd:

  Standard deviation from the control group at time 0

- t1_Ctrl_mean:

  Sample mean from the control group at time 1

- t1_Ctrl_sd:

  Standard deviation from the control group at time 1

- Ctrl_n:

  Sample size of the control group

- Ctrl_cor:

  Correlation between the means of the control group at t0 and t1

- t0_Exp_mean:

  Sample mean from the experimental group at time 0

- t0_Exp_sd:

  Standard deviation from the experimental group at time 0

- t1_Exp_mean:

  Sample mean from the experimental group at time 1

- t1_Exp_sd:

  Standard deviation from the experimental group at time 1

- Exp_n:

  Sample size of the experimental group

- Exp_cor:

  Correlation between the means of the experimental group at t0 and t1

## Value

A data frame containing the effect sizes and their sampling variance. By
default, the columns are named `yi` (effect size) and `vi` (sampling
variance). If `append = TRUE`, the results are appended to the input
`data`; otherwise, only the computed effect size columns are returned.
