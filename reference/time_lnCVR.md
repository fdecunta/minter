# Log Coefficient of Variation Ratio: Interaction Between Treatment and Time

Log Coefficient of Variation Ratio: Interaction Between Treatment and
Time

## Usage

``` r
time_lnCVR(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
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

- data:

  Data frame containing the variables used.

- col_names:

  Vector of two strings to name the output columns for the effect size
  and its sampling variance. Default is 'yi' and 'vi'.

- append:

  Logical. Append the results to `data`. Default is TRUE

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

  Number or numeric vector. Correlation between the means of the control
  group at t0 and t1

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

  Number or numeric vector. Correlation between the means of the
  experimental group at t0 and t1

## Value

A data frame containing the effect sizes and their sampling variance. By
default, the columns are named `yi` (effect size) and `vi` (sampling
variance). If `append = TRUE`, the results are appended to the input
`data`; otherwise, only the computed effect size columns are returned.

## References

Shinichi Nakagawa and Daniel Noble, personal communication.

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
# Pre-post design for coefficient of variation changes over time (Disturbance experiment)
data <- data.frame(
  study_id = 1:2,
  pre_control_mean = c(12.8, 15.4), pre_control_sd = c(2.6, 3.1),
  post_control_mean = c(13.2, 15.9), post_control_sd = c(2.7, 3.2),
  control_n = c(20, 18),
  pre_disturbed_mean = c(12.9, 15.2), pre_disturbed_sd = c(2.5, 3.0),
  post_disturbed_mean = c(8.7, 10.1), post_disturbed_sd = c(3.8, 4.3),
  disturbed_n = c(19, 21)
)

result <- time_lnCVR(
  data = data,
  t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
  t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
  Ctrl_n = "control_n", Ctrl_cor = 0.8,
  t0_Exp_mean = "pre_disturbed_mean", t0_Exp_sd = "pre_disturbed_sd",
  t1_Exp_mean = "post_disturbed_mean", t1_Exp_sd = "post_disturbed_sd",
  Exp_n = "disturbed_n", Exp_cor = 0.5
)
```
