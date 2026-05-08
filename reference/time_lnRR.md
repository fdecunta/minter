# Log Response Ratio: Interaction Between Treatment and Time

\$\$lnRR = \ln\left(\frac{\bar{X}\_{t1,Exp} /
\bar{X}\_{t1,Ctrl}}{\bar{X}\_{t0,Exp} / \bar{X}\_{t0,Ctrl}}\right)\$\$

## Usage

``` r
time_lnRR(
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

## Details

\$\$var(\ln RR) = \frac{(sd\_{t0,Exp}^2 \bar{X}\_{t1,Exp}^2 +
sd\_{t1,Exp}^2 \bar{X}\_{t0,Exp}^2 - 2r\_{Exp} \bar{X}\_{t0,Exp}
\bar{X}\_{t1,Exp} sd\_{t0,Exp} sd\_{t1,Exp})}{n\_{Exp}
\bar{X}\_{t0,Exp}^2 \bar{X}\_{t1,Exp}^2} +\$\$
\$\$\frac{(sd\_{t0,Ctrl}^2 \bar{X}\_{t1,Ctrl}^2 + sd\_{t1,Ctrl}^2
\bar{X}\_{t0,Ctrl}^2 - 2r\_{Ctrl} \bar{X}\_{t0,Ctrl} \bar{X}\_{t1,Ctrl}
sd\_{t0,Ctrl} sd\_{t1,Ctrl})}{n\_{Ctrl} \bar{X}\_{t0,Ctrl}^2
\bar{X}\_{t1,Ctrl}^2}\$\$

## References

Shinichi Nakagawa and Daniel Noble, personal communication.

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
data <- data.frame(
  study_id = 1:2,
  pre_control_mean = c(8.4, 10.2),     # Control before restoration
  pre_control_sd = c(1.8, 2.1),
  post_control_mean = c(8.9, 10.7),    # Control after restoration period
  post_control_sd = c(1.9, 2.2),
  control_n = c(22, 18),
  pre_restoration_mean = c(8.6, 10.1), # Restoration sites before
  pre_restoration_sd = c(1.9, 2.0),
  post_restoration_mean = c(15.3, 17.8), # Restoration sites after
  post_restoration_sd = c(3.2, 3.7),
  restoration_n = c(20, 19)
)

result <- time_lnRR(
  data = data,
  t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
  t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
  Ctrl_n = "control_n", Ctrl_cor = 0.7,  # Correlation within control sites
  t0_Exp_mean = "pre_restoration_mean", t0_Exp_sd = "pre_restoration_sd",
  t1_Exp_mean = "post_restoration_mean", t1_Exp_sd = "post_restoration_sd", 
  Exp_n = "restoration_n", Exp_cor = 0.6   # Correlation within restoration sites
)

# Using different correlations for each study
result2 <- time_lnRR(
  data = data,
  t0_Ctrl_mean = "pre_control_mean", t0_Ctrl_sd = "pre_control_sd",
  t1_Ctrl_mean = "post_control_mean", t1_Ctrl_sd = "post_control_sd",
  Ctrl_n = "control_n", Ctrl_cor = c(0.6, 0.8),
  t0_Exp_mean = "pre_restoration_mean", t0_Exp_sd = "pre_restoration_sd",
  t1_Exp_mean = "post_restoration_mean", t1_Exp_sd = "post_restoration_sd", 
  Exp_n = "restoration_n", Exp_cor = c(0.5, 0.7)
)
```
