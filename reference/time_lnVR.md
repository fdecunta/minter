# Log of Variability Ratio: Interaction Between Treatment and Time

\$\$\lnVR = \ln\left(\frac{sd\_{t1,Exp} / sd\_{t1,Ctrl}}{sd\_{t0,Exp} /
sd\_{t0,Ctrl}}\right)\$\$

## Usage

``` r
time_lnVR(
  data,
  col_names = c("yi", "vi"),
  append = TRUE,
  t0_Ctrl_sd,
  t1_Ctrl_sd,
  Ctrl_n,
  Ctrl_cor,
  t0_Exp_sd,
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

- t0_Ctrl_sd:

  Standard deviation from the control group at time 0

- t1_Ctrl_sd:

  Standard deviation from the control group at time 1

- Ctrl_n:

  Sample size of the control group

- Ctrl_cor:

  Number or numeric vector. Correlation between the means of the control
  group at t0 and t1

- t0_Exp_sd:

  Standard deviation from the experimental group at time 0

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

\$\$var(\ln VR) = \frac{(1 - r\_{Exp}^2)}{n\_{Exp} - 1} + \frac{(1 -
r\_{Ctrl}^2)}{n\_{Ctrl} - 1}\$\$

## References

Shinichi Nakagawa and Daniel Noble, personal communication.

## Author

Facundo Decunta - fdecunta@agro.uba.ar

## Examples

``` r
data <- data.frame(
  study_id = 1:2,
  pre_control_sd = c(2.1, 2.4),
  post_control_sd = c(2.2, 2.5),
  control_n = c(24, 19),
  pre_invaded_sd = c(2.0, 2.3),
  post_invaded_sd = c(4.1, 4.6),
  invaded_n = c(21, 22)
)

result <- time_lnVR(
  data = data,
  t0_Ctrl_sd = "pre_control_sd", t1_Ctrl_sd = "post_control_sd",
  Ctrl_n = "control_n", Ctrl_cor = 0.6,
  t0_Exp_sd = "pre_invaded_sd", t1_Exp_sd = "post_invaded_sd",
  Exp_n = "invaded_n", Exp_cor = 0.4
)
```
