# Effect Sizes Formulas

## Introduction

## Quick Navigation

#### Standardized Mean Difference (SMD)

- [`SMD_ind`](#smd_ind)
- [`SMD_main`](#smd_main)
- [`SMD_inter`](#smd_inter)

#### Log Response Ratio (lnRR)

- [`lnRR_ind`](#lnrr_ind)
- [`lnRR_main`](#lnrr_main)
- [`lnRR_inter`](#lnrr_inter)

#### Log Variation Ratio (lnVR)

- [`lnVR_ind`](#lnvr_ind)
- [`lnVR_main`](#lnvr_main)
- [`lnVR_inter`](#lnvr_inter)

#### Log Coefficient of Variation Ratio (lnCVR)

- [`lnCVR_ind`](#lncvr_ind)
- [`lnCVR_main`](#lncvr_main)
- [`lnCVR_inter`](#lncvr_inter)

### Repeated Measures Functions

- [`time_SMD`](#time_smd)
- [`time_lnRR`](#time_lnrr)
- [`time_lnVR`](#time_lnvr)
- [`time_lnCVR`](#time_lncvr)

------------------------------------------------------------------------

## Factorial Design Effect Sizes

These functions compute effect sizes for standard 2×2 factorial designs
where two factors (A and B) are manipulated, creating four treatment
groups: Control, A alone, B alone, and AB combined.

Effect sizes for factorial meta-analysis were first introduced by
Gurevitch et al. (2000), who presented methods for estimating main and
interaction effects analogous to factorial ANOVA. Originally developed
for Standardized Mean Difference, these approaches have been extended to
other effect size families including response ratios and variability
measures.

------------------------------------------------------------------------

## Log Response Ratio (lnRR)

### `lnRR_inter()`

**Formula:** \\\ln RR\_{inter} =
\ln\left(\frac{\bar{X}\_{AB}}{\bar{X}\_B}\right) -
\ln\left(\frac{\bar{X}\_A}{\bar{X}\_{Ctrl}}\right)\\

**Sampling variance:** \\var(\ln RR\_{inter}) =
\frac{sd\_{AB}^2}{n\_{AB}\bar{X}\_{AB}^2} +
\frac{sd_A^2}{n_A\bar{X}\_A^2} + \frac{sd_B^2}{n_B\bar{X}\_B^2} +
\frac{sd\_{Ctrl}^2}{n\_{Ctrl}\bar{X}\_{Ctrl}^2}\\

------------------------------------------------------------------------

## Log Variation Ratio (lnVR)

### `lnVR_ind()`

**Formula:** \\\ln VR\_{ind} =
\ln\left(\frac{sd\_{A}}{sd\_{Ctrl}}\right) + \frac{1}{2(n\_{A} - 1)} -
\frac{1}{2(n\_{Ctrl} - 1)}\\

**Sampling variance:** \\var(\ln VR\_{ind}) = \frac{1}{2(n\_{A} - 1)} +
\frac{1}{2(n\_{Ctrl} - 1)}\\

### `lnVR_main()`

**Formula:** \\\ln VR\_{main} = \frac{1}{2} \ln\left( \frac{sd\_{AB}
\cdot sd\_{A}}{sd\_{B} \cdot sd\_{Ctrl}} \right) + \frac{1}{2} \left(
\frac{1}{2(n\_{AB} - 1)} + \frac{1}{2(n\_{A} - 1)} - \frac{1}{2(n\_{B} -
1)} - \frac{1}{2(n\_{Ctrl} - 1)} \right)\\

**Sampling variance:** \\var(\ln VR\_{main}) = \frac{1}{4} \left(
\frac{1}{2(n\_{AB} - 1)} + \frac{1}{2(n\_{A} - 1)} + \frac{1}{2(n\_{B} -
1)} + \frac{1}{2(n\_{Ctrl} - 1)} \right)\\

### `lnVR_inter()`

**Formula:** \\\ln VR\_{inter} = \ln\left( \frac{sd\_{AB} /
sd\_{B}}{sd\_{A} / sd\_{Ctrl}} \right) + \frac{1}{2(n\_{AB} - 1)} -
\frac{1}{2(n\_{A} - 1)} - \frac{1}{2(n\_{B} - 1)} +
\frac{1}{2(n\_{Ctrl} - 1)}\\

**Sampling variance:** \\var(\ln VR\_{inter}) = \frac{1}{2(n\_{AB} -
1)} + \frac{1}{2(n\_{A} - 1)} + \frac{1}{2(n\_{B} - 1)} +
\frac{1}{2(n\_{Ctrl} - 1)}\\

------------------------------------------------------------------------

## Log Coefficient of Variation Ratio (lnCVR)

The Log Coefficient of Variation Ratio combines information about both
mean responses and variability by comparing coefficients of variation
(\\CV = sd / \bar{x}\\).

### `lnCVR_ind()`

**Formula:** \\\ln CVR\_{ind} = \ln\left( \frac{CV\_{A}}{CV\_{Ctrl}}
\right) + \frac{1}{2(n\_{A} - 1)} - \frac{1}{2(n\_{Ctrl} - 1)}\\

**Sampling variance:** \\var(\ln CVR\_{ind}) =
\frac{sd\_{Ctrl}^2}{n\_{Ctrl}\bar{X}\_{Ctrl}^2} + \frac{1}{2(n\_{Ctrl} -
1)} + \frac{sd_A^2}{n_A\bar{X}\_A^2} + \frac{1}{2(n_A - 1)}\\

This assumes no correlation between mean and variance (Nakagawa et
al. 2015) and is computed as the sum of lnRR and lnVR variances.

### `lnCVR_main()`

**Formula:** \\\ln CVR\_{main} = \frac{1}{2} \ln\left( \frac{CV\_{AB}
\cdot CV\_{A}}{CV\_{B} \cdot CV\_{Ctrl}} \right) + \frac{1}{2} \left(
\frac{1}{2(n\_{AB} - 1)} + \frac{1}{2(n\_{A} - 1)} - \frac{1}{2(n\_{B} -
1)} - \frac{1}{2(n\_{Ctrl} - 1)} \right)\\

**Sampling variance:** \\var(\ln CVR\_{main}) = var(\ln RR\_{main}) +
var(\ln VR\_{main})\\

This follows the assumption of no correlation between mean and variance.

### `lnCVR_inter()`

**Formula:** \\\ln CVR\_{inter} = \ln\left( \frac{CV\_{AB} /
CV\_{B}}{CV\_{A} / CV\_{Ctrl}} \right) + \frac{1}{2} \left(
\frac{1}{2(n\_{AB} - 1)} - \frac{1}{2(n\_{A} - 1)} + \frac{1}{2(n\_{B} -
1)} - \frac{1}{2(n\_{Ctrl} - 1)} \right)\\

**Sampling variance:** \\var(\ln CVR\_{inter}) = var(\ln RR\_{inter}) +
var(\ln VR\_{inter})\\

This follows the assumption of no correlation between mean and variance.

------------------------------------------------------------------------

## Repeated Measures Effect Sizes

These functions compute effect sizes for treatment × time interactions
in longitudinal studies, comparing how experimental and control groups
change over time.

### `time_SMD()`

**Formula:** \\d = \frac{((\bar{X}\_{t1,Exp} - \bar{X}\_{t1,Ctrl}) -
(\bar{X}\_{t0,Exp} - \bar{X}\_{t0,Ctrl}))}{S\_{pooled}} \cdot J\\

**Time-specific pooled standard deviation:** \\S\_{pooled} =
\sqrt{\frac{((n\_{Exp} - 1)(sd\_{t0,Exp}^2 + sd\_{t1,Exp}^2) +
(n\_{Ctrl} - 1)(sd\_{t0,Ctrl}^2 + sd\_{t1,Ctrl}^2))}{2(n\_{Exp} +
n\_{Ctrl} - 2)}}\\

**Sampling variance:** \\var(d) = \frac{2(1 - r\_{Exp})}{n\_{Exp}} +
\frac{2(1 - r\_{Ctrl})}{n\_{Ctrl}} + \frac{d^2}{2(n\_{Exp} +
n\_{Ctrl})}\\

where \\r\_{Exp}\\ and \\r\_{Ctrl}\\ are the correlations between time
points within each group.

### `time_lnRR()`

**Formula:** \\\ln RR = \ln\left(\frac{\bar{X}\_{t1,Exp} /
\bar{X}\_{t1,Ctrl}}{\bar{X}\_{t0,Exp} / \bar{X}\_{t0,Ctrl}}\right)\\

**Sampling variance:** \\var(\ln RR) = \frac{(sd\_{t0,Exp}^2
\bar{X}\_{t1,Exp}^2 + sd\_{t1,Exp}^2 \bar{X}\_{t0,Exp}^2 - 2r\_{Exp}
\bar{X}\_{t0,Exp} \bar{X}\_{t1,Exp} sd\_{t0,Exp} sd\_{t1,Exp})}{n\_{Exp}
\bar{X}\_{t0,Exp}^2 \bar{X}\_{t1,Exp}^2} +\\ \\\frac{(sd\_{t0,Ctrl}^2
\bar{X}\_{t1,Ctrl}^2 + sd\_{t1,Ctrl}^2 \bar{X}\_{t0,Ctrl}^2 - 2r\_{Ctrl}
\bar{X}\_{t0,Ctrl} \bar{X}\_{t1,Ctrl} sd\_{t0,Ctrl}
sd\_{t1,Ctrl})}{n\_{Ctrl} \bar{X}\_{t0,Ctrl}^2 \bar{X}\_{t1,Ctrl}^2}\\

### `time_lnVR()`

**Formula:** \\\ln VR = \ln\left(\frac{sd\_{t1,Exp} /
sd\_{t1,Ctrl}}{sd\_{t0,Exp} / sd\_{t0,Ctrl}}\right)\\

**Sampling variance:** \\var(\ln VR) = \frac{(1 -
r\_{Exp}^2)}{n\_{Exp} - 1} + \frac{(1 - r\_{Ctrl}^2)}{n\_{Ctrl} - 1}\\

### `time_lnCVR()`

**Formula:** \\\ln CVR = \ln\left(\frac{CV\_{t1,Exp} /
CV\_{t1,Ctrl}}{CV\_{t0,Exp} / CV\_{t0,Ctrl}}\right)\\

**Sampling variance:** \\var(\ln CVR) = var(\ln RR) + var(\ln VR)\\

------------------------------------------------------------------------

## Function Parameters

All functions share common parameters:

- **`data`**: Data frame containing the variables
- **`col_names`**: Vector of two strings naming the output columns
  (default: `c("yi", "vi")`)
- **`append`**: Logical, whether to append results to input data
  (default: `TRUE`)

### Factorial Design Parameters

- **Group statistics**: `[Group]_mean`, `[Group]_sd`, `[Group]_n` for
  each group (Ctrl, A, B, AB)
- **`hedges_correction`**: Logical, apply small-sample bias correction
  (SMD only, default: `TRUE`)

### Repeated Measures Parameters

- **Time-specific statistics**: `t0_[Group]_mean`, `t0_[Group]_sd`,
  `t1_[Group]_mean`, `t1_[Group]_sd`
- **Sample sizes**: `[Group]_n` for each group
- **Correlations**: `[Group]_cor` - correlation between time points
  within each group

------------------------------------------------------------------------

## References

- Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The
  interaction between competition and predation: a meta-analysis of
  field experiments. *The American Naturalist*, 155(4), 435-453.

- Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D.,
  Borowicz, V. A., Gilbert, G. S., … & Vázquez, D. P. (2007). Direct and
  interactive effects of enemies and mutualists on plant performance: a
  meta‐analysis. *Ecology*, 88(4), 1021-1029.

- Nakagawa, S., Poulin, R., Mengersen, K., Reinhold, K., Engqvist, L.,
  Lagisz, M., & Senior, A. M. (2015). Meta‐analysis of variation:
  ecological and evolutionary applications and beyond. *Methods in
  Ecology and Evolution*, 6(2), 143-152.

- Lajeunesse, M. J. (2011). On the meta‐analysis of response ratios for
  studies with correlated and multi‐group designs. *Ecology*, 92(11),
  2049-2055.

*Personal communications: Shinichi Nakagawa and Daniel Noble for recent
developments in factorial effect size calculations.*
