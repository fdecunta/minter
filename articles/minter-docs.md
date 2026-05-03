# Effect Sizes Formulas

## Introduction

The **minter** package is a collection of functions for calculating
effect sizes. Specifically, effect sizes from both factorial
experimental designs and repeated measures studies. For both design
types, there are different possible measures: two for differences in
means (standardized mean difference and log response ratio), and two for
differences in variance (log variation ratio and log coefficient of
variation ratio).

Typical effect sizes compare two values, the treatments vs the control.
However, factorial experiments allows to compute different contrasts:

- **Individual effects**: Direct comparisons between single treatments,
  the classic effect size.
- **Main effects**: Average effect of one factor across levels of
  another factor, analogous to main effect in ANOVA.  
- **Interaction effects**: How the effect of one factor depends on the
  level of another factor.

For repeated measures, the only possible effect size allows to compute
the treatment × time interactions.

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

### `lnRR_ind()`

Computes the individual effect of Factor A compared to Control. This is
equivalent to the classic lnRR that can be computed using metafor’s
`escalc()` function with `measure = "ROM"`.

**Formula:**
``` math
\ln RR_{ind} = \ln\left(\frac{\bar{X}_A}{\bar{X}_{Ctrl}}\right)
```

**Sampling variance:**
``` math
var(\ln RR_{ind}) = \frac{sd_A^2}{n_A\bar{X}_A^2} + \frac{sd_{Ctrl}^2}{n_{Ctrl}\bar{X}_{Ctrl}^2}
```

### `lnRR_main()`

**Formula:**
``` math
\ln RR_{main} = \ln\left(\frac{\bar{X}_A + \bar{X}_{AB}}{\bar{X}_{Ctrl} + \bar{X}_B}\right)
```

**Sampling variance:**
``` math
var(\ln RR_{main}) = \left(\frac{1}{\bar{X}_A + \bar{X}_{AB}}\right)^2 \left(\frac{sd_A^2}{n_A} + \frac{sd_{AB}^2}{n_{AB}}\right) + \left(\frac{1}{\bar{X}_{Ctrl} + \bar{X}_B}\right)^2 \left(\frac{sd_{Ctrl}^2}{n_{Ctrl}} + \frac{sd_B^2}{n_B}\right)
```

### `lnRR_inter()`

**Formula:**
``` math
\ln RR_{inter} = \ln\left(\frac{\bar{X}_{AB}}{\bar{X}_B}\right) - \ln\left(\frac{\bar{X}_A}{\bar{X}_{Ctrl}}\right)
```

**Sampling variance:**
``` math
var(\ln RR_{inter}) = \frac{sd_{AB}^2}{n_{AB}\bar{X}_{AB}^2} + \frac{sd_A^2}{n_A\bar{X}_A^2} + \frac{sd_B^2}{n_B\bar{X}_B^2} + \frac{sd_{Ctrl}^2}{n_{Ctrl}\bar{X}_{Ctrl}^2}
```

------------------------------------------------------------------------

## Log Variation Ratio (lnVR)

### `lnVR_ind()`

**Formula:**
``` math
\ln VR_{ind} = \ln\left(\frac{sd_{A}}{sd_{Ctrl}}\right) + \frac{1}{2(n_{A} - 1)} - \frac{1}{2(n_{Ctrl} - 1)}
```

**Sampling variance:**
``` math
var(\ln VR_{ind}) = \frac{1}{2(n_{A} - 1)} + \frac{1}{2(n_{Ctrl} - 1)}
```

### `lnVR_main()`

**Formula:**
``` math
\ln VR_{main} = \frac{1}{2} \ln\left( \frac{sd_{AB} \cdot sd_{A}}{sd_{B} \cdot sd_{Ctrl}} \right) + \frac{1}{2} \left( \frac{1}{2(n_{AB} - 1)} + \frac{1}{2(n_{A} - 1)} - \frac{1}{2(n_{B} - 1)} - \frac{1}{2(n_{Ctrl} - 1)} \right)
```

**Sampling variance:**
``` math
var(\ln VR_{main}) = \frac{1}{4} \left( \frac{1}{2(n_{AB} - 1)} + \frac{1}{2(n_{A} - 1)} + \frac{1}{2(n_{B} - 1)} + \frac{1}{2(n_{Ctrl} - 1)} \right)
```

### `lnVR_inter()`

**Formula:**
``` math
\ln VR_{inter} = \ln\left( \frac{sd_{AB} / sd_{B}}{sd_{A} / sd_{Ctrl}} \right) + \frac{1}{2(n_{AB} - 1)} - \frac{1}{2(n_{A} - 1)} - \frac{1}{2(n_{B} - 1)} + \frac{1}{2(n_{Ctrl} - 1)}
```

**Sampling variance:**
``` math
var(\ln VR_{inter}) = \frac{1}{2(n_{AB} - 1)} + \frac{1}{2(n_{A} - 1)} + \frac{1}{2(n_{B} - 1)} + \frac{1}{2(n_{Ctrl} - 1)}
```

------------------------------------------------------------------------

## Log Coefficient of Variation Ratio (lnCVR)

The Log Coefficient of Variation Ratio combines information about both
mean responses and variability by comparing coefficients of variation
($`CV = sd / \bar{x}`$).

### `lnCVR_ind()`

**Formula:**
``` math
\ln CVR_{ind} = \ln\left( \frac{CV_{A}}{CV_{Ctrl}} \right) + \frac{1}{2(n_{A} - 1)} - \frac{1}{2(n_{Ctrl} - 1)}
```

**Sampling variance:**
$`var(\ln CVR_{ind}) = \frac{sd_{Ctrl}^2}{n_{Ctrl}\bar{X}_{Ctrl}^2} + \frac{1}{2(n_{Ctrl} - 1)} + \frac{sd_A^2}{n_A\bar{X}_A^2} + \frac{1}{2(n_A - 1)}`$

This assumes no correlation between mean and variance (Nakagawa et
al. 2015) and is computed as the sum of lnRR and lnVR variances.

### `lnCVR_main()`

**Formula:**
``` math
\ln CVR_{main} = \frac{1}{2} \ln\left( \frac{CV_{AB} \cdot CV_{A}}{CV_{B} \cdot CV_{Ctrl}} \right) + \frac{1}{2} \left( \frac{1}{2(n_{AB} - 1)} + \frac{1}{2(n_{A} - 1)} - \frac{1}{2(n_{B} - 1)} - \frac{1}{2(n_{Ctrl} - 1)} \right)
```

**Sampling variance:**
$`var(\ln CVR_{main}) = var(\ln RR_{main}) + var(\ln VR_{main})`$

This follows the assumption of no correlation between mean and variance.

### `lnCVR_inter()`

**Formula:**
``` math
\ln CVR_{inter} = \ln\left( \frac{CV_{AB} / CV_{B}}{CV_{A} / CV_{Ctrl}} \right) + \frac{1}{2} \left( \frac{1}{2(n_{AB} - 1)} - \frac{1}{2(n_{A} - 1)} + \frac{1}{2(n_{B} - 1)} - \frac{1}{2(n_{Ctrl} - 1)} \right)
```

**Sampling variance:**
$`var(\ln CVR_{inter}) = var(\ln RR_{inter}) + var(\ln VR_{inter})`$

This follows the assumption of no correlation between mean and variance.

------------------------------------------------------------------------

## Repeated Measures Effect Sizes

These functions compute effect sizes for treatment × time interactions
in longitudinal studies, comparing how experimental and control groups
change over time.

### `time_SMD()`

**Formula:**
``` math
d = \frac{((\bar{X}_{t1,Exp} - \bar{X}_{t1,Ctrl}) - (\bar{X}_{t0,Exp} - \bar{X}_{t0,Ctrl}))}{S_{pooled}} \cdot J
```

**Time-specific pooled standard deviation:**
``` math
S_{pooled} = \sqrt{\frac{((n_{Exp} - 1)(sd_{t0,Exp}^2 + sd_{t1,Exp}^2) + (n_{Ctrl} - 1)(sd_{t0,Ctrl}^2 + sd_{t1,Ctrl}^2))}{2(n_{Exp} + n_{Ctrl} - 2)}}
```

**Sampling variance:**
``` math
var(d) = \frac{2(1 - r_{Exp})}{n_{Exp}} + \frac{2(1 - r_{Ctrl})}{n_{Ctrl}} + \frac{d^2}{2(n_{Exp} + n_{Ctrl})}
```

where $`r_{Exp}`$ and $`r_{Ctrl}`$ are the correlations between time
points within each group.

### `time_lnRR()`

**Formula:**
``` math
\ln RR = \ln\left(\frac{\bar{X}_{t1,Exp} / \bar{X}_{t1,Ctrl}}{\bar{X}_{t0,Exp} / \bar{X}_{t0,Ctrl}}\right)
```

**Sampling variance:**
``` math
var(\ln RR) = \frac{(sd_{t0,Exp}^2 \bar{X}_{t1,Exp}^2 + sd_{t1,Exp}^2 \bar{X}_{t0,Exp}^2 - 2r_{Exp} \bar{X}_{t0,Exp} \bar{X}_{t1,Exp} sd_{t0,Exp} sd_{t1,Exp})}{n_{Exp} \bar{X}_{t0,Exp}^2 \bar{X}_{t1,Exp}^2} +
```
``` math
\frac{(sd_{t0,Ctrl}^2 \bar{X}_{t1,Ctrl}^2 + sd_{t1,Ctrl}^2 \bar{X}_{t0,Ctrl}^2 - 2r_{Ctrl} \bar{X}_{t0,Ctrl} \bar{X}_{t1,Ctrl} sd_{t0,Ctrl} sd_{t1,Ctrl})}{n_{Ctrl} \bar{X}_{t0,Ctrl}^2 \bar{X}_{t1,Ctrl}^2}
```

### `time_lnVR()`

**Formula:**
``` math
\ln VR = \ln\left(\frac{sd_{t1,Exp} / sd_{t1,Ctrl}}{sd_{t0,Exp} / sd_{t0,Ctrl}}\right)
```

**Sampling variance:**
$`var(\ln VR) = \frac{(1 - r_{Exp}^2)}{n_{Exp} - 1} + \frac{(1 - r_{Ctrl}^2)}{n_{Ctrl} - 1}`$

### `time_lnCVR()`

**Formula:**
``` math
\ln CVR = \ln\left(\frac{CV_{t1,Exp} / CV_{t1,Ctrl}}{CV_{t0,Exp} / CV_{t0,Ctrl}}\right)
```

**Sampling variance:**
``` math
var(\ln CVR) = var(\ln RR) + var(\ln VR)
```

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
