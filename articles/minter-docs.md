# Effect Size Calculations with minter

## Overview

The **minter** package provides tools for calculating effect sizes used
in meta-analyses, with specialized support for factorial experimental
designs and repeated measures studies. The package offers a consistent
interface for computing various effect size metrics.

## Key Features

**minter** implements two primary categories of effect size
calculations:

### 1. Factorial Design Effect Sizes

For experimental designs with two factors (2×2 designs), supporting
three types of comparisons:

- **Individual/Simple effects**: Direct comparisons between single
  treatments
- **Main effects**: Average effect of one factor across levels of
  another factor  
- **Interaction effects**: How the effect of one factor depends on the
  level of another factor

### 2. Repeated Measures Effect Sizes

For longitudinal studies examining treatment × time interactions in
before-after or time-series designs.

## Quick Navigation

### Factorial Design Functions

#### Standardized Mean Difference (SMD)

- [`SMD_ind`](#smd_ind) - Individual/simple effect
- [`SMD_main`](#smd_main) - Main effect  
- [`SMD_inter`](#smd_inter) - Interaction effect

#### Log Response Ratio (lnRR)

- [`lnRR_ind`](#lnrr_ind) - Individual/simple effect
- [`lnRR_main`](#lnrr_main) - Main effect
- [`lnRR_inter`](#lnrr_inter) - Interaction effect

#### Log Variation Ratio (lnVR)

- [`lnVR_ind`](#lnvr_ind) - Individual/simple effect
- [`lnVR_main`](#lnvr_main) - Main effect
- [`lnVR_inter`](#lnvr_inter) - Interaction effect

#### Log Coefficient of Variation Ratio (lnCVR)

- [`lnCVR_ind`](#lncvr_ind) - Individual/simple effect
- [`lnCVR_main`](#lncvr_main) - Main effect
- [`lnCVR_inter`](#lncvr_inter) - Interaction effect

### Repeated Measures Functions

- [`time_SMD`](#time_smd) - Treatment × time interaction (SMD)
- [`time_lnRR`](#time_lnrr) - Treatment × time interaction (lnRR)
- [`time_lnVR`](#time_lnvr) - Treatment × time interaction (lnVR)
- [`time_lnCVR`](#time_lncvr) - Treatment × time interaction (lnCVR)

------------------------------------------------------------------------

## Factorial Design Effect Sizes

These functions compute effect sizes for standard 2×2 factorial designs
where two factors (A and B) are manipulated, creating four treatment
groups: Control, A alone, B alone, and AB combined.

### Background

Effect sizes for factorial meta-analysis were first introduced by
Gurevitch et al. (2000), who presented methods for estimating main and
interaction effects analogous to factorial ANOVA. Originally developed
for Standardized Mean Difference, these approaches have been extended to
other effect size families including response ratios and variability
measures.

Recent developments by Shinichi Nakagawa and Daniel Noble have expanded
these methods to include lnVR and lnCVR effect sizes, providing
researchers with comprehensive tools for meta-analyzing factorial
experiments.

### Acknowledgments

We thank **Shinichi Nakagawa** and **Daniel Noble** for their
theoretical contributions to factorial meta-analysis methodology and for
generously sharing unpublished formulas.

------------------------------------------------------------------------

## Standardized Mean Difference (SMD)

The Standardized Mean Difference measures the magnitude of treatment
effects in standard deviation units, making it comparable across studies
with different measurement scales.

### Individual Effect: `SMD_ind()`

Computes the individual or simple effect of Factor A compared to
Control. This is equivalent to the classic SMD that can be computed
using metafor’s `escalc()` function with `measure = "SMD"`.

**Formula:**
$$d_{ind} = \frac{{\bar{X}}_{A} - {\bar{X}}_{Ctrl}}{S_{pooled}} \cdot J(m)$$

where the pooled standard deviation is:
$$S_{pooled} = \sqrt{\frac{\left( n_{A} - 1 \right)sd_{A}^{2} + \left( n_{Ctrl} - 1 \right)sd_{Ctrl}^{2}}{n_{A} + n_{Ctrl} - 2}}$$

**Small-sample bias correction:** The $J(m)$ correction (Hedges
correction) is applied by default but can be disabled:
$$J(m) = 1 - \frac{3}{4m - 1}$$ where $m = n_{A} + n_{Ctrl} - 2$

**Sampling variance:**
$$var\left( d_{ind} \right) = \frac{1}{n_{A}} + \frac{1}{n_{Ctrl}} + \frac{d^{2}}{2\left( n_{A} + n_{Ctrl} \right)}$$

### Main Effect: `SMD_main()`

Computes the main effect of Factor A averaged across levels of Factor B,
analogous to main effects in factorial ANOVA.

**Formula:**
$$d_{main} = \frac{\left( {\bar{X}}_{A} + {\bar{X}}_{AB} \right) - \left( {\bar{X}}_{B} + {\bar{X}}_{Ctrl} \right)}{2S_{pooled}} \cdot J(m)$$

**Pooled standard deviation (four groups):**
$$S_{pooled} = \sqrt{\frac{\left( n_{A} - 1 \right)sd_{A}^{2} + \left( n_{B} - 1 \right)sd_{B}^{2} + \left( n_{AB} - 1 \right)sd_{AB}^{2} + \left( n_{Ctrl} - 1 \right)sd_{Ctrl}^{2}}{n_{A} + n_{B} + n_{AB} + n_{Ctrl} - 4}}$$

**Degrees of freedom:** $m = n_{A} + n_{B} + n_{AB} + n_{Ctrl} - 4$

**Sampling variance:**
$$var\left( d_{main} \right) = \frac{1}{4}\left( \frac{1}{n_{A}} + \frac{1}{n_{B}} + \frac{1}{n_{AB}} + \frac{1}{n_{Ctrl}} + \frac{d_{main}^{2}}{2\left( n_{A} + n_{B} + n_{AB} + n_{Ctrl} \right)} \right)$$

### Interaction Effect: `SMD_inter()`

Computes the interaction between factors A and B, measuring how the
effect of A depends on the level of B.

**Formula:**
$$d_{inter} = \frac{\left( {\bar{X}}_{AB} - {\bar{X}}_{B} \right) - \left( {\bar{X}}_{A} - {\bar{X}}_{Ctrl} \right)}{S_{pooled}} \cdot J(m)$$

Uses the same four-group pooled standard deviation as the main effect.

**Sampling variance:**
$$var\left( d_{inter} \right) = \frac{1}{n_{A}} + \frac{1}{n_{B}} + \frac{1}{n_{AB}} + \frac{1}{n_{Ctrl}} + \frac{d_{inter}^{2}}{2\left( n_{A} + n_{B} + n_{AB} + n_{Ctrl} \right)}$$

------------------------------------------------------------------------

## Log Response Ratio (lnRR)

The Log Response Ratio measures proportional changes between treatments,
providing intuitive interpretation.

### Individual Effect: `lnRR_ind()`

**Formula:**
$$\ln RR_{ind} = \ln\left( \frac{{\bar{X}}_{A}}{{\bar{X}}_{Ctrl}} \right)$$

**Sampling variance:**
$$var\left( \ln RR_{ind} \right) = \frac{sd_{A}^{2}}{n_{A}{\bar{X}}_{A}^{2}} + \frac{sd_{Ctrl}^{2}}{n_{Ctrl}{\bar{X}}_{Ctrl}^{2}}$$

### Main Effect: `lnRR_main()`

**Formula:**
$$\ln RR_{main} = \ln\left( \frac{{\bar{X}}_{A} + {\bar{X}}_{AB}}{{\bar{X}}_{Ctrl} + {\bar{X}}_{B}} \right)$$

**Sampling variance:**
$$var\left( \ln RR_{main} \right) = \left( \frac{1}{{\bar{X}}_{A} + {\bar{X}}_{AB}} \right)^{2}\left( \frac{sd_{A}^{2}}{n_{A}} + \frac{sd_{AB}^{2}}{n_{AB}} \right) + \left( \frac{1}{{\bar{X}}_{Ctrl} + {\bar{X}}_{B}} \right)^{2}\left( \frac{sd_{Ctrl}^{2}}{n_{Ctrl}} + \frac{sd_{B}^{2}}{n_{B}} \right)$$

### Interaction Effect: `lnRR_inter()`

**Formula:**
$$\ln RR_{inter} = \ln\left( \frac{{\bar{X}}_{AB}}{{\bar{X}}_{B}} \right) - \ln\left( \frac{{\bar{X}}_{A}}{{\bar{X}}_{Ctrl}} \right)$$

**Sampling variance:**
$$var\left( \ln RR_{inter} \right) = \frac{sd_{AB}^{2}}{n_{AB}{\bar{X}}_{AB}^{2}} + \frac{sd_{A}^{2}}{n_{A}{\bar{X}}_{A}^{2}} + \frac{sd_{B}^{2}}{n_{B}{\bar{X}}_{B}^{2}} + \frac{sd_{Ctrl}^{2}}{n_{Ctrl}{\bar{X}}_{Ctrl}^{2}}$$

------------------------------------------------------------------------

## Log Variation Ratio (lnVR)

The Log Variation Ratio compares variability between treatments,
providing insights into how treatments affect response consistency.

### Individual Effect: `lnVR_ind()`

**Formula:**
$$\ln VR_{ind} = \ln\left( \frac{sd_{A}}{sd_{Ctrl}} \right) + \frac{1}{2\left( n_{A} - 1 \right)} - \frac{1}{2\left( n_{Ctrl} - 1 \right)}$$

**Sampling variance:**
$$var\left( \ln VR_{ind} \right) = \frac{1}{2\left( n_{A} - 1 \right)} + \frac{1}{2\left( n_{Ctrl} - 1 \right)}$$

### Main Effect: `lnVR_main()`

**Formula:**
$$\ln VR_{main} = \frac{1}{2}\ln\left( \frac{sd_{AB} \cdot sd_{A}}{sd_{B} \cdot sd_{Ctrl}} \right) + \frac{1}{2}\left( \frac{1}{2\left( n_{AB} - 1 \right)} + \frac{1}{2\left( n_{A} - 1 \right)} - \frac{1}{2\left( n_{B} - 1 \right)} - \frac{1}{2\left( n_{Ctrl} - 1 \right)} \right)$$

**Sampling variance:**
$$var\left( \ln VR_{main} \right) = \frac{1}{4}\left( \frac{1}{2\left( n_{AB} - 1 \right)} + \frac{1}{2\left( n_{A} - 1 \right)} + \frac{1}{2\left( n_{B} - 1 \right)} + \frac{1}{2\left( n_{Ctrl} - 1 \right)} \right)$$

### Interaction Effect: `lnVR_inter()`

**Formula:**
$$\ln VR_{inter} = \ln\left( \frac{sd_{AB}/sd_{B}}{sd_{A}/sd_{Ctrl}} \right) + \frac{1}{2\left( n_{AB} - 1 \right)} - \frac{1}{2\left( n_{A} - 1 \right)} - \frac{1}{2\left( n_{B} - 1 \right)} + \frac{1}{2\left( n_{Ctrl} - 1 \right)}$$

**Sampling variance:**
$$var\left( \ln VR_{inter} \right) = \frac{1}{2\left( n_{AB} - 1 \right)} + \frac{1}{2\left( n_{A} - 1 \right)} + \frac{1}{2\left( n_{B} - 1 \right)} + \frac{1}{2\left( n_{Ctrl} - 1 \right)}$$

------------------------------------------------------------------------

## Log Coefficient of Variation Ratio (lnCVR)

The Log Coefficient of Variation Ratio combines information about both
mean responses and variability by comparing coefficients of variation
($CV = sd/\bar{x}$).

### Individual Effect: `lnCVR_ind()`

**Formula:**
$$\ln CVR_{ind} = \ln\left( \frac{CV_{A}}{CV_{Ctrl}} \right) + \frac{1}{2\left( n_{A} - 1 \right)} - \frac{1}{2\left( n_{Ctrl} - 1 \right)}$$

**Sampling variance:**
$var\left( \ln CVR_{ind} \right) = \frac{sd_{Ctrl}^{2}}{n_{Ctrl}{\bar{X}}_{Ctrl}^{2}} + \frac{1}{2\left( n_{Ctrl} - 1 \right)} + \frac{sd_{A}^{2}}{n_{A}{\bar{X}}_{A}^{2}} + \frac{1}{2\left( n_{A} - 1 \right)}$

This assumes no correlation between mean and variance (Nakagawa et
al. 2015) and is computed as the sum of lnRR and lnVR variances.

### Main Effect: `lnCVR_main()`

**Formula:**
$$\ln CVR_{main} = \frac{1}{2}\ln\left( \frac{CV_{AB} \cdot CV_{A}}{CV_{B} \cdot CV_{Ctrl}} \right) + \frac{1}{2}\left( \frac{1}{2\left( n_{AB} - 1 \right)} + \frac{1}{2\left( n_{A} - 1 \right)} - \frac{1}{2\left( n_{B} - 1 \right)} - \frac{1}{2\left( n_{Ctrl} - 1 \right)} \right)$$

**Sampling variance:**
$var\left( \ln CVR_{main} \right) = var\left( \ln RR_{main} \right) + var\left( \ln VR_{main} \right)$

This follows the assumption of no correlation between mean and variance.

### Interaction Effect: `lnCVR_inter()`

**Formula:**
$$\ln CVR_{inter} = \ln\left( \frac{CV_{AB}/CV_{B}}{CV_{A}/CV_{Ctrl}} \right) + \frac{1}{2}\left( \frac{1}{2\left( n_{AB} - 1 \right)} - \frac{1}{2\left( n_{A} - 1 \right)} + \frac{1}{2\left( n_{B} - 1 \right)} - \frac{1}{2\left( n_{Ctrl} - 1 \right)} \right)$$

**Sampling variance:**
$var\left( \ln CVR_{inter} \right) = var\left( \ln RR_{inter} \right) + var\left( \ln VR_{inter} \right)$

This follows the assumption of no correlation between mean and variance.

------------------------------------------------------------------------

## Repeated Measures Effect Sizes

These functions compute effect sizes for treatment × time interactions
in longitudinal studies, comparing how experimental and control groups
change over time.

### Treatment × Time SMD: `time_SMD()`

Computes the standardized mean difference for the interaction between
experimental treatment and time.

**Formula:**
$$d = \frac{\left( \left( {\bar{X}}_{t1,Exp} - {\bar{X}}_{t1,Ctrl} \right) - \left( {\bar{X}}_{t0,Exp} - {\bar{X}}_{t0,Ctrl} \right) \right)}{S_{pooled}} \cdot J$$

**Time-specific pooled standard deviation:**
$$S_{pooled} = \sqrt{\frac{\left( \left( n_{Exp} - 1 \right)\left( sd_{t0,Exp}^{2} + sd_{t1,Exp}^{2} \right) + \left( n_{Ctrl} - 1 \right)\left( sd_{t0,Ctrl}^{2} + sd_{t1,Ctrl}^{2} \right) \right)}{2\left( n_{Exp} + n_{Ctrl} - 2 \right)}}$$

**Sampling variance:**
$$var(d) = \frac{2\left( 1 - r_{Exp} \right)}{n_{Exp}} + \frac{2\left( 1 - r_{Ctrl} \right)}{n_{Ctrl}} + \frac{d^{2}}{2\left( n_{Exp} + n_{Ctrl} \right)}$$

where $r_{Exp}$ and $r_{Ctrl}$ are the correlations between time points
within each group.

### Treatment × Time lnRR: `time_lnRR()`

**Formula:**
$$\ln RR = \ln\left( \frac{{\bar{X}}_{t1,Exp}/{\bar{X}}_{t1,Ctrl}}{{\bar{X}}_{t0,Exp}/{\bar{X}}_{t0,Ctrl}} \right)$$

**Sampling variance:**
$$var\left( \ln RR \right) = \frac{\left( sd_{t0,Exp}^{2}{\bar{X}}_{t1,Exp}^{2} + sd_{t1,Exp}^{2}{\bar{X}}_{t0,Exp}^{2} - 2r_{Exp}{\bar{X}}_{t0,Exp}{\bar{X}}_{t1,Exp}sd_{t0,Exp}sd_{t1,Exp} \right)}{n_{Exp}{\bar{X}}_{t0,Exp}^{2}{\bar{X}}_{t1,Exp}^{2}} +$$$$\frac{\left( sd_{t0,Ctrl}^{2}{\bar{X}}_{t1,Ctrl}^{2} + sd_{t1,Ctrl}^{2}{\bar{X}}_{t0,Ctrl}^{2} - 2r_{Ctrl}{\bar{X}}_{t0,Ctrl}{\bar{X}}_{t1,Ctrl}sd_{t0,Ctrl}sd_{t1,Ctrl} \right)}{n_{Ctrl}{\bar{X}}_{t0,Ctrl}^{2}{\bar{X}}_{t1,Ctrl}^{2}}$$

### Treatment × Time lnVR: `time_lnVR()`

**Formula:**
$$\ln VR = \ln\left( \frac{sd_{t1,Exp}/sd_{t1,Ctrl}}{sd_{t0,Exp}/sd_{t0,Ctrl}} \right)$$

**Sampling variance:**
$var\left( \ln VR \right) = \frac{\left( 1 - r_{Exp}^{2} \right)}{n_{Exp} - 1} + \frac{\left( 1 - r_{Ctrl}^{2} \right)}{n_{Ctrl} - 1}$

### Treatment × Time lnCVR: `time_lnCVR()`

**Formula:**
$$\ln CVR = \ln\left( \frac{CV_{t1,Exp}/CV_{t1,Ctrl}}{CV_{t0,Exp}/CV_{t0,Ctrl}} \right)$$

**Sampling variance:**
$$var\left( \ln CVR \right) = var\left( \ln RR \right) + var\left( \ln VR \right)$$

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
