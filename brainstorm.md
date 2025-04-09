# minter: Meta-Analysis of Interactions in Factorial Experiments

## Overview

**minter** is a lightweight and focused R package designed to perform **meta-analyses of interaction effects** in **factorial experiments**. It provides tools for calculating and synthesizing:
- Simple effects,
- Overall effects (main effects),
- Interaction effects,
- Effect sizes related to **changes in variability** (e.g., lnCVR).

## Motivation

While most meta-analyses focus on average effects, **interactions** between factors are critical in ecology, agriculture, and global change research. These interactions tell us whether the effect of one factor depends on the level or presence of another—a key insight often lost in traditional meta-analyses.

Despite the prevalence of factorial experiments in the primary literature, **there is no standard tool** to synthesize interaction effects across studies. This package aims to fill that gap.

## Applications

- **Climate change**: Does the effect of temperature vary by rainfall regime?
- **Agriculture**: Does the impact of a pesticide differ by crop type?
- **Community ecology**: Is the effect of herbivory context-dependent on nutrient availability?

## Methodological background

minter is inspired by and builds upon established work, including:

- **Gurevitch et al. (2000)** – Hedges’ *d* for interaction effects.
- **Morris (2007)** – Log response ratio (LRR) for interactions.
- **McCartney (2022)** – Application of interaction-focused meta-analysis in ecology.
- **Nature (2023)** – Study of synergistic pesticide effects on pollinators, highlighting the importance of considering interactions.

## Package Components

### Core functions

- `interaction_d()` – Calculates Hedges’ *d* for interaction.
- `interaction_lrr()` – Calculates log response ratio for interaction.
- `interaction_lnCVR()` – Effect size for change in variability.
- `meta_interaction()` – Performs meta-analysis on interaction effects.

### Visualization tools

- `plot_interaction_forest()` – Forest plot for interaction effects.
- `plot_interaction_surface()` – Optional 3D visualization (context-specific).

### Documentation

- A full **vignette/tutorial** will walk users through:
  - Input data structure,
  - Effect size calculation,
  - Meta-analysis model,
  - Interpretation and visualization.

## Availability

- The package will be hosted on **GitHub** for accessibility and collaboration.
- It will be accompanied by a paper submitted to **Methods in Ecology and Evolution**.


