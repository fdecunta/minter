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

- Gurevitch, J., Morrison, J. A., & Hedges, L. V. (2000). The interaction between competition and predation: a meta-analysis of field experiments. The American Naturalist, 155(4), 435-453.

- Morris, W. F., Hufbauer, R. A., Agrawal, A. A., Bever, J. D., Borowicz, V. A., Gilbert, G. S., ... & Vázquez, D. P. (2007). Direct and interactive effects of enemies and mutualists on plant performance: a meta‐analysis. Ecology, 88(4), 1021-1029.

- Macartney, E. L., Lagisz, M., & Nakagawa, S. (2022). The relative benefits of environmental enrichment on learning and memory are greater when stressed: A meta-analysis of interactions in rodents. Neuroscience & Biobehavioral Reviews, 135, 104554.

- Siviter, H., Bailes, E. J., Martin, C. D., Oliver, T. R., Koricheva, J., Leadbeater, E., & Brown, M. J. (2021). Agrochemicals interact synergistically to increase bee mortality. Nature, 596(7872), 389-392.

- Borowicz, V. A. (2001). Do arbuscular mycorrhizal fungi alter plant–pathogen relations?. Ecology, 82(11), 3057-3068.

- Hechtel, L. J., & Juliano, S. A. (1997). Effects of a predator on prey metamorphosis: plastic responses by prey or selective mortality?. Ecology, 78(3), 838-851.
