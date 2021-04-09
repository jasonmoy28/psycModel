# psycModel
## Intergrated Toolkit for Pyschological Analysis and Modeling in R

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R package version](https://img.shields.io/github/r-package/v/jasonmoy28/psycModel)]()
[![Github last commit](https://img.shields.io/github/last-commit/jasonmoy28/psycModel)]()
[![GPLv3license](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://perso.crans.org/besson/LICENSE.html)
[![DOI](https://zenodo.org/badge/355611696.svg)](https://zenodo.org/badge/latestdoi/4671923)
<!-- badges: end -->

## Installation
```R
devtools::install_github('jasonmoy28/psycModel')
```
## Feature
The package consists of three types of analysis: descriptive statistics, mixed effect model, latent variable model

1. Descriptive statistics: correlation (`cor_test`); descriptive statistics table (`descriptive_table`)

2. Mixed effect model: linear mixed effect model (`lme_model`); generalized linear mixed effect model (`glme_model`); mixed effect model interaction plot (`two_way_interaction_plot`, `three_way_interaction_plot`); mixed effect model summary (`model_summary`, `model_summary_with_plot`)

3. Latent variable Modeling: Confirmatory factor analysis (`cfa_summary`, `measurement_invariance`)


## Credit
**Authors:** [Jason Moy](https://jasonmoy.us)

**Citation:** Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel


**BibTex Entry:**
  @Manual {,  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;title = {{psycModel}Intergrated Toolkit for Pyschological Analysis and Modeling in R},  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;author = {Jason Moy},  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;year = {2021},  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;url = {https://github.com/jasonmoy28/psycModel},  
  }

## Updates
### Prospective Features: 
1. Mediation analysis 
2. Latent class and profile analysis 

### version 0.1.0 (released on 4/8/2021)
* initial build of package
