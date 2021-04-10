# psycModel
## Intergrated Toolkit for Pyschological Analysis and Modeling in R

<!-- badges: start -->
[![R package version](https://img.shields.io/github/r-package/v/jasonmoy28/psycModel)]()
[![R-CMD-check](https://github.com/jasonmoy28/psycModel/workflows/R-CMD-check/badge.svg)](https://github.com/jasonmoy28/psycModel/actions)
[![Github last commit](https://img.shields.io/github/last-commit/jasonmoy28/psycModel)]()
[![DOI](https://zenodo.org/badge/355611696.svg)](https://doi.org/10.5281/zenodo.4671947)
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
1. Support mediation, moderated mediation analysis
2. Support latent class and profile analysis (possible latent transition analysis)
3. Support model assumption checking tools
4. Support `lm` and `glm` 
6. Support exploratory factor analysis

### version 0.1.1 (released on 4/9/2021)
**New Feature:**
* descriptive_table support wider array of descriptive indicator (e.g., median, range) and missing & non_missing values count

**Bugs fixed:**
* Fixed the cor_test bug that the function return a correlation matrix with blank cells if the correlation is too high between the two items (rounded to 1).
* Add a data_check function that warns the users if non-numeric variables are coerced into numeric.

### version 0.1.0 (released on 4/8/2021)
* initial build of package
