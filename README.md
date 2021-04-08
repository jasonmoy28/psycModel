# psycModel
## Intergrated Toolkit for Pyschological Analysis and Modelling in R

[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)

## Installation
```R
devtools::install_github('jasonmoy28/psycModel')
```
## Feature
The package consists of three types of analysis: descriptive statistics, mixed effect model, latent variable model

1. Descriptive statistics: correlation (`cor_test`); descriptive statistics table (`descriptive_table`)

2. Mixed effect model: linear mixed effect model (`lme_model`); generalized linear mixed effect model (`glme_model`); mixed effect model interaction plot (`two_way_interaction_plot`, `three_way_interaction_plot`); mixed effect model summary (`model_summary`, `model_summary_with_plot`)

3. Latent variable modelling: Confirmatory factor analysis (`cfa_summary`, `measurement_invariance`)


## Credit
**Authors:** [Jason Moy](https://jasonmoy.us)

**Citation:** Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modelling in R. R package version 0.1.0, https://github.com/jasonmoy28/psycModel.


**BibTex Entry:**
  @Manual {,  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;title = {{psycModel}Intergrated Toolkit for Pyschological Analysis and Modelling in R},  
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
