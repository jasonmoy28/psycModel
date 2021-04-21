# psycModel
## Integrated Toolkit for Psychological Analysis and Modeling in R

<!-- badges: start -->
[![R package version](https://img.shields.io/github/r-package/v/jasonmoy28/psycModel)](https://github.com/jasonmoy28/psycModel)
[![R-CMD-check](https://github.com/jasonmoy28/psycModel/workflows/R-CMD-check/badge.svg)](https://github.com/jasonmoy28/psycModel/actions)
[![Codecov test coverage](https://codecov.io/gh/jasonmoy28/psycModel/branch/master/graph/badge.svg)](https://codecov.io/gh/jasonmoy28/psycModel?branch=master)
[![DOI](https://zenodo.org/badge/355611696.svg)](https://doi.org/10.5281/zenodo.4671947)

<!-- badges: end -->

## Installation
```R
#install.packages('devtools') if devtools is not installed
devtools::install_github('jasonmoy28/psycModel')
```
## Key Features
<span style="color:#009900">✓</span> A beginner-friendly R package for statistical analysis in psychology or related field (intermediate & advanced R users should also find it useful) <br/>
<span style="color:#009900">✓</span>  Tired of manually writing all variables in a model? You can use [dplyr::select()](https://dplyr.tidyverse.org/reference/select.html) syntax for all models <br/>
<span style="color:#009900">✓</span> Fitting models, plotting, checking goodness of fit, and model assumption violations all in one place. <br/>
<span style="color:#009900">✓</span> Beautiful and easy-to-read output. Check out this [example](https://jasonmoy28.github.io/psycModel/articles/quick-introduction.html) now. <br/>
<span style="color:#009900">✓</span> In the backend, this package uses reliable R packages (e.g., `lavaan`, `lme4`, `psych`) to handle all statistical analysis. <br/>

## Supported Models
Regression models:  <br/>
* Linear regression (i.e., support ANOVA, ANCOVA)  & generalized linear regression  <br/>
* Linear mixed effect model (i.e., HLM, MLM)  & generalized linear mixed effect model.  <br/>

Structure Equation Modeling:  <br/>
* Exploratory & confirmatory factor analysis  <br/>
* Measurement invariance (MGCFA approach)  <br/>
* Mediation analysis (SEM approach) <br/>

Other:  <br/>
* Descriptive statistics 
* Correlation (e.g., pearson, polychoric, tetrachoric, spearman), 
* Reliability analysis <br/>

## Credit
**Authors:** [Jason Moy](https://jasonmoy.us)

**Citation:** Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel

## Contributing:
If you like the package, please considering give it a star. It meant a lot to me knowing that people find it useful. This package is still in its early stage. There are lots of features / models that I plan to implement in the near future (see [here](https://github.com/jasonmoy28/psycModel/issues/3)), but it takes a long time for me to implement everything. So, if you are interested in help building this package, please feel free to submit a [pull request](https://github.com/jasonmoy28/psycModel/pulls) / [GitHub issue](https://github.com/jasonmoy28/psycModel/issues). I also tried my best to fix any bugs, but the package is still beta testing. If you find any bugs, please submit them in the [GitHub issue](https://github.com/jasonmoy28/psycModel/issues). Additionally, you should expect changes that are not backward compatible until the package's first major release (i.e., v1.0.0). 

## Acknowledgement
This package was built by standing on the shoulders of giants with special thanks to researchers and developers of [`lavaan`](https://lavaan.ugent.be/), [`lme4`](https://github.com/lme4/lme4), [`lmerTest`](https://github.com/runehaubo/lmerTestR), [`nlme`](https://cran.r-project.org/package=nlme), [`performance`](https://easystats.github.io/performance/), [`parameters`](https://easystats.github.io/parameters/), [`psych`](https://personality-project.org/r/psych/), and of course, all of the [`tidyverse`](https://tidyverse.tidyverse.org/) packages. I hope my package will help someone as they had helped me. 


## Upcoming Features
1. Support bootstrapping and robust standard error for relevant models 
2. Support moderated mediation analysis and multilevel mediation analysis (currently support multilevel with level-1 variable only)
3. Support latent class and profile analysis (possible latent transition analysis)
4. Support generalized linear regression
5. Support polynomial regression
6. Support outputting table to MS word (don't know how to implement yet)  

A more exhaustive list is avaliable [here](https://github.com/jasonmoy28/psycModel/issues/3). If you have feature request, please feel free to let me know by writing a new GitHub issue. 


