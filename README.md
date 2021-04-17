# psycModel
## Intergrated Toolkit for Psychological Analysis and Modeling in R

<!-- badges: start -->
[![R package version](https://img.shields.io/github/r-package/v/jasonmoy28/psycModel)](https://github.com/jasonmoy28/psycModel)
[![R-CMD-check](https://github.com/jasonmoy28/psycModel/workflows/R-CMD-check/badge.svg)](https://github.com/jasonmoy28/psycModel/actions)
[![Codecov test coverage](https://codecov.io/gh/jasonmoy28/psycModel/branch/master/graph/badge.svg)](https://codecov.io/gh/jasonmoy28/psycModel?branch=master)
[![Github last commit](https://img.shields.io/github/last-commit/jasonmoy28/psycModel)](https://github.com/jasonmoy28/psycModel)
[![DOI](https://zenodo.org/badge/355611696.svg)](https://doi.org/10.5281/zenodo.4671947)

<!-- badges: end -->

## Installation
```R
devtools::install_github('jasonmoy28/psycModel') #install.packages('devtools') if devtools is not installed
```
## Key Features
<span style="color:#009900">✓</span> Easy to use for R beginners who don't want to find dedicated packages for each analysis. <br/>
<span style="color:#009900">✓</span>  Tired of manually writing all variables in a model? You can use [dplyr::select()](https://dplyr.tidyverse.org/reference/select.html) syntax for all models <br/>
<span style="color:#009900">✓</span> Rich-text formatted output. Check out the  [exmaple](https://jasonmoy28.github.io/psycModel/articles/quick-introduction.html) here now. <br/>
<span style="color:#009900">✓</span> Fitting models, plotting, checking model performance and model assumption violations all in one place. <br/>

## Supported Models
<span style="color:#009900">✓</span> Regression models: Linear regression (i.e., support ANOVA, ANCOVA) Hierarchical linear modeling (i.e., linear mixed effect model). <br/>
<span style="color:#009900">✓</span> Measurement models: Exploratory & confirmatory factor analysis, measurement invariance. <br/>
<span style="color:#009900">✓</span> Other: Descriptive statistics & correlation table. <br/>

## Credit
**Authors:** [Jason Moy](https://jasonmoy.us)

**Citation:** Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel

## Acknowledgement
This package was built by standing on the shoulders of giants with special thanks to researchers and developers of [`lavaan`](https://lavaan.ugent.be/), [`lme4`](https://github.com/lme4/lme4), [`lmerTest`](https://github.com/runehaubo/lmerTestR), [`nlme`](https://cran.r-project.org/package=nlme), [`performance`](https://easystats.github.io/performance/), [parameters](https://easystats.github.io/parameters/), [`psych`](https://personality-project.org/r/psych/), and of couse, all of the [`tidyverse`](https://tidyverse.tidyverse.org/) packages. I hope my package can help someone as they had helped me. 


## Updates
### Upcoming Features: 
1. Support mediation, moderated mediation analysis
2. Support latent class and profile analysis (possible latent transition analysis)
3. Support path diagram in all model using SEM
4. Support generalized linear regression
5. Support poly


mial regression 
6. Support reliability analysis

### version 0.2.1 (released on 4/16/2021)
**Major Feature:** <br/>
* Support linear regression <br/>
* Support exploratory factor analysis <br/>
* Complete overhaul to produce rich-text formatted summary output <br/>

**Minor Feature:** <br/>
* `measurement_invariance` support multiple-factor model with tidyselect syntax <br/>
* `model_summary_with_plot` support outlier detection <br/> 
* Changed data from EWCS_2015_shorten to popular (a dataset that is easier to understand) <br/>
* Added a new function that allow convert HTML to PDF function for knitting Rmd <br/>
* `model_performance` support a wider array of model performance measure <br/>
* `cfa_summary` and `measurement_invariance` support checking goodness of fit <br/>
* Re-write `bruceR::Print` and `bruceR::print_table`. <br/>

**Bugs fixed** <br/>
* Critical bug fix for `model_summary_with_plot`. You can request `simple_slope` and `assumption_plot` correctly. <br/>
* Critical bug fix that `cor_test` is not exported <br/>
* remove some packages from import and switch to `requireNamespace()` <br/>
* added fallback for normality check <br/>

### version 0.2.0 (released on 4/11/2021)
**Major Feature:** <br/>
* `lme_model`, `model_summary_with_plot` support tidyselect syntax <br/>
* `cfa_summary` support multi-factor CFA with tidyselect syntax <br/>

**Minor Feature:** <br/>
* Added `assumption_plot` to visually inspect assumptions for mixed effect models in `model_summary_with_plot` <br/>
* `two_way_interaction_plot`, `three_way_interaction_plot` only require the model object to produce the plot. <br/>
* `lme_model`, `model_summary_with_plot` support using `lme4` package. <br/>
* `model_summary_with_plot` `lme_model` support passing explicit model. <br/>
* `compare_fit` support more model comparison (e.g., lme, glme). <br/>

**Bugs fixed:** <br/>
* the current version build pass CMD check <br/>
* `measurement_invariance` stop using `semTools::compareFit`. Added a self-created `compare_fit` function for the package <br/>
* remove `papaja::apa_theme()` dependency. <br/>
* use `.data` from `rlang` for `mutate` function <br/>
* remove `bruceR` from import. <br/>
* `model_summary_with_plot` always return list and changed to logical (set to T to return result_list) <br/>
* `model_summary_with_plot` return a named list object <br/>

### version 0.1.1 (released on 4/9/2021)

**New Feature:** <br/>
* `descriptive_table` support wider array of descriptive indicator (e.g., median, range) and missing & non_missing values count <br/>

**Bugs fixed:** <br/>
* Fixed the `cor_test` bug that the function return a correlation matrix with blank cells if the correlation is too high between the two items (rounded to 1). <br/>
* Add a `data_check` function that warns the users if non-numeric variables are coerced into numeric. <br/>

### version 0.1.0 (released on 4/8/2021)
* initial build of package
