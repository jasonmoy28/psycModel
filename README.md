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
<span style="color:#009900">✓</span> A beginner-friendly R package for modeling in psychology or related field (intermediate & advanced R users should also find it useful) <br/>
<span style="color:#009900">✓</span>  Tired of manually writing all variables in a model? You can use [dplyr::select()](https://dplyr.tidyverse.org/reference/select.html) syntax for all models <br/>
<span style="color:#009900">✓</span> Fitting models, plotting, checking goodness of fit, and model assumption violations all in one place. <br/>
<span style="color:#009900">✓</span> Beautiful and easy-to-read output. Check out the  [example](https://jasonmoy28.github.io/psycModel/articles/quick-introduction.html) here now. <br/>
<span style="color:#009900">✓</span> In the backend, this package uses reliable and well-established R packages (e.g., lavaan, lme4, psych) to handle all statistical analysis <br/>

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


## Updates
### [Upcoming Features](https://github.com/jasonmoy28/psycModel/issues/3): 
1. Support bootstrapping and robust standard error for relevant models 
2. Support moderated mediation analysis and multilevel mediation analysis (currently support multilevel with level-1 variable only)
3. Support latent class and profile analysis (possible latent transition analysis)
4. Support generalized linear regression
5. Support polynomial regression
6. Support outputting table to MS word (don't know how to implement yet)

### version 0.3.0 (initial CRAN released on 4/19/2021)

**Major Feature:** <br/>
* Support reliability measure summary
* Support mediation
* Support generalized linear regression (`glm` and `glmer` without plot)

**Minor Feature:** <br/>
* `cfa_summary`  support path diagram
* `efa_summary` rewrite using functions from `parameters` and support post-hoc CFA test
* `cfa_summary` support factor loading is hidden for same latent factor (only when group = `NULL`)
* `cor_test` and `descriptive_table` support rich-text formatted table output
* `model_summary` rewrite using `parameters::model_parameters` 
*  integrate summary with plot for `lm_summary` to `integrated_model_summary`
* `cor_test` re-write using the correlation package, so it supports more methods and robust standard errors
* `quite` and `streamline` support in all models that print output
* Give instruction on how to use R Markdown (see `knit_to_Rmd`)

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
