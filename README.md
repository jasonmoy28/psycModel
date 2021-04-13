# psycModel
## Intergrated Toolkit for Pyschological Analysis and Modeling in R

<!-- badges: start -->
[![R package version](https://img.shields.io/github/r-package/v/jasonmoy28/psycModel)](https://github.com/jasonmoy28/psycModel)
[![R-CMD-check](https://github.com/jasonmoy28/psycModel/workflows/R-CMD-check/badge.svg)](https://github.com/jasonmoy28/psycModel/actions)
[![Codecov test coverage](https://codecov.io/gh/jasonmoy28/psycModel/branch/master/graph/badge.svg)](https://codecov.io/gh/jasonmoy28/psycModel?branch=master)
[![Github last commit](https://img.shields.io/github/last-commit/jasonmoy28/psycModel)](https://github.com/jasonmoy28/psycModel)
[![DOI](https://zenodo.org/badge/355611696.svg)](https://doi.org/10.5281/zenodo.4671947)

<!-- badges: end -->

## Installation
```R
devtools::install_github('jasonmoy28/psycModel')
```
## Key Features
<span style="color:#009900">✓</span> Easy to use for R beginners who don't want to find dedicated packages for each analysis. Check out this [quick-introduction](https://jasonmoy28.github.io/psycModel/articles/quick-introduction.html) example now.  
<span style="color:#009900">✓</span> Support [dplyr::select()](https://dplyr.tidyverse.org/reference/select.html) syntax for all models   
<span style="color:#009900">✓</span> Rich-text formatted output instead of plain text output   
<span style="color:#009900">✓</span> Support plotting based on the model. For example, interaction plot for regression, scree plot for exploratory factor analysis  
<span style="color:#009900">✓</span> Support model assumption inspection and model performance & comparisons

## Supported Models
<span style="color:#009900">✓</span> Regression models: Hierarchical linear modeling  
<span style="color:#009900">✓</span> Measurement models: Exploratory & confirmatory factor analysis, measurement invariance   
<span style="color:#009900">✓</span> Other models: Descriptive statistics & correlation table  

## Credit
**Authors:** [Jason Moy](https://jasonmoy.us)

**Citation:** Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel

## Acknoledgement
This package was built by standing on the shoulders of giants with special thanks to developers of [`lavaan`](https://lavaan.ugent.be/), [`lme4`](https://github.com/lme4/lme4), [`lmerTest`](https://github.com/runehaubo/lmerTestR), [`nlme`](https://cran.r-project.org/package=nlme), [`performance`](https://easystats.github.io/performance/), [`psych`](https://personality-project.org/r/psych/), [`jmv`](https://github.com/jamovi/jmv), and all [`tidyverse`](https://tidyverse.tidyverse.org/) packages. Additionally, I want to thank the developer of [`bruceR`](https://github.com/psychbruce/bruceR). Although [`bruceR`](https://github.com/psychbruce/bruceR) is not imported, I used `bruceR::Print` and `bruceR::print_table` for these amazing rich-text formatted output. Additionally, I want to thank the developers for [`devtools`](https://github.com/r-lib/devtools) and [`usethis`](https://usethis.r-lib.org/),[`styler`](https://github.com/r-lib/styler). It would have been much much harder (maybe even impossible for me) to write an R package without the help of these two packages. I hope my package can help someone as they had helped me. 


## Updates
### Prospective Features: 
1. Support mediation, moderated mediation analysis
2. Support latent class and profile analysis (possible latent transition analysis)
3. Support linear / generalized linear regression
4. Support reliability analysis
5. Support outlier detection

### version 0.2.0 (released on 4/11/2021)
**New Feature:**
* `lme_model`, `model_summary_with_plot` support tidyselect syntax 
* `cfa_summary` support multi-factor CFA with tidyselect syntax 
* Added `check_assumption` to inspect assumptions for mixed effect models in `model_summary_with_plot`
* `two_way_interaction_plot`, `three_way_interaction_plot` only require the model object to produce the plot.
* `lme_model`, `model_summary_with_plot` support using `lme4` package. 
* `model_summary_with_plot` `lme_model` support passing explicit model
* `compare_fit` support more model comparison (e.g., lme, glme)

**Bugs fixed:**
* the current version build pass CMD check 
* `measurement_invariance` stop using `semTools::compareFit`. Added a self-created `compare_fit` function for the package
* remove papaja::apa_theme() dependency.
* use .data from rlang for mutate function
* remove bruceR import.
* `model_summary_with_plot` always return list and changed to logical (set to T to return result_list)
* `model_summary_with_plot` return a named list object

### version 0.1.1 (released on 4/9/2021)
**New Feature:**
* `descriptive_table` support wider array of descriptive indicator (e.g., median, range) and missing & non_missing values count

**Bugs fixed:**
* Fixed the `cor_test` bug that the function return a correlation matrix with blank cells if the correlation is too high between the two items (rounded to 1).
* Add a `data_check` function that warns the users if non-numeric variables are coerced into numeric.

### version 0.1.0 (released on 4/8/2021)
* initial build of package
