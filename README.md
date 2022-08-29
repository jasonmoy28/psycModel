# psycModel  <a href='https://jasonmoy28.github.io/psycModel/'><img src='man/figures/logo.png' align="right" height="220px" /></a>

## Integrated Toolkit for Psychological Analysis and Modeling in R

<!-- badges: start -->
[![CRAN version](https://img.shields.io/cran/v/psycModel)](https://cran.r-project.org/package=psycModel)
[![R-CMD-check](https://github.com/jasonmoy28/psycModel/workflows/R-CMD-check/badge.svg)](https://github.com/jasonmoy28/psycModel/actions)
[![Codecov test coverage](https://codecov.io/gh/jasonmoy28/psycModel/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jasonmoy28/psycModel?branch=master)
[![download](https://cranlogs.r-pkg.org/badges/grand-total/psycModel)](https://cran.r-project.org/package=psycModel)
<!-- badges: end -->

# Installation

**CRAN Stable Version**
```R
# Install the standard version 
install.packages('psycModel')

# Install all of the suggested dependencies for full functionality 
install.packages('psycModel',dependencies = c("Depends", "Imports","Suggests")) 
```
**Dev Version (newest feature)**
```R
devtools::install_github('jasonmoy28/psycModel')
```
## Key Features
<span style="color:#009900">✓</span> A beginner-friendly R package for statistical analysis in social science (intermediate & advanced R users should also find it useful)  <br/>
<span style="color:#009900">✓</span>  Tired of manually writing all variables in a model? You can use [dplyr::select()](https://dplyr.tidyverse.org/reference/select.html) syntax for all models  <br/>
<span style="color:#009900">✓</span> Produce publication-ready tables and figures (e.g., descriptive table)  <br/>
<span style="color:#009900">✓</span> Fitting models, plotting, checking goodness of fit, and model assumption violations all in one place.  <br/>
<span style="color:#009900">✓</span> Beautiful and easy-to-read output. Check out this [example](https://jasonmoy28.github.io/psycModel//articles/quick-introduction.html) now.  <br/>

## Supported Models
Regression models:  <br/>
* Linear regression (i.e., support ANOVA, ANCOVA) & curvilinear regression  <br/>
* Linear mixed effect model (i.e., HLM, MLM).  <br/>

Structure Equation Modeling:  <br/>
* Exploratory & confirmatory factor analysis  <br/>
* Measurement invariance (MGCFA approach)  <br/>
* Mediation analysis (SEM approach) <br/>

Other:  <br/>
* Descriptive statistics  <br/>
* Correlation  <br/>
* Reliability analysis  <br/>

<br/>

*Note:* If you like this package, please considering give it a star. I would really appreciate that. If you experience any problem, please feel free to open a new issue [here](https://github.com/jasonmoy28/psycModel/issues)

## Credit
**Authors:** [Jason Moy](https://jasonmoy.us)

**Citation:** Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. *CRAN*. https://cran.r-project.org/package=psycModel.

**Logo Design:** Danlin Liu

## Disclaimer:
The current release is the [alpha version](https://en.wikipedia.org/wiki/Software_release_life_cycle#Alpha) of the package since I plan to add more features and support more models in the future (read more about planned updates [here](https://github.com/jasonmoy28/psycModel/issues/3)). If you are interested in help building this package, please feel free to submit a [pull request](https://github.com/jasonmoy28/psycModel/pulls) / [GitHub issue](https://github.com/jasonmoy28/psycModel/issues). Although I tried my best to fix any bugs, the package is not guarantee to be bug-free. If you find any bugs, please submit them in the [GitHub issue](https://github.com/jasonmoy28/psycModel/issues). This package is licensed under the [GPLv3 liscense](https://www.gnu.org/licenses/gpl-3.0.en.html). You may use, re-distribute, and modified the package. Additionally, this package does provide any kind of warranty, either expressed or implied based on the GPLv3 liscense. Finally, you should expect many changes that are not backward compatible until the package's first major release (i.e., v1.0.0). 

## Upcoming Features
1. Support bootstrapping and robust standard error for relevant models 
2. Support moderated mediation analysis and multilevel mediation analysis (currently support multilevel with level-1 variable only)
3. Support latent class and profile analysis (possible latent transition analysis)
4. Support generalized linear regression
5. Support outputting table to MS word (don't know how to implement yet)  

*Note:* A more exhaustive list is available [here](https://github.com/jasonmoy28/psycModel/issues/3). If you have any feature request, please feel free to let me know by writing a new GitHub issue. 
