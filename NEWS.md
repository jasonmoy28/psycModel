# psycModel 0.4.1
**Fixes**  
* Fixed anova plot labelling issue with two-way interaction  
* Fixed `model_summary` print lm model summary when glm model is specified  
* Fixed `model summary` cannot handle `aov` models  
* Fixed the `check_factorstructure` function to import from `performance` instead of `parameters` package  


# psycModel 0.4.0
**Major Feature**  
* Added support for ANOVA plots (with continuous variable as moderator)  
* Added support for polynomial plot (incl. curvilinear plots)   
* Added support for Cronbach alpha computation (useful to combine with `descriptive_table`)  

**Minor Feature**  
* Integrate two-way and three-way interaction plot into one function  

**Fixes**  
* Fixed the issue that some model assumption checks were not printed  
* Fixed the issue that `compare_fit` function not able to for comparing `lm` models  

# psycModel 0.3.3
**Fixes**  
* Fixed control variables must be numeric variables in interaction plots (i.e., added support for factor variables)
* Simple slope no longer need to pass in the `interaction_terms` and `data` arguments  
* Support multilevel modeling again after fixes introduced in `insight` package.  

# psycModel 0.3.2
**Fixes**  
* Drop support for multilevel modeling temporarily due to `insight` package recent non-backward compatible changes  

# psycModel 0.3.1
**Fixes**  
* Fixed bugs that measurement invariance does not have row name.  

# psycModel 0.3.0 (first CRAN release)

**Major Feature** <br/>
* Added support reliability measure summary  
* Added support mediation models  
* Added support generalized linear regression (`glm` and `glmer` without plot)  

**Minor Feature** <br/>
* `cfa_summary`  support path diagram  
* `efa_summary` rewrite using functions from `parameters` and support post-hoc CFA test  
* `cfa_summary` support factor loading is hidden for same latent factor (only when group = `NULL`)  
* `cor_test` and `descriptive_table` support rich-text formatted table output
* `model_summary` rewrite using `parameters::model_parameters`  
*  integrate summary with plot for `lm_summary` to `integrated_model_summary`
* `cor_test` re-write using the correlation package, so it supports more methods and robust standard errors  
* `quite` and `streamline` support in all models that print output  
* Give instruction on how to use R Markdown (see `knit_to_Rmd`)  



# psycModel 0.2.1

**Major Feature**  
* Added support linear regression <br/>
* Added support exploratory factor analysis <br/>
* Complete overhaul to produce rich-text formatted summary output <br/>

**Minor Feature**  
* `measurement_invariance` support multiple-factor model with tidyselect syntax <br/>
* `model_summary_with_plot` support outlier detection <br/>
* Changed data from EWCS_2015_shorten to popular (a data-set that is easier to understand) <br/>
* Added a new function that allow convert HTML to PDF function for knitting Rmd <br/>
* `model_performance` support a wider array of model performance measure <br/>
* `cfa_summary` and `measurement_invariance` support checking goodness of fit <br/>

**Fixes**  
* Critical bug fix for `model_summary_with_plot`. You can no request `simple_slope` and `check_assumption` correctly. <br/>
* Critical bug fix that `cor_test` is not exported <br/>
* remove some packages from import and switch to `requireNamespace()` <br/>
* added fallback for normality check <br/>

# psycModel 0.2.0
**Major Feature**  
* `lme_model`, `model_summary_with_plot` support tidyselect syntax <br/>
* `cfa_summary` support multi-factor CFA with tidyselect syntax <br/>

**Minor Feature**  
* Added `assumption_plot` to visually inspect assumptions for mixed effect models in `model_summary_with_plot` <br/>
* `two_way_interaction_plot`, `three_way_interaction_plot` only require the model object to produce the plot. <br/>
* `lme_model`, `model_summary_with_plot` support using `lme4` package. <br/>
* `model_summary_with_plot` `lme_model` support passing explicit model <br/>
* `compare_fit` support more model comparison (e.g., lme, glme) <br/>

**Fixes**  
* This current version build pass CMD check <br/>
* `measurement_invariance` stop using `semTools::compareFit`. Added a self-created `compare_fit` function for the package <br/>
* Remove `papaja::apa_theme()` dependency. <br/>
* Use `.data` from `rlang` for `mutate` function <br/>
* `model_summary_with_plot` always return list and changed to logical (set to T to return result_list) <br/>
* `model_summary_with_plot` return a named list object <br/>

# psycModel 0.1.1
**New Feature**  
* `descriptive_table` support wider array of descriptive indicator (e.g., median, range) and missing & non_missing values count <br/>

**Fixes**  
* Fixed the `cor_test` bug that the function return a correlation matrix with blank cells if the correlation is too high between the two items (rounded to 1).<br/>
* Add a `data_check` function that warns the users if non-numeric variables are coerced into numeric. <br/>

# psycModel 0.1.0

* initial build

