# psycModel 0.3.2

# psycModel 0.3.1

Bugs fixed: 

* Fixed bugs that measurement invariance does not have row name.

# psycModel 0.3.0 (first CRAN release)

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



# psycModel 0.2.1

**Major Feature:**  
* Support linear regression <br/>
* Support exploratory factor analysis <br/>
* Complete overhaul to produce rich-text formatted summary output <br/>

**Minor Feature:**  
* `measurement_invariance` support multiple-factor model with tidyselect syntax <br/>
* `model_summary_with_plot` support outlier detection <br/>
* Changed data from EWCS_2015_shorten to popular (a data-set that is easier to understand) <br/>
* Added a new function that allow convert HTML to PDF function for knitting Rmd <br/>
* `model_performance` support a wider array of model performance measure <br/>
* `cfa_summary` and `measurement_invariance` support checking goodness of fit <br/>

**Bugs fixed**  
* Critical bug fix for `model_summary_with_plot`. You can no request `simple_slope` and `check_assumption` correctly. <br/>
* Critical bug fix that `cor_test` is not exported <br/>
* remove some packages from import and switch to `requireNamespace()` <br/>
* added fallback for normality check <br/>

# psycModel 0.2.0
**Major Feature:**  
* `lme_model`, `model_summary_with_plot` support tidyselect syntax <br/>
* `cfa_summary` support multi-factor CFA with tidyselect syntax <br/>

**Minor Feature:**  
* Added `assumption_plot` to visually inspect assumptions for mixed effect models in `model_summary_with_plot` <br/>
* `two_way_interaction_plot`, `three_way_interaction_plot` only require the model object to produce the plot. <br/>
* `lme_model`, `model_summary_with_plot` support using `lme4` package. <br/>
* `model_summary_with_plot` `lme_model` support passing explicit model <br/>
* `compare_fit` support more model comparison (e.g., lme, glme) <br/>

**Bugs fixed:**  
* the current version build pass CMD check <br/>
* `measurement_invariance` stop using `semTools::compareFit`. Added a self-created `compare_fit` function for the package <br/>
* remove `papaja::apa_theme()` dependency. <br/>
* use `.data` from `rlang` for `mutate` function <br/>
* `model_summary_with_plot` always return list and changed to logical (set to T to return result_list) <br/>
* `model_summary_with_plot` return a named list object <br/>

# psycModel 0.1.1
**New Feature:**
* `descriptive_table` support wider array of descriptive indicator (e.g., median, range) and missing & non_missing values count <br/>

**Bugs fixed:**
* Fixed the `cor_test` bug that the function return a correlation matrix with blank cells if the correlation is too high between the two items (rounded to 1).<br/>
* Add a `data_check` function that warns the users if non-numeric variables are coerced into numeric. <br/>

# psycModel 0.1.0

* initial build

