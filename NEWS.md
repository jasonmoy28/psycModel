# psycModel 0.2.0
**New Feature:**
* `lme_model`, `model_summary_with_plot` support tidyselect syntax 
* `cfa_summary` support multi-factor CFA with tidyselect syntax.
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

# psycModel 0.1.1
**New Feature**: 
* `descriptive_table` support wider array of descriptive indicator (e.g., median, range) and missing & non_missing values count

**Bugs fixed**: 
* Fixed the `cor_test` bug that the function return a correlation matrix with blank cells if the correlation is too high between the two items (rounded to 1). 
* Add a `data_check` function that warns the users if non-numeric variables are coerced into numeric. 

# psycModel 0.1.0

* initial build

