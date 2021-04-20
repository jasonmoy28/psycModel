## Resubmission
This is a resubmission. In this version I have:
* Change all `F` and `T` to `TRUE` or `FALSE` based on CRAN reviewer's comment
* Change `dontrun{}` to `donttest{}` if `donttest{}` is more appropriate based on CRAN reviewer's comment. 
* Added `/value` to all functions based on CRAN reviewer's comment. But, I  want to add that all functions that mentioned by the CRAN reviewers except `pipe` are not exported (i.e., data_check, format_round, and tidyeval). Maybe I am looking at the wrong place, but they are not exported according to NAMESPACE file. Nonetheless, I added `/value` to all functions. 
* The CRAN reviewer also suggested adding references to the description field to the DESCRIPTION file. With all due respect, I don't think that's appropriate for this package. All the statistical analysis that this package are function call of other R packages. In other words, this package does not introduce any new implementation of statistical analysis, so I don't think it need references. There are some references included in this package, but they are not describing the statistical method itself, but it rather explain the "cut-off criteria" for the method. To put it in a simpler way, I am telling people that the p-value for statistical significance is 0.05 for linear regression (a statistical method), but I am not telling people how to linear regression (a statistical method) itself. In summary, it is not a package that is implementing a new statistical method, but merely a integration / combination of other previously built R packages. I hope it makes sense. 
* I also added a new vignette and updated some function's documentation (very trivial changes). Thanks for your time! 

## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
