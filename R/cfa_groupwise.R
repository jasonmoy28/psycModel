#' Confirmatory Factor Analysis (groupwise)
#'
#' This function will run a series of CFA (n = length(group)) with respect to each group. The function is intended to help you get a better understanding of which group has abnormal fit indicator
#'
#' @param data data frame
#' @param model explicit lavaan model. Either the `model` argument or the `items` argument must be specified.
#' @param group character. group variable.
#' @param items vector of tidyselect syntax or helpers. default to NULL if the model is specified. The argument will be ignored if the model is explicitly specified.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#'
#' @references 
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modelling in R. R package version 0.1.0, https://github.com/jasonmoy28/psycModel.#'
#' 
#' Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1–36. https://www.jstatsoft.org/v48/i02/.
#'
#' @export
#' @examples
#' # The example is used as the illustration of the function output only.
#' # It does not imply the data is appropriate for the analysis. 
#' cfa_groupwise(data = iris,group = 'Species',items = c(everything(), - Species))
#'

cfa_groupwise = function(data,
                         model = NULL,
                         group,
                         items = NULL,
                         ordered = F){
  
  items = ggplot2::enquo(items)
  group = ggplot2::enquo(group)
  
  if (is.null(model)) {
    data = data %>% dplyr::select(!!items, !!group)
    cfa_items = data %>% dplyr::select(!!items) %>% names(.)
    model = paste('DV =~', paste(cfa_items, collapse = ' + '))
  }
  
  groups = data %>% dplyr::select(!! group) %>% dplyr::distinct()
  groups = c(groups)[[1]]
  return_df = data.frame(group = NULL,cfi = NULL, rmsea = NULL, tli = NULL)
  for (i in groups) {
    cfa_data = data %>%
      dplyr::filter(dplyr::across(!! group) == i)
    cfa_model_summary = cfa_summary(model = model, data = cfa_data,ordered = ordered,return_result = 'short_summary',quite = T)
    cfa_model_summary = as.data.frame(cfa_model_summary)
    summary_df = data.frame(group = i, cfi = cfa_model_summary['cfi',], rmsea = cfa_model_summary['rmsea',],tli = cfa_model_summary['tli',])
    return_df = rbind(return_df,summary_df)
  }
  return(return_df)
}
