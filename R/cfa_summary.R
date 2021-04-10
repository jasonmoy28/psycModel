#' Confirmatory Factor Analysis
#' 
#' `r lifecycle::badge("stable")` \cr
#' The function fits a CFA model using the lavaan::cfa (Rosseel, 2012) function. In addition to passing an explicit lavaan model, users can fit a uni-factor CFA using the `items` argument. Moreover, users can request 3 types of summary for the model (see below for options). 
#'
#' @param data data frame
#' @param model explicit lavaan model. Either the `model` argument or the `items` argument must be specified.
#' @param group optional character. default is NULL. the nested variable for multilevel dataset (e.g., Country)
#' @param items vector of tidyselect syntax or helpers. default to NULL if the model is specified. The argument will be ignored if the model is explicitly specified.
#' @param summary_item vector of fit indices. Default is CFI, RMSEA, TLI, and SRMR. 
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param return_result Default is model. Options are 'model' (lavaan model), 'short_summary' (fit index summary only), 'long_summary' (lavaan full summary), or 'bruceR_summary' (uses the bruceR::CFA (Bao, 2021) function. Require the bruceR package. Please note that bruceR package may take a long time to install due to its dependencies)
#' @param quite default as F. If set to true, it will not print the running model statement. 
#' @param group_partial Items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').
#'
#' @references 
#' Bao, H.-W.-S. (2021). bruceR: Broadly useful convenient and efficient R functions. R package version 0.6.0. https://CRAN.R-project.org/package=bruceR 
#' 
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#' 
#' Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1–36. https://www.jstatsoft.org/v48/i02/.
#' 
#' @export
#' @examples
#' # Fitting a CFA model by passing explicit lavaan model. 
#' cfa_summary(model = 'visual  =~ x1 + x2 + x3
#'                      textual =~ x4 + x5 + x6
#'                      speed   =~ x7 + x8 + x9 ',
#'             data = lavaan::HolzingerSwineford1939)
#'             
#' # Fitting a unifactor CFA model by passing items.
#' cfa_summary(items = x1:x3,
#'             data = lavaan::HolzingerSwineford1939)
#' 
#' 
cfa_summary = function(data,
                       model = NULL,
                       group = NULL,
                       items = NULL,
                       summary_item = c('cfi', 'rmsea', 'tli','srmr'),
                       ordered = F,
                       return_result = 'model',
                       quite = F,
                       group_partial = NULL) {
  
  items = ggplot2::enquo(items)
  
  if (is.null(model)) {
    cfa_items = data %>% dplyr::select(!!items) %>% names()
    model = paste('DV =~', paste(cfa_items, collapse = ' + '))
  }
  
  # Print statement
  if (quite == F) {
    print(paste('Computing CFA using:',model))
  }
  
  # for long summary result, run CFA using bruceR
  # if(return_result == 'bruceR_summary') {
  #   if (!is.null(group)) {
  #     warning('Group variable is ignored. Multilevel CFA is only supported with returning model or short_summary')
  #   }
  #   check_package = requireNamespace('bruceR')
  #   if (check_package == F) {
  #     response = readline('Install bruceR package? It may take a long time to install. Enter Y/N ')
  #     if (stringr::str_to_upper(response) == 'Y') {
  #       utils::install.packages('bruceR')
  #       bruceR::CFA(data = data, model = model)
  #     } else{
  #       print('Installation Halted. Please do not pass "bruceR_summary" to the return_result argument')
  #     } 
  #   } else{
  #   bruceR::CFA(data = data, model = model)
  #   }
  # }

  cfa_model = lavaan::cfa(
    model = model,
    data = data,
    group = group,
    ordered = ordered,
    group.partial = group_partial
  )
  if (ordered) {
    summary_item = paste(summary_item, '.scaled', sep = '')
  }
  if (return_result == 'model') {
    return(cfa_model)
  } else if (return_result == 'short_summary') {
    cfa_short_summary = lavaan::fitMeasures(cfa_model)[summary_item]
    return(cfa_short_summary)
  } else if(return_result == 'long_summary'){
    lavaan::summary(cfa_model, fit.measure = T, standardized = T)
  }
}
