#' Measurement Invariance
#'
#' Compute the measurement invariance model (i.e., measurement equivalence model) using multigroup confirmatory factor analysis (MGCFA; Jöreskog, 1971). This function uses the lavaan::cfa (Rosseel, 2012) and the semTools::compareFit (Jorgensen, 2021) function. Users can run the configural-metric or the configural-metric-scalar comparisons (see below for detail instruction). 
#' 
#' @param data data frame
#' @param model explicit lavaan model. Either the `model` argument or the `items` argument must be specified.
#' @param group character. group variable.
#' @param items vector of tidyselect syntax or helpers. default to NULL if the model is specified. The argument will be ignored if the model is explicitly specified.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param group_partial items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').
#' @param invariance_level "metric" or "scalar". Default is 'metric'. Set as 'metric' for configural-metric comparison, and set as 'scalar' for configural-metric-scalar comparison. 
#'
#' @references 
#' Jöreskog, K. G. (1971). Simultaneous factor analysis in several populations. Psychometrika, 36(4), 409-426.
#' 
#' Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2021). semTools: Useful tools for structural equation modeling. R package version 0.5-4. Retrieved from https://CRAN.R-project.org/package=semTools
#' 
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modelling in R. R package version 0.1.0, https://github.com/jasonmoy28/psycModel.#'
#' 
#' Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1–36. https://www.jstatsoft.org/v48/i02/.
#' 
#' @export
#' @return
#' return the compareFit object
#' 
#' @examples
#' # Fitting measurement invariance model by passing explicit lavaan model 
#' measurement_invariance(model = 'visual  =~ x1 + x2 + x3
#'                                 textual =~ x4 + x5 + x6
#'                                 speed   =~ x7 + x8 + x9 ',
#'                        data = lavaan::HolzingerSwineford1939,
#'                        group = 'school')
#'
#' # Fitting a unifactor measurement invariance model by passing items. 
#' measurement_invariance(items = x1:x3,
#'                        data = lavaan::HolzingerSwineford1939,
#'                        group = 'school')
#'
measurement_invariance = function(data,
                                  model = NULL,
                                  items = NULL,
                                  group,
                                  ordered = F,
                                  group_partial = NULL,
                                  invariance_level = 'metric') {
  
  items = ggplot2::enquo(items)
  if (is.null(model)) {
    cfa_items = data %>% dplyr::select(!!items) %>% names(.)
    model = paste('DV =~', paste(cfa_items, collapse = ' + '))
  }
  
  print(paste('Computing CFA using:',model))
  
  if (invariance_level == 'metric') {
    print('Computing for configural model')
    config_model = lavaan::cfa(
      model = model,
      data = data,
      group = group,
      ordered = ordered,
      group.partial = group_partial
    )
    print('Computing for metric model')
    metric_model = lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = 'loadings',
      ordered = ordered,
      group.partial = group_partial
    )
    
    require('semTools') # loaded semTools package for compareFit function
    fit = semTools::compareFit(config_model, metric_model)
    return(fit)
    
  } else if(invariance_level == 'scalar'){
    print('Computing for configural model')
    config_model = lavaan::cfa(
      model = model,
      data = data,
      group = group,
      ordered = ordered,
      group.partial = group_partial
    )
    print('Computing for metric model')
    metric_model = lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = 'loadings',
      ordered = ordered,
      group.partial = group_partial
    )
    print('Computing for scalar model')
    
    scalar_model = lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = c('loadings','intercepts'),
      ordered = ordered,
      group.partial = group_partial
    )
    require('semTools') # loaded semTools package for compareFit function
    fit = semTools::compareFit(config_model, metric_model,scalar_model)
    return(fit)
    
  } else{
    print('Error: Invariance level must be either metric or scalar')
    return()
  }

}
