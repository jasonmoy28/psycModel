#' Comparison of Model Fit
#' 
#' `r lifecycle::badge("experimental")` \cr
#' Compare the fit indices models (see below for model support)
#'
#' @param models list of object from `lavaan` (potential expansion with other models)
#' @param digits number of digit to round
#'
#' @return
#' data frame with fit indices and change in fit indices 
#' @export
#'
#' 
compare_fit = function(models,
                       digits = 3)
{
  if (class(models[[1]]) == 'lavaan') {
    blank_df = tibble::tibble(chisq = '', df = '', pvalue = '', cfi = '', rmsea = '',srmr = '',tli = '',aic = '', bic = '', bic2 = '',rowname ='.') %>% tibble::column_to_rownames()
    return_df = tibble::tibble(chisq = NULL, df = NULL, pvalue = NULL, cfi = NULL, rmsea = NULL,srmr = NULL,tli = NULL,aic = NULL, bic = NULL, bic2 = NULL)
    fit_indices_df = tibble::tibble(chisq = NULL, df = NULL, pvalue = NULL, cfi = NULL, rmsea = NULL,srmr = NULL,tli = NULL,aic = NULL, bic = NULL, bic2 = NULL)
    model_name = c('configural','metric','scalar')
    i = 0
    for (model in models) {
      i = i + 1
      fit_measure = lavaan::fitmeasures(model)
      fit_indices = c('chisq','df','pvalue','cfi','rmsea','srmr','tli','aic','bic','bic2')
      fit_indices_loop_df = as.data.frame(fit_measure[fit_indices]) %>% 
        tibble::rownames_to_column() %>% 
        tidyr::pivot_wider(names_from = 'rowname', values_from = 'fit_measure[fit_indices]') %>% 
        dplyr::mutate(model_name = model_name[i]) %>% 
        tibble::column_to_rownames(var = 'model_name')
      fit_indices_df = rbind(fit_indices_df,fit_indices_loop_df)
    }
    if (nrow(fit_indices_df) == 2) { #config and metric model
      config_metric = fit_indices_df[2,] - fit_indices_df[1,] %>% as.data.frame()
      rownames(config_metric) = 'metric - config'
      compare_fit_df = config_metric
    } else if(nrow(fit_indices_df) == 3){
      config_metric = fit_indices_df[2,] - fit_indices_df[1,] %>% as.data.frame()
      metric_scalar = fit_indices_df[3,] - fit_indices_df[2,] %>% as.data.frame()
      rownames(config_metric) = 'metric - config'
      rownames(metric_scalar) = 'scalar - metric'
      compare_fit_df = rbind(config_metric,metric_scalar)
    }
    
    fit_indices_df = fit_indices_df %>% 
      dplyr::mutate(across(tidyr::everything(), ~ format(round(., digits = digits),nsmall = digits)))
    compare_fit_df = compare_fit_df %>% 
      dplyr::mutate(across(tidyr::everything(), ~ format(round(., digits = digits),nsmall = digits)))
    
    return_df = 
      rbind(fit_indices_df,blank_df,compare_fit_df)
    return(return_df)
  }
}