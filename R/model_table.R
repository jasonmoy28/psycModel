#' Model Table
#' Generate tables with multiple response and predictor variable (only `lm` models are supported)
#'
#' @param data `data.frame`
#' @param response_variable response variable. Support `dplyr::select()` syntax.
#' @param predictor_variable predictor variable. Support `dplyr::select()` syntax. It will automatically remove the response variable from predictor variable, so you can use `contains()` or `start_with()` safely.
#' @param control_variable control variables. Support `dplyr::select()` syntax.
#' @param full_model Default is `FALSE`. If `TRUE`, it will report the full model with coefficients of the control variables
#' @param alpha the set alpha level for marginally significant (denoted by `.`). Set to 0.05 if do not want marginally significant denotation.
#' @param return_result It set to `TRUE`, it return the model estimates data frame.
#' @param quite suppress printing output
#'
#' @return
#' data.frame
#' @export
#'
#' @examples
#' 
#' model_table(data = iris, 
#'             response_variable = c(Sepal.Length,Sepal.Width),
#'             predictor_variable = Petal.Width)

model_table = function(data, 
                       response_variable,
                       predictor_variable,
                       control_variable = NULL,
                       full_model = FALSE,
                       alpha = 0.1,
                       return_result = FALSE,
                       quite = FALSE
){
  # parse select syntax
  response_variable <- data %>%
    dplyr::select(!!dplyr::enquo(response_variable)) %>%
    names()
  predictor_variable <- data %>%
    dplyr::select(!!dplyr::enquo(predictor_variable)) %>%
    names()
  control_variable = data %>%
    dplyr::select(!!dplyr::enquo(control_variable)) %>%
    names()
  
  if (length(response_variable) > 1) {
    
    for (i in 1:length(response_variable)) {
      model = lm_model(data = data,
                       response_variable = response_variable[i],
                       predictor_variable = dplyr::all_of(c(predictor_variable,control_variable)),
                       quite = T)
      if (full_model == FALSE) {
        model_summary = model %>% 
          parameters::parameters() %>%
          tibble::as_tibble() %>% 
          dplyr::filter(.data$Parameter == predictor_variable) %>% 
          dplyr::mutate('IV' = predictor_variable) %>% 
          dplyr::mutate('DV' = response_variable[i]) %>% 
          dplyr::select(dplyr::any_of(c('DV','IV','Coefficient', 'p'))) 
        
        if (i == 1) {
          model_summary_final = model_summary
        } else{
          model_summary_final = rbind(model_summary_final,model_summary)
        }
      } else{
        model_summary = model %>% 
          parameters::parameters() %>%
          tibble::as_tibble() %>% 
          dplyr::select(dplyr::any_of('Parameter', 'Coefficient', 'p')) %>% 
          coefficent_to_p(alpha = alpha) %>% 
          dplyr::rename(!!response_variable[i] := 'Coefficient') 
        
        if (i == 1) {
          model_summary_final = model_summary
        } else{
          model_summary_final = model_summary_final %>% dplyr::full_join(model_summary,by = "Parameter") 
        }
      }
    }
    
  }
  if (length(predictor_variable) > 1) {
    
    model_summary_final = tibble::tibble()
    for (i in 1:length(predictor_variable)) {
      model = lm_model(data = data,
                       response_variable = response_variable,
                       predictor_variable = dplyr::all_of(c(predictor_variable[i],control_variable)),
                       quite = T)
      if (full_model == FALSE) {
        model_summary = model %>% 
          parameters::parameters() %>%
          tibble::as_tibble() %>% 
          dplyr::filter(.data$Parameter == predictor_variable[i]) %>% 
          dplyr::mutate('IV' = predictor_variable[i]) %>% 
          dplyr::mutate('DV' = response_variable) %>% 
          dplyr::select(dplyr::any_of(c('DV','IV','Coefficient', 'p'))) 
        
        if (i == 1) {
        model_summary_final = model_summary
        } else{
          model_summary_final = rbind(model_summary_final,model_summary)
        }
        
      } else{
        
        model_summary = model %>% 
          parameters::parameters() %>%
          tibble::as_tibble() %>% 
          dplyr::select(dplyr::any_of(c('Parameter', 'Coefficient', 'p'))) %>% 
          dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == predictor_variable[i],'Focal Predictor',.data$Parameter)) %>% 
          coefficent_to_p(alpha = alpha) %>% 
          dplyr::rename(!!predictor_variable[i] := 'Coefficient') 
        
        if (i == 1) {
          model_summary_final = model_summary
        } else{
          model_summary_final = model_summary_final %>% dplyr::full_join(model_summary,by = "Parameter") 
          
        }
      }
    }
  }
  if (quite == FALSE) {
    print_table(model_summary_final,alpha = alpha)
  }
  if (return_result) {
    return(model_summary_final)
  }
}