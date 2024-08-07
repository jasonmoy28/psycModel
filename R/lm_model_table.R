#' Linear Regression Model Table
#' 
#' `r lifecycle::badge("experimental")` \cr
#' Generate tables with multiple response, predictor, or two-way interaction variables (only `lm` models are supported). 
#' You can pass multiple variables for one type of variable (either response, pred, or interaction) only. 
#' If you want to pass multiple variables for multiple type of variable, try lm_model_explore instead.
#' At the moment, multi-categorical variables are not supported as predictors or interactions (but control is fine). Binary variable should be `numeric` instead of `factor`
#'
#' @param data `data.frame`
#' @param response_variable response variable. Support `dplyr::select()` syntax.
#' @param predictor_variable predictor variable. Support `dplyr::select()` syntax. It will automatically remove the response variable from predictor variable, so you can use `contains()` or `start_with()` safely. 
#' @param two_way_interaction_variable Two-way interaction variable. Each two-way interaction variable will interact with the predictor variable. Support `dplyr::select()` syntax.
#' @param control_variable control variables. Support `dplyr::select()` syntax. 
#' @param other_parameters catch call for all other parameters that need to be entered (e.g., non-changing interaction terms). Have to be `character` type.
#' @param marginal_alpha the set marginal_alpha level for marginally significant (denoted by `.`). Set to 0.05 if do not want marginally significant denotation.
#' @param return_result It set to `TRUE`, it return the model estimates data frame.
#' @param verbose default is `TRUE`. Set to `FALSE` to suppress outputs
#' @param show_p show the p-value in parenthesis
#'
#' @return
#' data.frame
#' @export
#'
#' @examples
#' # If you want all varibles to be changing, try lm_model_explore.
#' 
#' test = data.frame(y1 = rnorm(1000,2,3),
#' y2 = rnorm(1000,10,2),
#' y3 = rnorm(1000,1,4),
#' x1 = rnorm(1000,100,10),
#' x2 = rnorm(1000,10,1),
#' x3 = rnorm(1000,6,2),
#' m1 = rnorm(1000,3,1),
#' m2 = rnorm(1000,2,0.5),
#' m3 = rnorm(1000,9,0.1),
#' c1 = rnorm(1000,5,0.4),
#' c2 = rnorm(1000,2,0.2),
#' c3 = rnorm(1000,7,0.9)
#' )
#'
#' # Changing response variable 
#' lm_model_table(data = test, 
#'                response_variable = c(y1,y2,y3),
#'                predictor_variable = x1,
#'                control_variable = c(c1,c2,c3))
#'  
#' # Changing predictors 
#' lm_model_table(data = test, 
#'                response_variable = y1,
#'                predictor_variable = c(x1,x2,x3),
#'                control_variable = c(c1,c2,c3))
#'                  
#'                  
#' # Changing interaction terms with a non-changing response variable
#' lm_model_table(data = test, 
#'                response_variable = y1,
#'                predictor_variable = x1,
#'                two_way_interaction_variable = c(m1,m2,m3),
#'                control_variable = c(c1,c2,c3))
#'                  
#' # A non-changing interaction term with changing response variables                 
#' lm_model_table(data = test, 
#'                response_variable = c(y1,y2,y3),
#'                predictor_variable = x1,
#'                other_parameters = c('x1*m1'),
#'                control_variable = c(c1,c2,c3))                  

lm_model_table = function(data, 
                          response_variable,
                          predictor_variable,
                          two_way_interaction_variable = NULL,
                          control_variable = NULL,
                          other_parameters = NULL,
                          marginal_alpha = 0.1,
                          return_result = FALSE,
                          verbose = TRUE,
                          show_p = FALSE
){
  # parse select syntax
  response_variable <- data %>%
    tidyselect::eval_select(data = ., expr = enquo(response_variable),strict = TRUE) %>%
    names()
  predictor_variable <- data %>%
    tidyselect::eval_select(data = ., expr = enquo(predictor_variable),strict = TRUE) %>%
    names()
  control_variable = data %>%
    tidyselect::eval_select(data = ., expr = enquo(control_variable),strict = TRUE) %>%
    names()
  two_way_interaction_variable = data %>%
    tidyselect::eval_select(data = ., expr = dplyr::enquo(two_way_interaction_variable),strict = TRUE) %>%
    names()
  two_way_interaction_variable = two_way_interaction_variable[!two_way_interaction_variable %in% c(response_variable)]
  
  
  variable_nums = c(length(response_variable),length(predictor_variable),length(two_way_interaction_variable))
  
  if (sum(variable_nums > 1) == 1) {
  } else {
    stop('You can only pass multiple variables for one type of variable (either response, pred, or interaction). Try lm_model_explore instead.')
  }
  
  
  # Multiple response variables
  if (length(response_variable) > 1) {
    for (i in 1:length(response_variable)) {
      formula = paste(response_variable[i],'~',predictor_variable)
      if (length(control_variable) != 0) {
        formula = paste(formula,"+",paste(control_variable, collapse = " + "))
      }
      if (length(other_parameters) != 0) {
        formula = paste(formula,"+",paste(other_parameters, collapse = " + "))
      }
      
      formula <- stats::as.formula(formula)
      model = stats::lm(formula = formula, data = data)
      
      model_summary = model %>%
        parameters::parameters() %>%
        tibble::as_tibble() %>%
        dplyr::select(dplyr::any_of(c('Parameter', 'Coefficient', 'p'))) %>%
        coefficent_to_p(marginal_alpha = marginal_alpha,show_p = show_p) %>%
        tibble::add_row(tibble::tibble(Parameter = 'df', Coefficient = format_round(insight::get_df(model),digits = 3))) %>%
        tibble::add_row(tibble::tibble(Parameter = 'r2', Coefficient = format_round(performance::r2(model)$R2,digits = 3))) %>%
        dplyr::rename(!!response_variable[i] := 'Coefficient')
      
      if (i == 1) {
        model_summary_final = model_summary
      } else{
        model_summary_final = model_summary_final %>% dplyr::full_join(model_summary,by = "Parameter")
      }
    }
    model_summary_final = 
      model_summary_final %>% dplyr::rename('Parameter/Focal_response' = 'Parameter')
    
  }
  
  # Multiple predictor variables
  if (length(predictor_variable) > 1) {
    model_summary_final = tibble::tibble()
    for (i in 1:length(predictor_variable)) {
      formula = paste(response_variable,'~',predictor_variable[i])
      if (length(control_variable) != 0) {
        formula = paste(formula,"+",paste(control_variable, collapse = " + "))
      }
      if (length(other_parameters) != 0) {
        formula = paste(formula,"+",paste(other_parameters, collapse = " + "))
      }
      formula <- stats::as.formula(formula)
      model = stats::lm(formula = formula, data = data)
      
      model_summary = model %>%
        parameters::parameters() %>%
        tibble::as_tibble() %>%
        dplyr::select(dplyr::any_of(c('Parameter', 'Coefficient', 'p'))) %>%
        dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == predictor_variable[i],'Focal Predictor',.data$Parameter)) %>%
        coefficent_to_p(marginal_alpha = marginal_alpha,show_p = show_p) %>%
        tibble::add_row(tibble::tibble(Parameter = 'df', Coefficient = format_round(insight::get_df(model),digits = 3))) %>%
        tibble::add_row(tibble::tibble(Parameter = 'r2', Coefficient = format_round(performance::r2(model)$R2,digits = 3))) %>%
        dplyr::rename(!!predictor_variable[i] := 'Coefficient')
      
      if (i == 1) {
        model_summary_final = model_summary
      } else{
        model_summary_final = model_summary_final %>% dplyr::full_join(model_summary,by = "Parameter")
        
      }
    }
    model_summary_final = 
      model_summary_final %>% dplyr::rename('Parameter/Focal_pred' = 'Parameter')
  }
  
  if (length(two_way_interaction_variable) > 1) {
    model_summary_final = tibble::tibble()
    for (i in 1:length(two_way_interaction_variable)) {
      two_way_interaction_terms = two_way_interaction_terms(c(predictor_variable,two_way_interaction_variable[i]))
      formula = paste(response_variable,'~',predictor_variable,'+',two_way_interaction_terms)
      if (length(control_variable) != 0) {
        formula = paste(formula,"+",paste(control_variable, collapse = " + "))
      }
      if (length(other_parameters) != 0) {
        formula = paste(formula,"+",paste(other_parameters, collapse = " + "))
      }
      formula <- stats::as.formula(formula)
      model = stats::lm(formula = formula, data = data)
      
      model_summary = model %>%
        parameters::parameters() %>%
        tibble::as_tibble() %>%
        dplyr::select(dplyr::any_of(c('Parameter', 'Coefficient', 'p'))) %>%
        dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == two_way_interaction_variable[i],'Focal_interact_pred',.data$Parameter)) %>%
        dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == stringr::str_replace(paste0(predictor_variable,':',two_way_interaction_variable[i]),pattern = '\\*', replacement = ':'),'Focal_interact_term',.data$Parameter)) %>%
        coefficent_to_p(marginal_alpha = marginal_alpha,show_p = show_p) %>%
        tibble::add_row(tibble::tibble(Parameter = 'df', Coefficient = format_round(insight::get_df(model),digits = 3))) %>%
        tibble::add_row(tibble::tibble(Parameter = 'r2', Coefficient = format_round(performance::r2(model)$R2,digits = 3))) %>%
        dplyr::rename(!!two_way_interaction_terms := 'Coefficient')
      
      if (i == 1) {
        model_summary_final = model_summary
      } else{
        model_summary_final = model_summary_final %>% 
          dplyr::full_join(model_summary,by = "Parameter")
        
      }
    }
    model_summary_final = 
      model_summary_final %>% dplyr::rename('Parameter/Focal_interact_term' = 'Parameter')
    
  }
  if (verbose == TRUE) {
    print_table(model_summary_final,marginal_alpha = marginal_alpha)
    if (show_p == TRUE) {
      super_print(paste('Note: Coefficient (p-value): + p < ',marginal_alpha,', * p < 0.05, ** p < 0.01, *** p < 0.001',sep = ''))
    } else{
      super_print(paste('Note: + < ',marginal_alpha,', * p < 0.05, ** p < 0.01, *** p < 0.001',sep = ''))
    }
  }
  if (return_result) {
    return(model_summary_final)
  }
}
