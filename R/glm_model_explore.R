#' Exploratory Linear Regression Model Table
#' 
#' `r lifecycle::badge("experimental")` \cr
#' Exploratory analyses for linear regression models with multiple response, predictor, and two-way interaction variables. (`lm` models).
#' At the moment, multi-categorical variables are not supported as predictors or interactions (but control is fine). Binary variable should be `numeric` instead of `factor`
#'
#' @param data `data.frame`
#' @param response_variable Response variable. Support `dplyr::select()` syntax.
#' @param predictor_variable Pred variable. Support `dplyr::select()` syntax.
#' @param family a GLM family. It will passed to the family argument in glm See `?glm` for possible options.
#' @param two_way_interaction_variable Two-way interaction variable. Each two-way interaction variable will interact with each pred variable. Support `dplyr::select()` syntax.
#' @param three_way_interaction_variable Three-way interaction variable. Each three-way interaction variable will interact with each pred and two-way interaction variables. Support `dplyr::select()` syntax.
#' @param control_variable Control variables. Support `dplyr::select()` syntax.
#' @param marginal_alpha Set marginal_alpha level for marginally significant (denoted by `.`). Set to 0.05 if do not want marginally significant denotation.
#' @param return_result Default is `FALSE`. If `TRUE`, it returns the model estimates as a data frame.
#' @param verbose Default is `TRUE`. Set to `FALSE` to suppress outputs
#' @param show_p Default is `TRUE`. When `TRUE`, show the p-value in parenthesis. 
#' @param print_control Default is `FALSE`. If `TRUE`, print coefficients of control variables. 
#' @param plot_interaction Default is `FALSE`. If `TRUE`, interactions will be plotted and saved on your laptop.
#' @param file_dir Path of the directory to save plot to
#' @param device Default is "jpeg". See ggplot2::ggsave() for all options. 
#' @param width Default is 8.5 (i.e., letter size width). 
#' @param height Default is 5. 
#' @param units Default is inches. Options are "in", "cm", "mm" or "px".
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' 
#' @return
#' data.frame
#' @export
#'
#' @examples
#' 
#' test = data.frame(y1 = sample(c(0, 1), size = 1000, replace = TRUE),
#' y2 = sample(c(0, 1), size = 1000, replace = TRUE),
#' y3 = sample(c(0, 1), size = 1000, replace = TRUE),
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
#' glm_model_explore(data = test, 
#'                  response_variable = c(y1,y2,y3),
#'                  predictor_variable = c(x1,x2,x3),
#'                  two_way_interaction_variable = c(m1,m2,m3),
#'                  family = binomial(link = 'logit'),
#'                  control_variable = c(c1,c2,c3))


glm_model_explore = function(data, 
                            response_variable,
                            predictor_variable,
                            family,
                            two_way_interaction_variable = NULL,
                            three_way_interaction_variable = NULL,
                            control_variable = NULL,
                            marginal_alpha = 0.1,
                            verbose = TRUE,
                            show_p = TRUE,
                            return_result = FALSE,
                            print_control = FALSE,
                            plot_interaction = FALSE,
                            file_dir = NULL,
                            device = 'jpeg',
                            width = 8.5, 
                            height = 5,
                            units = 'in',
                            y_lim = c(0,1)
){
  # parse select syntax
  response_variable <- data %>%
    tidyselect::eval_select(data = ., expr = dplyr::enquo(response_variable),strict = TRUE) %>%
    names()
  predictor_variable <- data %>%
    tidyselect::eval_select(data = ., expr = dplyr::enquo(predictor_variable),strict = TRUE) %>%
    names()
  control_variable = data %>%
    tidyselect::eval_select(data = ., expr = dplyr::enquo(control_variable),strict = TRUE) %>%
    names()
  
  two_way_interaction_variable = data %>%
    tidyselect::eval_select(data = ., expr = dplyr::enquo(two_way_interaction_variable),strict = TRUE) %>%
    names()
  
  three_way_interaction_variable = data %>%
    tidyselect::eval_select(data = ., expr = dplyr::enquo(three_way_interaction_variable),strict = TRUE) %>%
    names()
  
  model_summary_final = tibble::tibble()
  
  for (i in 1:length(response_variable)) {
    for (j in 1:length(predictor_variable)) {
      # for three-way interactions 
      if (length(three_way_interaction_variable)!=0) {
        for (k in 1:length(two_way_interaction_variable)) {
          for(l in 1:length(three_way_interaction_variable)){
            three_way_interaction_terms = three_way_interaction_terms(c(predictor_variable[j],two_way_interaction_variable[k],three_way_interaction_variable[l]))
            formula = paste(response_variable[i],'~',predictor_variable[j],'+',three_way_interaction_terms)
            if (length(control_variable) != 0) {
              formula = paste(formula,"+",paste(control_variable, collapse = " + "))
            }
            formula <- stats::as.formula(formula)
            model = stats::glm(formula = formula, data = data, family = family)
            model_summary = model %>%
              parameters::parameters() 
              tibble::as_tibble() %>% 
              dplyr::select(dplyr::any_of(c('Parameter', 'Coefficient', 'p'))) %>%
              dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == predictor_variable[j],'Pred_1_coef',.data$Parameter)) %>%
              dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == two_way_interaction_variable[k],'Pred_2_coef',.data$Parameter)) %>% 
              dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == three_way_interaction_variable[l],'Pred_3_coef',.data$Parameter)) %>% 
              dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == paste0(predictor_variable[j],':',two_way_interaction_variable[k]),'Pred_1_coef:Pred_2_coef',.data$Parameter)) %>% 
              dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == paste0(predictor_variable[j],':',three_way_interaction_variable[l]),'Pred_1_coef:Pred_3_coef',.data$Parameter)) %>% 
              dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == paste0(two_way_interaction_variable[k],':',three_way_interaction_variable[l]),'Pred_2_coef:Pred_3_coef',.data$Parameter)) %>% 
              dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == stringr::str_replace_all(three_way_interaction_terms,pattern = '\\*', replacement = ':'),'Interact_term_coef',.data$Parameter)) %>% 
              coefficent_to_p(marginal_alpha = marginal_alpha,show_p = show_p) %>%
              tidyr::pivot_wider(names_from = 'Parameter',values_from = 'Coefficient') %>% 
              dplyr::select('Interact_term_coef','Pred_1_coef','Pred_2_coef','Pred_3_coef',dplyr::everything()) %>% 
              tibble::add_column(tibble::tibble(df = format_round(insight::get_df(model),digits = 3))) %>%
              tibble::add_column(tibble::tibble(r2 = format_round(performance::r2(model)$R2,digits = 3))) %>% 
              tibble::add_column(tibble::tibble(Interact_term = three_way_interaction_terms),.before = 0) %>% 
              tibble::add_column(tibble::tibble(Pred_3 =  three_way_interaction_variable[l]),.before = 0) %>% 
              tibble::add_column(tibble::tibble(Pred_2 = two_way_interaction_variable[k]),.before = 0) %>% 
              tibble::add_column(tibble::tibble(Pred_1 = predictor_variable[j]),.before = 0) %>% 
              tibble::add_column(tibble::tibble(Response = response_variable[i]),.before = 0)
            
            if (plot_interaction == TRUE) {
              if (stringr::str_detect(model_summary[['Interact_term_coef']],'[\\*\\+]+')) { # match significant or marginally significant
                model_plot = interaction_plot(model,return_plot = TRUE,print_plot = FALSE,verbose = FALSE,y_lim = y_lim)
                if (is.null(file_dir)) {stop('Please specificy the directory (i.e., path) to save the plot.')} 
                ggplot2::ggsave(plot = model_plot,filename = paste0(file_dir,model_summary[['Response']],' ~ ',model_summary[['Interact_term']],'.',device),width = width,device = device, height = height, units = units)
              }
            }
            if (print_control == FALSE) {
              model_summary = model_summary %>% dplyr::select(-dplyr::all_of(control_variable))
            }
            model_summary_final = model_summary_final %>% dplyr::bind_rows(model_summary)
          }
        }
        # for two-way interactions 
      } else if (length(two_way_interaction_variable)!=0) {
        for (k in 1:length(two_way_interaction_variable)) {
          two_way_interaction_terms = two_way_interaction_terms(c(predictor_variable[j],two_way_interaction_variable[k]))
          formula = paste(response_variable[i],'~',predictor_variable[j],'+',two_way_interaction_terms)
          if (length(control_variable) != 0) {
            formula = paste(formula,"+",paste(control_variable, collapse = " + "))
          }
          formula <- stats::as.formula(formula)
          model = stats::glm(formula = formula, data = data, family = family)
          model_summary = model %>%
            parameters::parameters() %>%
            tibble::as_tibble() %>%
            dplyr::select(dplyr::any_of(c('Parameter', 'Coefficient', 'p'))) %>%
            dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == predictor_variable[j],'Pred_coef',.data$Parameter)) %>%
            dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == two_way_interaction_variable[k],'Interact_pred_coef',.data$Parameter)) %>%
            dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == stringr::str_replace(two_way_interaction_terms,pattern = '\\*', replacement = ':'),'Interact_term_coef',.data$Parameter)) %>% 
            coefficent_to_p(marginal_alpha = marginal_alpha,show_p = show_p) %>%
            tidyr::pivot_wider(names_from = 'Parameter',values_from = 'Coefficient') %>% 
            dplyr::select('Interact_term_coef','Interact_pred_coef','Pred_coef',dplyr::everything()) %>% 
            tibble::add_column(tibble::tibble(df = format_round(insight::get_df(model),digits = 3))) %>%
            tibble::add_column(tibble::tibble(r2 = format_round(performance::r2(model)$R2,digits = 3))) %>% 
            tibble::add_column(tibble::tibble(Interact_term = two_way_interaction_terms),.before = 0) %>% 
            tibble::add_column(tibble::tibble(Interact_pred = two_way_interaction_variable[k]),.before = 0) %>% 
            tibble::add_column(tibble::tibble(Pred = predictor_variable[j]),.before = 0) %>% 
            tibble::add_column(tibble::tibble(Response = response_variable[i]),.before = 0)
          if (plot_interaction == TRUE) {
            if (stringr::str_detect(model_summary[['Interact_term_coef']],'[\\*\\+]+')) { # match significant or marginally significant
              model_plot = interaction_plot(model,return_plot = TRUE,print_plot = FALSE,verbose = FALSE,y_lim = y_lim)
              if (is.null(file_dir)) {stop('Please specificy the directory (i.e., path) to save the plot.')} 
              ggplot2::ggsave(plot = model_plot,filename = paste0(file_dir,model_summary[['Response']],' ~ ',model_summary[['Interact_term']],'.',device),width = width,device = device, height = height, units = units)
            }
          }
          if (print_control == FALSE) {
            model_summary = model_summary %>% dplyr::select(-dplyr::all_of(control_variable))
          }
          model_summary_final = model_summary_final %>% dplyr::bind_rows(model_summary)
        }
      } else{      
        # for non-interactions
        formula = paste(response_variable[i],'~',predictor_variable[j])
        if (length(control_variable) != 0) {
          formula = paste(formula,"+",paste(control_variable, collapse = " + "))
        }
        formula <- stats::as.formula(formula)
        model = stats::glm(formula = formula, data = data, family = family)
        model_summary = model %>%
          parameters::parameters() %>%
          tibble::as_tibble() %>%
          dplyr::select(dplyr::any_of(c('Parameter', 'Coefficient', 'p'))) %>%
          dplyr::mutate(Parameter = dplyr::if_else(.data$Parameter == predictor_variable[j],'Pred_coef',.data$Parameter)) %>%
          coefficent_to_p(marginal_alpha = marginal_alpha,show_p = show_p) %>%
          tidyr::pivot_wider(names_from = 'Parameter',values_from = 'Coefficient') %>% 
          dplyr::select('Pred_coef',dplyr::everything()) %>% 
          tibble::add_column(tibble::tibble(df = format_round(insight::get_df(model),digits = 3))) %>%
          tibble::add_column(tibble::tibble(r2 = format_round(performance::r2(model)$R2,digits = 3))) %>% 
          tibble::add_column(tibble::tibble(Pred = predictor_variable[j]),.before = 0) %>% 
          tibble::add_column(tibble::tibble(Response = response_variable[i]),.before = 0)
        if (print_control == FALSE) {
          model_summary = model_summary %>% dplyr::select(-dplyr::all_of(control_variable))
        }
        model_summary_final = model_summary_final %>% dplyr::bind_rows(model_summary)
      }
    }
    
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
