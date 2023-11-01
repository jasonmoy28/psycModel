#' Linear Regression Model Table
#' Generate tables with multiple response and predictor variable (only `lm` models are supported)
#'
#' @param data `data.frame`
#' @param response_variable response variable. Support `dplyr::select()` syntax.
#' @param predictor_variable predictor variable. Support `dplyr::select()` syntax. It will automatically remove the response variable from predictor variable, so you can use `contains()` or `start_with()` safely.
#' @param control_variable control variables. Support `dplyr::select()` syntax.
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
#' 
#' lm_model_table(data = iris, 
#'             response_variable = c(Sepal.Length,Sepal.Width),
#'             predictor_variable = Petal.Width)

lm_model_table = function(data, 
                          response_variable,
                          predictor_variable,
                          control_variable = NULL,
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
  
  # Multiple response variables
  if (length(response_variable) > 1) {
    for (i in 1:length(response_variable)) {
      model = lm_model(data = data,
                       response_variable = dplyr::all_of(response_variable[i]),
                       predictor_variable = dplyr::all_of(c(predictor_variable,control_variable)),
                       quite = TRUE)
      # Non-full model
      # if (full_model == FALSE) {
      #   model_summary = model %>%
      #     parameters::parameters() %>%
      #     tibble::as_tibble() %>%
      #     dplyr::filter(.data$Parameter == predictor_variable) %>%
      #     dplyr::mutate('IV' = predictor_variable) %>%
      #     dplyr::mutate('DV' = response_variable[i]) %>%
      #     dplyr::select(dplyr::any_of(c('DV','IV','Coefficient', 'p')))
      #
      #   if (i == 1) {
      #     model_summary_final = model_summary
      #   } else{
      #     model_summary_final = rbind(model_summary_final,model_summary)
      #   }
      #   # Full model
      # } else{
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
    #}

  }
  # Multiple predictor variables
  if (length(predictor_variable) > 1) {
    model_summary_final = tibble::tibble()
    for (i in 1:length(predictor_variable)) {
      model = lm_model(data = data,
                       response_variable = dplyr::all_of(response_variable),
                       predictor_variable = dplyr::all_of(c(predictor_variable[i],control_variable)),
                       quite = TRUE)
      # if (full_model == FALSE) {
      #   model_summary = model %>%
      #     parameters::parameters() %>%
      #     tibble::as_tibble() %>%
      #     dplyr::filter(.data$Parameter == predictor_variable[i]) %>%
      #     dplyr::mutate('IV' = predictor_variable[i]) %>%
      #     dplyr::mutate('DV' = response_variable) %>%
      #     dplyr::select(dplyr::any_of(c('DV','IV','Coefficient', 'p')))
      #
      #   if (i == 1) {
      #   model_summary_final = model_summary
      #   } else{
      #     model_summary_final = rbind(model_summary_final,model_summary)
      #   }
      #
      #} else{

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
      #}
    }
  }
  if (verbose == TRUE) {
    print_table(model_summary_final,marginal_alpha = marginal_alpha)
    if (show_p == TRUE) {
      super_print(paste('Note: Coefficient (p-value): . p < ',marginal_alpha,', * p < 0.05, ** p < 0.01, *** p < 0.001',sep = ''))
    } else{
      super_print(paste('Note: . < ',marginal_alpha,', * p < 0.05, ** p < 0.01, *** p < 0.001',sep = ''))
    }
  }
  if (return_result) {
    return(model_summary_final)
  }
}
