#' Mediation Analysis
#'
#' `r lifecycle::badge("experimental")` \cr
#' It currently only support simple mediation analysis using the path analysis approach with the `lavaan` package. I am trying to implement multilevel mediation in `lavaan`.
#' In the future, I will try supporting moderated mediation (through `lavaan` or `mediation`) and mediation with latent variable (through `lavaan`).
#'
#' @param data data frame
#' @param response_variable response variable. Support `dplyr::select()` syntax.
#' @param mediator mediator. Support `dplyr::select()` syntax.
#' @param predictor_variable predictor variable. Support `dplyr::select()` syntax.
#' @param control_variable control variables / covariate. Support `dplyr::select()` syntax.
#' @param standardize standardized coefficients. Default is `TRUE`
#' @param digits number of digits to round to
#' @param return_result If it is set to `TRUE`, it will return the `lavaan` object
#' @param group nesting variable for multilevel mediation. Not confident about the implementation method. `r lifecycle::badge("experimental")`
#' @param streamline print streamlined output
#' @param quite suppress printing output
#'
#' @return an object from `lavaan`
#' @export
#'
#' @examples
#' mediation_summary(
#'   data = lmerTest::carrots,
#'   response_variable = Preference,
#'   mediator = Sweetness,
#'   predictor_variable = Crisp
#' )
mediation_summary <- function(data,
                              response_variable,
                              mediator,
                              predictor_variable,
                              control_variable = NULL,
                              group = NULL,
                              standardize = TRUE,
                              digits = 3,
                              quite = FALSE,
                              streamline = FALSE,
                              return_result = FALSE) {
  data <- data_check(data)
  response_variable <- data %>%
    dplyr::select(!!enquo(response_variable)) %>%
    names()
  mediator <- data %>%
    dplyr::select(!!enquo(mediator)) %>%
    names()
  predictor_variable <- data %>%
    dplyr::select(!!enquo(predictor_variable)) %>%
    names()
  control_variable <- data %>%
    dplyr::select(!!enquo(control_variable)) %>%
    names()
  group <- data %>%
    dplyr::select(!!enquo(group)) %>%
    names()
  
  med_reg_formula <- paste0(mediator, " ~ ", "a*", predictor_variable)
  response_reg_formula <- paste(c(paste0(response_variable, " ~ ", "b*", mediator, " + ", "c*", predictor_variable), control_variable), collapse = " + ")
  lavaan_effect <- "direct := c\nindirect := a*b\ntotal := c + (a*b)"
  mediation_model <- paste(med_reg_formula, response_reg_formula, lavaan_effect, sep = "\n")
  
  mediation_result <- lavaan::sem(model = mediation_model, data = data, group = group)
  mediation_param <- parameters::model_parameters(mediation_result, standardize = standardize)
  
  # Cleaning up the output from parameters::model_parameters
  mediation_output <- mediation_param %>%
    tibble::as_tibble() %>%
    dplyr::select("Label", tidyselect::everything()) %>%
    dplyr::rename(Est = .data$Coefficient) %>%
    dplyr::rename(ci.lower = .data$CI_low) %>%
    dplyr::rename(ci.upper = .data$CI_high) %>%
    dplyr::select(-c("Label"))
  
  if (standardize == TRUE) {
    mediation_output <- mediation_output %>% dplyr::rename(Est.Std = .data$Est)
  }
  
  mediation_effect_output <- mediation_output %>%
    dplyr::filter(.data$Component == "Defined") %>%
    dplyr::rename(`Effect Type` = .data$To) %>%
    dplyr::select(-c("From", "Component", "Operator"))
  
  mediation_reg_output <- mediation_output %>%
    dplyr::filter(.data$Component == "Regression") %>%
    dplyr::rename("Response" = .data$To) %>%
    dplyr::rename("Predict" = .data$From) %>%
    dplyr::select(-"Component")
  
  ########################################## Output ###############################################
  if (quite == FALSE) {
    if (streamline == FALSE) {
      super_print("underline|Model Summary")
      super_print("Model Type = Mediation Analysis (fitted using lavaan)")
    }
    cat("\n")
    super_print("underline|Effect Summary")
    print_table(mediation_effect_output, digits = digits)
    cat("\n")
    super_print("underline|Regression Summary")
    print_table(mediation_reg_output, digits = digits)
  }
  
  if (return_result == TRUE) {
    return(mediation_result)
  }
}
