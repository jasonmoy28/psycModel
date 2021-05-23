#' Generalized Linear Regression
#'
#' `r lifecycle::badge("experimental")` \cr
#' Fit a generalized linear regression using `glm()`. This function is still in early development stage.
#'
#' @param data data frame
#' @param response_variable response variable. Support `dplyr::select()` syntax.
#' @param predictor_variable predictor variable. Support `dplyr::select()` syntax.
#' @param quite suppress printing output
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select()` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select()` syntax.
#' @param family a GLM family. It will passed to the family argument in glmer. See `?glmer` for possible options.
#'
#' @return an object class of `glm` representing the linear regression fit
#' @export
#'
#' @examples
#' fit <- glm_model(
#'   response_variable = incidence,
#'   predictor_variable = period,
#'   family = "poisson", # or you can enter as poisson(link = 'log'),
#'   data = lme4::cbpp
#' )
glm_model <- function(data,
                      response_variable,
                      predictor_variable,
                      two_way_interaction_factor = NULL,
                      three_way_interaction_factor = NULL,
                      family,
                      quite = FALSE) {
  glm_model_check <- function(object, method) {
    if (method == "response_variableiable_check") {
      if (length(object) != 1) {
        stop("Response variable must be length of 1")
      }
    }
    if (method == "three_way_interaction_factor_check") {
      if (length(three_way_interaction_factor) != 3) {
        stop("three_way_interaction_factor must have three factors")
      }
    }
  }

  ## parse tidyselect syntax
  response_variable <- data %>%
    dplyr::select(!!enquo(response_variable)) %>%
    names()
  predictor_variable <- data %>%
    dplyr::select(!!enquo(predictor_variable)) %>%
    names()
  two_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(two_way_interaction_factor)) %>%
    names()
  three_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(three_way_interaction_factor)) %>%
    names()
  predictor_variable <- predictor_variable[!predictor_variable %in% c(response_variable)]

  # data_check after parsing variable
  data <- data_check(data)

  ## remove response variable and id from random_effect_factors
  predictor_variable <- predictor_variable[!predictor_variable %in% c(response_variable)]
  two_way_interaction_factor <- two_way_interaction_factor[!two_way_interaction_factor %in% c(response_variable)]
  three_way_interaction_factor <- three_way_interaction_factor[!three_way_interaction_factor %in% c(response_variable)]

  if (length(two_way_interaction_factor) == 0) {
    two_way_interaction_factor <- NULL
  }
  if (length(three_way_interaction_factor) == 0) {
    three_way_interaction_factor <- NULL
  } else {
    glm_model_check(three_way_interaction_factor, method = "three_way_interaction_factor_check")
  }

  glm_model_check(response_variable, method = "response_variableiable_check")

  ####################################### Building Model #######################################
  two_way_interaction_terms <- NULL
  three_way_interaction_terms <- NULL
  # Check if interaction term exist, if so, add interaction terms to fixed factor
  if (!is.null(two_way_interaction_factor)) {
    two_way_interaction_terms <- two_way_interaction_terms(two_way_interaction_factor)
  }

  if (!is.null(three_way_interaction_factor)) {
    two_way_interaction_terms <- NULL
    three_way_interaction_terms <- paste(three_way_interaction_factor, collapse = "*")
  }

  predictor_variable <- c(predictor_variable, two_way_interaction_terms, three_way_interaction_terms)
  model <- paste(response_variable, "~", paste(predictor_variable, collapse = " + "))

  if (quite == FALSE) {
    cat(paste("Fitting Model with glm:\n Formula = ", model, "\n", sep = ""))
  }
  model <- stats::as.formula(model)

  glm_model <- stats::glm(formula = model, data = data, family = family)


  return(glm_model)
}
