#' Linear Regressions / ANOVA / ANCOVA
#'
#' `r lifecycle::badge("stable")` \cr
#' Fit a linear regression using `lm()`. Linear regression is used to explore the effect of continuous variables / categorical variables in predicting a normally-distributed continuous variables.
#' If you are using a categorical predictor to predict a continuous variable, some may call it a ANOVA / ANCOVA while it is just a special form of linear regression).
#' In this package, I will not build separate function for ANOVA & ANCOVA since they are the same as linear regression
#'
#' @param data data frame
#' @param response_variable response variable. Support `dplyr::select()` syntax.
#' @param predictor_variable predictor variable. Support `dplyr::select()` syntax. It will automatically remove the response variable from predictor variable, so you can use `contains()` or `start_with()` safely.
#' @param quite suppress printing output
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select()` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select()` syntax.
#'
#' @return an object class of `lm` representing the linear regression fit
#' @export
#'
#' @examples
#' fit <- lm_model(
#'   data = iris,
#'   response_variable = "Sepal.Length",
#'   predictor_variable = tidyselect::everything(),
#'   two_way_interaction_factor = c(Sepal.Width, Species)
#' )
lm_model <- function(data,
                     response_variable,
                     predictor_variable,
                     two_way_interaction_factor = NULL,
                     three_way_interaction_factor = NULL,
                     quite = FALSE) {
  data <- data_check(data)
  getfun <- function(x) {
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }

  lm_model_check <- function(object, method) {
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
    lm_model_check(three_way_interaction_factor, method = "three_way_interaction_factor_check")
  }

  lm_model_check(response_variable, method = "response_variableiable_check")

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
    cat(paste("Fitting Model with lm:\n Formula = ", model, "\n", sep = ""))
  }
  model <- stats::as.formula(model)

  lm_model <- stats::lm(formula = model, data = data)

  return(lm_model)
}
