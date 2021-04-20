#' Generalized Linear Mixed Effect Model
#'
#' `r lifecycle::badge("experimental")` \cr
#' Fit a generalized linear mixed effect model using `lme4::glmer()`. This function is still in early development stage.
#'
#' @param data data frame
#' @param response_variable DV (i.e., outcome variable / response variable). Length of 1. Support `dplyr::select()` syntax.
#' @param random_effect_factors random effect factors (level-1 variable for HLM people) Factors that need to estimate fixed effect and random effect (i.e., random slope / varying slope based on the id). Support `dplyr::select()` syntax.
#' @param non_random_effect_factors non-random effect factors (level-2 variable for HLM people). Factors only need to estimate fixed effect. Support `dplyr::select()` syntax.
#' @param family a GLM family. It will passed to the family argument in glmer. See `?glmer` for possible options.
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select()` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select()` syntax.
#' @param id the nesting variable (e.g. group, time). Length of 1. Support `dplyr::select()` syntax.
#' @param estimation_method character. `ML` or `REML` default to `REML`.
#' @param na.action default is `stats::na.omit`. Another common option is `na.exclude`
#' @param opt_control character. default is `bobyqa`. See `?lme4::glmerControl` for more options. 
#' @param quite suppress printing output
#' @param model `lme4` model syntax. Support more complicated model. Note that model_summary will only return fixed effect estimates. This is not tested. `r lifecycle::badge("experimental")`
#'
#' @return An object of class `glmerMod` representing the linear mixed-effects model fit.
#' @export
#' @examples
#' fit <- glme_model(
#'   response_variable = incidence,
#'   random_effect_factors = period,
#'   family = "poisson", # or you can enter as poisson(link = 'log')
#'   id = herd,
#'   data = lme4::cbpp
#' )
glme_model <- function(data,
                       model = NULL,
                       response_variable,
                       random_effect_factors = NULL,
                       non_random_effect_factors = NULL,
                       family,
                       two_way_interaction_factor = NULL,
                       three_way_interaction_factor = NULL,
                       id,
                       estimation_method = "REML",
                       opt_control = "bobyqa",
                       na.action = stats::na.omit,
                       quite = FALSE) {
  ########################################## Set up #############################################
  data <- data_check(data) # check data and coerced into numeric
  glme_model_check <- function(object, method) {
    if (method == "response_variable_check") {
      if (length(object) != 1) {
        stop("Response variable must be length of 1")
      }
    }
    if (method == "id_check") {
      if (length(object) != 1) {
        stop("ID must be length of 1")
      }
    }
    if (method == "three_way_interaction_factor_check") {
      if (length(object) != 3) {
        stop("three_way_interaction_factor must have three factors")
      }
    }
    if (method == "two_interaction_factor_check") {
      if (length(object) < 2) {
        stop("two_way_interaction_factor must have three factors")
      }
    }
  }
  # run a getfun function that is essentially for do.call() later
  getfun <- function(x) {
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }

  ###################################### Modeling with Explict Model #############################################
  if (!is.null(model)) {
    glmerformula <- stats::as.formula(model)
    glmerCtr <- lme4::glmerControl(optimizer = opt_control)

    model <- do.call(getfun("lme4::glmer"), list(
      formula = glmerformula,
      data = data,
      na.action = na.action,
      control = glmerCtr
    ))

    return(model)
  }
  ###################################### Build model for models without explicit model #############################################
  ## parse tidyselect syntax
  response_variable <- data %>%
    dplyr::select(!!enquo(response_variable)) %>%
    names()
  random_effect_factors <- data %>%
    dplyr::select(!!enquo(random_effect_factors)) %>%
    names()
  non_random_effect_factors <- data %>%
    dplyr::select(!!enquo(non_random_effect_factors)) %>%
    names()
  two_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(two_way_interaction_factor)) %>%
    names()
  three_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(three_way_interaction_factor)) %>%
    names()
  id <- data %>%
    dplyr::select(!!enquo(id)) %>%
    names()

  ## remove response variable and id from all other variables
  random_effect_factors <- random_effect_factors[!random_effect_factors %in% c(response_variable, id)]
  non_random_effect_factors <- non_random_effect_factors[!non_random_effect_factors %in% c(response_variable, id)]
  two_way_interaction_factor <- two_way_interaction_factor[!two_way_interaction_factor %in% c(response_variable, id)]
  three_way_interaction_factor <- three_way_interaction_factor[!three_way_interaction_factor %in% c(response_variable, id)]

  # Check variable length & assign NULL to variables that is NULL
  if (length(non_random_effect_factors) == 0) {
    non_random_effect_factors <- NULL
  }
  if (length(two_way_interaction_factor) == 0) {
    two_way_interaction_factor <- NULL
  } else {
    glme_model_check(two_way_interaction_factor, method = "two_interaction_factor_check")
  }
  if (length(three_way_interaction_factor) == 0) {
    three_way_interaction_factor <- NULL
  } else {
    glme_model_check(three_way_interaction_factor, method = "three_way_interaction_factor_check")
  }
  glme_model_check(response_variable, method = "response_variable_check")
  glme_model_check(id, method = "id_check")


  # Fixed factor inlcude both level factor
  fixed_factors <- c(random_effect_factors, non_random_effect_factors)

  # Random factor only include individual_level factor
  random_factors <- c(1, random_effect_factors)

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
  fixed_factors <- c(fixed_factors, two_way_interaction_terms, three_way_interaction_terms)

  # Create the formula for fixed factor
  glmer_fixed_factors_formula <- paste(paste(response_variable, "~"), paste(fixed_factors, collapse = " + "))
  # Created the formula for random factors
  glmer_random_factors_formula <- paste(paste(random_factors, collapse = " + "), paste("|", id))
  glmerformula <- stats::as.formula(paste(glmer_fixed_factors_formula, " + (", glmer_random_factors_formula, ")", sep = ""))
  glmerCtr <- lme4::glmerControl(optimizer = opt_control)

  if (quite == FALSE) {
    fit_formula <- paste(glmer_fixed_factors_formula, " + (", glmer_random_factors_formula, ")", sep = "")
    cat(paste("Fitting Model with glmer: \n Formula = ", fit_formula, "\n Family = ", family[1], "\n", sep = ""))
  }

  if (any(family %in% "negbin")) {
    stop("Sorry, we do not support negative binomial distribution yet.") # do not support negative binomial now
  } else {
    model <- do.call(getfun("lme4::glmer"), list(
      formula = glmerformula,
      data = data,
      family = family,
      na.action = na.action,
      control = glmerCtr
    ))
  }

  return(model)
}
