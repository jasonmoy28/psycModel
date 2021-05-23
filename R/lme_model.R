#' Linear Mixed Effect Model
#'
#' `r lifecycle::badge("stable")` \cr
#' Fit a linear mixed effect model (i.e., hierarchical linear model, multilevel linear model) using the `nlme::lme()`  or the `lmerTest::lmer()` function.
#' Linear mixed effect model is used to explore the effect of continuous / categorical variables in predicting a normally distributed continuous variable.
#'
#'
#' @param data data frame
#' @param model  `lme4` model syntax. Support more complicated model. Note that model_summary will only return fixed effect estimates.
#' @param response_variable DV (i.e., outcome variable / response variable). Length of 1. Support `dplyr::select()` syntax.
#' @param random_effect_factors random effect factors (level-1 variable for HLM people) Factors that need to estimate fixed effect and random effect (i.e., random slope / varying slope based on the id). Support `dplyr::select()` syntax.
#' @param non_random_effect_factors non-random effect factors (level-2 variable for HLM people). Factors only need to estimate fixed effect. Support `dplyr::select()` syntax.
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select()` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select()` syntax.
#' @param id the nesting variable (e.g. group, time). Length of 1. Support `dplyr::select()` syntax.
#' @param estimation_method character. `ML` or `REML` default to `REML`.
#' @param na.action default is `stats::na.omit`. Another common option is `na.exclude`
#' @param opt_control default is `optim` for `lme` and `bobyqa` for `lmerTest`
#' @param use_package Default is `lmerTest`. Only available for linear mixed effect model. Options are `nlme`, `lmerTest`, or `lme4`(`'lme4` return similar result as `lmerTest` except the return model)
#' @param quite suppress printing output
#'
#' @details
#' Here is a little tip. If you are using generic selecting syntax (e.g., contains() or start_with()), you don't need to remove the response variable and the id from the factors. It will be automatically remove. For example, if you have x1:x9 as your factors. You want to regress x2:x8 on x1. Your probably pass something like response_variable = x1, random_effect_factors = c(contains('x'),-x1) to the function. However, you don't need to do that, you can just pass random_effect_factors = c(contains('x')) to the function since it will automatically remove the response variable from selection.
#'
#' @return an object representing the linear mixed-effects model fit (it maybe an object from `lme` or `lmer` depending of the package you use)
#'
#' @export
#'
#' @examples
#' # two-level model with level-1 and level-2 variable with random intercept and random slope
#' fit1 <- lme_model(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = c(extrav, sex),
#'   non_random_effect_factors = texp,
#'   id = class
#' )
#'
#'
#' # added two-way interaction factor
#' fit2 <- lme_model(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = c(extrav, sex),
#'   non_random_effect_factors = texp,
#'   two_way_interaction_factor = c(extrav, texp),
#'   id = class
#' )
#'
#' # pass a explicit lme model (I don't why you want to do that, but you can)
#' lme_fit <- lme_model(
#'   model = "popular ~ extrav*texp + (1 + extrav | class)",
#'   data = popular
#' )
lme_model <- function(data,
                      model = NULL,
                      response_variable,
                      random_effect_factors = NULL,
                      non_random_effect_factors = NULL,
                      two_way_interaction_factor = NULL,
                      three_way_interaction_factor = NULL,
                      id,
                      estimation_method = "REML",
                      opt_control = "bobyqa",
                      na.action = stats::na.omit,
                      use_package = "lmerTest",
                      quite = FALSE) {
  ########################################### Set up #############################################
  # check data type and covert all variable to numeric
  data <- data_check(data)
  lme_model_check <- function(object, method) {
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

  # opt_control check for lme4 and lmerTest to change default optimizer from optim to bobyqa
  opt_control_check <- function(opt_control) {
    if (opt_control == "bobyqa") {
      warning("The default optimizer is changed from bobyqa to optim for nlme package")
      opt_control <- "optim"
      return(opt_control)
    }
    return(opt_control)
  }



  ###################################### Modeling with Explict Model #############################################
  if (!is.null(model)) {
    if (use_package == "nlme") {
      warning("A model is specified explicitly. Switching to lmerTest for estimation.")
      use_package <- "lmerTest"
    }

    lmerformula <- stats::as.formula(model)
    lmerCtr <- lme4::lmerControl(optimizer = opt_control)

    if (use_package == "lmerTest") {
      model <- lmerTest::lmer(
        formula = lmerformula,
        data = data,
        na.action = na.action,
        control = lmerCtr
      )
    } else if (use_package == "lme4") {
      model <- lme4::lmer(
        formula = lmerformula,
        data = data,
        na.action = na.action,
        control = lmerCtr
      )
    }
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
    lme_model_check(two_way_interaction_factor, method = "two_interaction_factor_check")
  }
  if (length(three_way_interaction_factor) == 0) {
    three_way_interaction_factor <- NULL
  } else {
    lme_model_check(three_way_interaction_factor, method = "three_way_interaction_factor_check")
  }
  lme_model_check(response_variable, method = "response_variable_check")
  lme_model_check(id, method = "id_check")

  # Fixed factor include both level factor
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


  ###################################### Use nlme as the Package for Modeling #############################################
  if (use_package == "nlme") {
    # change the default optimzer to optim for nlme
    opt_control <- opt_control_check(opt_control)

    # Create the formula for fixed factor
    fixed_factors_formula <- stats::as.formula(paste(paste(response_variable, "~"), paste(fixed_factors, collapse = " + ")))
    # Created the formula for random factors
    random_factors_formula <- stats::as.formula(paste("~ 1 +", paste(random_factors, collapse = " + "), paste("|", id)))

    # print formula
    if (quite == FALSE) {
      fit_fixed_effect_formula <- paste(paste(response_variable, "~"), paste(fixed_factors, collapse = " + "))
      fit_random_effect_formula <- paste("~", paste(random_factors, collapse = " + "), paste("|", id))
      fit_formula <- paste("\n Fixed =", fit_fixed_effect_formula, "\n Random =", fit_random_effect_formula)
      cat(paste("Fitting Model with lme:", fit_formula, "\n"))
    }

    ctrl <- nlme::lmeControl(opt = opt_control)
    # Run lme model
    model <- do.call(getfun("nlme::lme"), list(
      fixed = fixed_factors_formula,
      random = random_factors_formula,
      data = quote(data),
      na.action = na.action,
      control = ctrl,
      method = estimation_method
    ))

    ###################################### Use lme4 or lmerTest as the Package for Modeling #############################################
  } else if (use_package == "lmerTest" | use_package == "lme4") {
    # Create the formula for fixed factor
    lmer_fixed_factors_formula <- paste(paste(response_variable, "~"), paste(fixed_factors, collapse = " + "))
    # Created the formula for random factors
    lmer_random_factors_formula <- paste(paste(random_factors, collapse = " + "), paste("|", id))
    lmerformula <- stats::as.formula(paste(lmer_fixed_factors_formula, " + (", lmer_random_factors_formula, ")", sep = ""))
    lmerCtr <- lme4::lmerControl(optimizer = opt_control)

    # Print fitting formula
    if (quite == FALSE) {
      fit_formula <- paste(lmer_fixed_factors_formula, " + (", lmer_random_factors_formula, ")", sep = "")
      cat(paste("Fitting Model with lmer:\n Formula = ", fit_formula, "\n", sep = ""))
    }
    if (use_package == "lmerTest") {
      # run lmerTest model
      model <- lmerTest::lmer(
        formula = lmerformula,
        data = data,
        na.action = na.action,
        control = lmerCtr
      )
    } else if (use_package == "lme4") {
      # run lme4 model
      model <- lme4::lmer(
        formula = lmerformula,
        data = data,
        na.action = na.action,
        control = lmerCtr
      )
    }
  }
  return(model)
}
