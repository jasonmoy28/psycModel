#' Generalized Linear Mixed Effect Model
#'
#' `r lifecycle::badge("experimental")` \cr
#' Generalized linear mixed effect model. The function uses the `lme4::glmer`(Bates et al., 2014) function. This function is still in very early development stage.
#'
#' @param data data frame
#' @param response_variable character or vector of length 1
#' @param random_effect_factors vector. Level-1 variables (e.g., individual-level)
#' @param non_random_effect_factors optional vector. level-2 variables (e.g., group-level)
#' @param family a GLM family. It will passed to the family argument in glmer. See `?glmer` for possible options.
#' @param two_way_interaction_factor optional vector of length more than 2. Default to `null`
#' @param three_way_interaction_factor optional vector of length 3. Do not include two-way interaction factors if this is not null. Default to `null`
#' @param id character or vector of length 1. The nesting variable (e.g. group)
#' @param estimation_method character. `ML` or `REML` default to `REML`.
#' @param na.action default to `stats::na.exclude`.
#' @param opt_control character. default to `bobyqa`
#' @param quite default to F. If set to `T`, it will not print the fitting model statement
#'
#' @references
#' Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1–48. doi: 10.18637/jss.v067.i01.
#'
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' @return An object of class "glmerMod" representing the linear mixed-effects model fit.
#' @export
#'
#' @examples
#' x <- rnorm(200)
#' y <- rpois(200, exp(1 + x))
#' group <- rep(1:10, 20)
#' test_df <- as.data.frame(cbind(x, y, group))
#' fit <- glme_model(
#'   data = test_df,
#'   response_variable = "y",
#'   random_effect_factors = "x",
#'   family = poisson(link = "log"),
#'   id = "group"
#' )
glme_model <- function(data,
                       response_variable,
                       random_effect_factors,
                       non_random_effect_factors = NULL,
                       family,
                       two_way_interaction_factor = NULL,
                       three_way_interaction_factor = NULL,
                       id,
                       estimation_method = "REML",
                       opt_control = "bobyqa",
                       na.action = stats::na.exclude,
                       quite = F) {
  data <- data_check(data) # check data and coerced into numeric

  # Fixed factor inlcude both level factor
  fixed_factors <- c(random_effect_factors, non_random_effect_factors)

  # Random factor only include individual_level factor
  random_factors <- random_effect_factors

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
  glmer_random_factors_formula <- paste("1 +", paste(random_factors, collapse = " + "), paste("|", id))
  glmerformula <- stats::as.formula(paste(glmer_fixed_factors_formula, " + (", glmer_random_factors_formula, ")", sep = ""))
  glmerCtr <- lme4::glmerControl(optimizer = opt_control)

  if (quite == F) {
    fit_formula <- paste(glmer_fixed_factors_formula, " + (", glmer_random_factors_formula, ")", sep = "")
    cat(paste("Fitting Model with glmer: \n Formula = ", fit_formula, "\n Family = ", family[1], "\n", sep = ""))
  }
  getfun <- function(x) {
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }

  if (any(family %in% "negbin")) {
    stop("Sorry, we do not support negative binomial distribution yet.")
    # library(lme4) # need to figure out why it doesn't work
    # model = do.call(getfun("lme4::glmer.nb"), list(formula = glmerformula,
    #                                                data = data,
    #                                                na.action = na.action,
    #                                                control = glmerCtr))
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
