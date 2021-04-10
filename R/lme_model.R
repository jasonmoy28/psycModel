#' Linear Mixed Effect Model
#' 
#' `r lifecycle::badge("experimental")` \cr
#' Compute a linear mixed effect model (i.e., hierarchical linear model, multilevel linear model) using the `nlme::lme` (Pinheiro, 2006)  or the `lmerTest::lmer` (Kuznetsova, 2017) function.
#' 
#' 
#' @param data data frame
#' @param response_variable character or vector of length 1
#' @param level_1_factors vector. Level-1 variables (e.g., individual-level)
#' @param level_2_factors optional vector. level-2 variables (e.g., group-level)
#' @param two_way_interaction_factor optional vector of length more than 2. Default to `null`
#' @param three_way_interaction_factor optional vector of length 3. Do not include two-way interaction factors if this is not null. Default to `null`
#' @param id character or vector of length 1. The nesting variable (e.g. group)
#' @param estimation_method character. `ML` or `REML` default to `REML`.
#' @param na.action default to `stats::na.exclude`.
#' @param opt_control character. default to `optim` for `lme` and `bobyqa` for lmerTest
#' @param use_package character. `nlme` or `lmerTest`, or `lme4`. Default is `nlme`.
#' @param quite default to F. If set to `T`, it will not print the fitting model statement
#' 
#' @return An object of class `lme` of `lmerModLmerTest` representing the linear mixed-effects model fit.
#' @export
#'
#' @references
#'
#' Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. (2017). lmerTest package: tests in linear mixed effects models. Journal of statistical software, 82(13), 1-26.
#'
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' Pinheiro, J., Bates, D., DebRoy, S., Sarkar, D., & Team, R. C. (2006). nlme: Linear and nonlinear mixed effects models. R package version, 3(4), 109.
#'
#' @examples
#' lme_model(response_variable = 'Reaction',
#'            level_1_factors = 'Days',
#'            id = 'Subject',
#'            data = lme4::sleepstudy)
#'
#'
lme_model <- function(data,
                      response_variable,
                      level_1_factors,
                      level_2_factors = NULL,
                      two_way_interaction_factor = NULL,
                      three_way_interaction_factor = NULL,
                      id,
                      estimation_method = 'REML',
                      opt_control = 'optim',
                      na.action = stats::na.exclude,
                      use_package = 'nlme',
                      quite = F)
{
  data = data_check(data) #check data and coerced into numeric
  
  # Fixed factor include both level factor
  fixed_factors = c(level_1_factors, level_2_factors)

  # Random factor only include individual_level factor
  random_factors = level_1_factors

  two_way_interaction_terms = NULL
  three_way_interaction_terms = NULL
  # Check if interaction term exist, if so, add interaction terms to fixed factor
  if (!is.null(two_way_interaction_factor)) {
    two_way_interaction_terms = two_way_interaction_terms(two_way_interaction_factor)
  }

  if (!is.null(three_way_interaction_factor)) {
    two_way_interaction_terms = NULL
    three_way_interaction_terms = paste(three_way_interaction_factor,collapse = '*')
  }
  fixed_factors = c(fixed_factors,two_way_interaction_terms,three_way_interaction_terms)



  if (use_package == 'nlme') {
    # Create the formula for fixed factor
    fixed_factors_formula = stats::as.formula(paste(paste(response_variable, '~'), paste(fixed_factors, collapse = ' + ')))
    # Created the formula for random factors
    random_factors_formula = stats::as.formula(paste('~ 1 +', paste(random_factors, collapse = ' + '), paste('|',id)))

     if (quite == F) {
      fit_fixed_effect_formula = paste(paste(response_variable, '~'), paste(fixed_factors, collapse = ' + '))
      fit_random_effect_formula = paste('~ 1 +', paste(random_factors, collapse = ' + '), paste('|',id))
      fit_formula = paste('\n Fixed =',fit_fixed_effect_formula,'\n Random =',fit_random_effect_formula)
      cat(paste('Fitting Model with lme:',fit_formula,'\n'))

    }

    ctrl = nlme::lmeControl(opt=opt_control)
    # Run lme model
    getfun<-function(x) {
      if(length(grep("::", x))>0) {
        parts<-strsplit(x, "::")[[1]]
        getExportedValue(parts[1], parts[2])
      } else {
        x
      }
    }
    model = do.call(getfun("nlme::lme"), list(fixed = fixed_factors_formula,
                                              random = random_factors_formula,
                                              data = quote(data),
                                              na.action = na.action,
                                              control = ctrl,
                                              method = estimation_method))

  } else if (use_package == 'lmerTest' | use_package == 'lme4') {
    # change the default optimzer to bobyqa for lmerTest
    if (opt_control == 'optim') {
      opt_control = 'bobyqa'
    }
    # Create the formula for fixed factor
    lmer_fixed_factors_formula = paste(paste(response_variable, '~'), paste(fixed_factors, collapse = ' + '))
    # Created the formula for random factors
    lmer_random_factors_formula = paste('1 +', paste(random_factors, collapse = ' + '), paste('|',id))
    lmerformula = stats::as.formula(paste(lmer_fixed_factors_formula, ' + (', lmer_random_factors_formula,')',sep = ''))
    lmerCtr = lme4::lmerControl(optimizer = opt_control)

    # Print fitting formula
    if (quite == F) {
      fit_formula = paste(lmer_fixed_factors_formula, ' + (', lmer_random_factors_formula,')',sep = '')
      cat(paste('Fitting Model with lmer:\n Formula = ',fit_formula,'\n',sep = ''))
    }

    # Parse for do.call later
    getfun<-function(x) {
      if(length(grep("::", x))>0) {
        parts<-strsplit(x, "::")[[1]]
        getExportedValue(parts[1], parts[2])
      } else {
        x
      }
    }
    if (use_package == 'lmerTest') {
      model = do.call(getfun("lmerTest::lmer"), list(formula = lmerformula,
                                                     data = data,
                                                     na.action = na.action,
                                                     control = lmerCtr))
    } else if(use_package == 'lme4'){
      model = do.call(getfun("lme4::lmer"), list(formula = lmerformula,
                                                     data = data,
                                                     na.action = na.action,
                                                     control = lmerCtr))
    }

  }
  return(model)
}

