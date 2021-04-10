#' Model Summary for Mixed Effect Model
#' 
#' `r lifecycle::badge("stable")` \cr
#' The function will extract the relevant coefficients from the linear mixed effect models (see supported model below). 
#' 
#' @param model an object from nlme::lme, lmerTest::lmer, or lme4::glmer
#' @param model_performance vector. `R2_full_model` for conditional R2 and `R2_fixed_effect` for marginal R2 (Nakagawa, 2013). `icc` for intraclass correlation coefficient. The function calls the performance package for R2 and ICC (Lüdecke et al., 2020). 
#' @param estimate_round numeric. number of digit to round to for the slope estimate
#' @param p_value_round numeric. number of digit to round to for the p-value
#' @param performance_round numeric. number of digit to round to for the performance statistics (e.g., R2)
#' @param model_fit if you know your model produce same result as one of the object specify in the model argument, then you can try pass it to the function. Do it at your own risk.
#'
#' @references
#' 
#' Lüdecke, D., Makowski, D., Waggoner, P., Patil I (2020). performance: Assessment of Regression Models Performance. CRAN. doi: 10.5281/zenodo.3952174, R package, https://easystats.github.io/performance/.
#' 
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#' 
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x
#' 
#' @return a data frame with estimate, degree of freedom, p_value, and whether the p-value is significant
#' @export
#'
#' @examples
#' fit = lme_model(response_variable = 'Reaction',
#'                level_1_factors = 'Days',
#'                id = 'Subject',
#'                data = lme4::sleepstudy)
#'                
#' model_summary(fit)
#'
#'
model_summary <- function(model,
                          model_performance = c('R2_fixed_effect','R2_full_model'),
                          estimate_round = 3,
                          p_value_round = 3,
                          performance_round = 3,
                          model_fit = 'none') {

  # lme model
  if (class(model) == 'lme' | model_fit == 'lme') {
    summary =  as.data.frame(summary(model)[20])
    
    return_df = summary %>%
      tibble::rownames_to_column(var = 'variable') %>%
      dplyr::select('variable','tTable.Value','tTable.DF','tTable.p.value') %>% 
      dplyr::mutate(estimate = .data$tTable.Value) %>% 
      dplyr::mutate(DF = .data$tTable.DF) %>%
      dplyr::mutate(p_value = .data$tTable.p.value) %>%
      dplyr::mutate(dplyr::across(.data$estimate, ~ format(round(.,estimate_round),nsmall = estimate_round))) %>% 
      dplyr::mutate(dplyr::across(.data$DF, ~ round(.,estimate_round))) %>% 
      dplyr::mutate(significant = dplyr::case_when(.data$p_value < 0.001 ~ '***',
                                                   .data$p_value < 0.01 & .data$p_value >= 0.001 ~ '**',
                                                   .data$p_value < 0.05 & .data$p_value >= 0.01 ~ '*',
                                                   .data$p_value > 0.05 ~ '')) %>%
      dplyr::mutate(dplyr::across(.data$p_value, ~ format(round(.,p_value_round),nsmall = p_value_round))) %>% #must plaec below case_when
      dplyr::select('variable','estimate','DF', 'p_value','significant')

    # lmer model
  } else if (class(model) == 'lmerModLmerTest' | class(model) == 'lmerMod' | model_fit == 'lmer') {
    if (class(model) == 'lmerMod') {
      model = lmerTest::as_lmerModLmerTest(model = model)
      print('Degree of freedom and p-value is extracted from lmerTest')
    }

    summary =  as.data.frame(summary(model)$coefficients)
    return_df = summary %>%
      tibble::rownames_to_column(var = 'variable') %>%
      dplyr::select('variable','Estimate','df','Pr(>|t|)') %>%
      dplyr::mutate(DF = format(round(.data$df,estimate_round),nsmall = estimate_round)) %>%
      dplyr::mutate(estimate = format(round(.data$Estimate,estimate_round),nsmall = estimate_round)) %>%
      dplyr::mutate(p_value = .data$`Pr(>|t|)`) %>%
      dplyr::mutate(significant = dplyr::case_when(.data$p_value <= 0.001 ~ '***',
                                                   .data$p_value <= 0.01 & .data$p_value > 0.001 ~ '**',
                                                   .data$p_value < 0.05 & .data$p_value > 0.01 ~ '*',
                                                   .data$p_value > 0.05 ~ '')) %>%
      dplyr::mutate(p_value = format(round(.data$`Pr(>|t|)`,p_value_round),nsmall = p_value_round)) %>%
      dplyr::select('variable','estimate', 'DF', 'p_value','significant')

    # glmer model
  } else if(class(model) == 'glmerMod' | model_fit == 'glmer'){
    summary = as.data.frame(summary(model)$coefficients)
    return_df = summary %>%
      tibble::rownames_to_column(var = 'variable') %>%
      dplyr::select('variable','Estimate','z value','Pr(>|z|)') %>%
      dplyr::mutate(z_value = format(round(.data$`z value`,estimate_round),nsmall = estimate_round)) %>%
      dplyr::mutate(estimate = format(round(.data$Estimate,estimate_round),nsmall = estimate_round)) %>%
      dplyr::mutate(p_value = .data$`Pr(>|z|)`) %>%
      dplyr::mutate(significant = dplyr::case_when(.data$p_value <= 0.001 ~ '***',
                                                   .data$p_value <= 0.01 & .data$p_value > 0.001 ~ '**',
                                                   .data$p_value < 0.05 & .data$p_value > 0.01 ~ '*',
                                                   .data$p_value > 0.05 ~ '')) %>%
      dplyr::mutate(p_value = format(round(.data$`Pr(>|z|)`,p_value_round),nsmall = p_value_round)) %>%
      dplyr::select('variable','estimate', 'z_value', 'p_value','significant')

  } else {
    print('The function currently only support lme,lmerMod,lmerModLmerTest, glmerMod object. You can coerced the function to fit by specifying the model_fit argument.Be aware that result is not teseted.')
  }

  if (class(model) == 'lme' | model_fit == 'lme' | class(model) == 'lmerModLmerTest' | class(model) == 'lmerMod' | model_fit == 'lmer') {
    # Caculate the effect size
    R2_conditional_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)
    R2_marginal_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)
    icc_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)

    if (any(model_performance %in% 'R2_full_model')) {
      R2_conditional = as.numeric(performance::r2(model)[1][['R2_conditional']])
      R2_conditional_df = data.frame(variable = 'R2_full_model', estimate = format(round(R2_conditional,performance_round),nsmall = performance_round), DF = '', p_value = '',significant = '')
    }
    if (any(model_performance %in% 'R2_fixed_effect')) {
      R2_marginal = as.numeric(performance::r2(model)[2][['R2_marginal']])
      R2_marginal_df = data.frame(variable = 'R2_fixed_effect', estimate= format(round(R2_marginal,performance_round),nsmall = performance_round), DF = '', p_value = '',significant = '')
    }
    if (any(model_performance %in% 'icc')) {
      icc = as.numeric(performance::icc(model)[1])
      icc_df = data.frame(variable = 'ICC_Adjusted', estimate= format(round(icc,performance_round),nsmall = performance_round), DF = '', p_value = '',significant = '')
    }
    return_df = rbind(return_df,R2_conditional_df,R2_marginal_df,icc_df)
  } else if (class(model) == 'glmerMod' | model_fit == 'glmer'){
    # Caculate the effect size
    R2_conditional_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)
    R2_marginal_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)
    icc_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)

    if (any(model_performance %in% 'R2_full_model')) {
      R2_conditional = as.numeric(performance::r2(model)[1][['R2_conditional']])
      R2_conditional_df = data.frame(variable = 'R2_full_model', estimate= format(round(R2_conditional,performance_round),nsmall = performance_round), z_value = '', p_value = '',significant = '')
    }
    if (any(model_performance %in% 'R2_fixed_effect')) {
      R2_marginal = as.numeric(performance::r2(model)[2][['R2_marginal']])
      R2_marginal_df = data.frame(variable = 'R2_fixed_effect', estimate= format(round(R2_marginal,performance_round),nsmall = performance_round), z_value = '', p_value = '',significant = '')
    }
    if (any(model_performance %in% 'icc')) {
      icc = as.numeric(performance::icc(model)[1])
      icc_df = data.frame(variable = 'ICC_Adjusted', estimate= format(round(icc,performance_round),nsmall = performance_round), z_value = '', p_value = '',significant = '')
    }

    return_df = rbind(return_df,R2_conditional_df,R2_marginal_df,icc_df)
  }

  return(return_df)
}
