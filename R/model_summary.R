#' Model Summary for Mixed Effect Model
#'
#' `r lifecycle::badge("stable")` \cr
#' The function will extract the relevant coefficients from the linear mixed effect models (see supported model below).
#'
#' @param model an object from nlme::lme, lmerTest::lmer, or lme4::glmer
#' @param round number of digit to round the values.
#' @param streamlined_output Only print model estimate and model performance. Default is `FALSE`
#' @param return_result return the model estimates data frame. Default is `FALSE`
#' @param assumption_plot Generate an panel of plots that check major assumptions. You can use this if the model summary show violation of assumption (those maybe unreliable due to the use of p-value which is sensitive to the sample size). In the background, it calls performance::check_model()
#'
#' @references
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x
#'
#' @return If return_result is `TRUE`, it will return a data frame with estimate, df, p_value, and the p-value significance.
#'
#' @export
#'
#' @examples
#' # I am going to show the more generic usage of this function
#' # You can also use this package's built in function to fit the models
#' # I recommend using the model_summary_with_plot to get everything
#'
#' # lme example
#' lme_fit <- lme4::lmer("popular ~ extrav + sex + texp  + (1 | class)",
#'   data = popular
#' )
#'
#' model_summary(lme_fit, assumption_plot = TRUE)
#'
#' # lm example
#'
#' lm_fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
#'   data = iris
#' )
#'
#' model_summary(lm_fit, assumption_plot = TRUE)
model_summary <- function(model,
                          streamlined_output = F,
                          round = 3,
                          assumption_plot = F,
                          return_result = F) {

  ################################################ Linear Mixed Effect Model ################################################
  ## lme package
  if (class(model) == "lme") {
    model_type <- "Linear Mixed Effect Model (fitted using nlme)"
    predict_var <- as.character(attributes(model$terms)$variables)
    DV <- predict_var[2]
    IV <- predict_var[c(3:length(predict_var))]
    IV <- paste0(IV, collapse = ", ")

    # Assumptions check
    convergence_check <- F
    normality_check <- F
    outlier_check <- F
    autocorrelation_check <- F
    heteroscedasticity_check <- T
    collinearity_check <- T
    singular_check <- T

    summary <- as.data.frame(summary(model)[20])
    model_summary_df <- summary %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::select("variable", "tTable.Value", "tTable.DF", "tTable.p.value") %>%
      dplyr::mutate(estimate = .data$tTable.Value) %>%
      dplyr::mutate(DF = .data$tTable.DF) %>%
      dplyr::mutate(p_value = .data$tTable.p.value) %>%
      dplyr::mutate(dplyr::across(.data$estimate, ~ format(round(., round), nsmall = round))) %>%
      dplyr::mutate(dplyr::across(.data$DF, ~ round(., round))) %>%
      dplyr::mutate(sig. = dplyr::case_when(
        .data$p_value < 0.001 ~ "***",
        .data$p_value < 0.01 & .data$p_value >= 0.001 ~ "**",
        .data$p_value < 0.05 & .data$p_value >= 0.01 ~ "*",
        .data$p_value > 0.05 ~ ""
      )) %>%
      dplyr::mutate(dplyr::across(.data$p_value, ~ format(round(., round), nsmall = round))) %>%
      # must plaec below case_when
      dplyr::select("variable", "estimate", "DF", "p_value", "sig.")

    ## lmer package
  } else if (class(model) == "lmerModLmerTest" | class(model) == "lmerMod") {
    model_type <- "Linear Mixed Effect Model (fitted using lme4 or lmerTest)"
    formula_attribute <- stats::terms(model@call$formula)
    DV <- as.character(attributes(formula_attribute)$variables)[2]
    IV <- attributes(formula_attribute)$term.labels
    IV <- IV[!stringr::str_detect(IV, "\\+")]
    IV <- paste0(IV, collapse = ", ")

    # Assumptions check
    convergence_check <- T
    normality_check <- T
    outlier_check <- T
    autocorrelation_check <- T
    heteroscedasticity_check <- T
    collinearity_check <- T
    singular_check <- T

    if (class(model) == "lmerMod") {
      model <- lmerTest::as_lmerModLmerTest(model = model)
      warning("Degree of freedom and p-value is extracted from lmerTest")
    }

    summary <- as.data.frame(summary(model)$coefficients)
    model_summary_df <- summary %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::select("variable", "Estimate", "df", "Pr(>|t|)") %>%
      dplyr::mutate(DF = format(round(.data$df, round), nsmall = round)) %>%
      dplyr::mutate(estimate = format(round(.data$Estimate, round), nsmall = round)) %>%
      dplyr::mutate(p_value = .data$`Pr(>|t|)`) %>%
      dplyr::mutate(sig. = dplyr::case_when(
        .data$p_value <= 0.001 ~ "***",
        .data$p_value <= 0.01 & .data$p_value > 0.001 ~ "**",
        .data$p_value < 0.05 & .data$p_value > 0.01 ~ "*",
        .data$p_value > 0.05 ~ ""
      )) %>%
      dplyr::mutate(p_value = format(round(.data$`Pr(>|t|)`, round), nsmall = round)) %>%
      dplyr::select("variable", "estimate", "DF", "p_value", "sig.")

    ################################################ Generalized Linear Mixed Effect Model ################################################
    ## glmer model
    # } else if (class(model) == "glmerMod") {
    #   summary <- as.data.frame(summary(model)$coefficients)
    #   model_summary_df <- summary %>%
    #     tibble::rownames_to_column(var = "variable") %>%
    #     dplyr::select("variable", "Estimate", "z value", "Pr(>|z|)") %>%
    #     dplyr::mutate(z_value = format(round(.data$`z value`, round), nsmall = round)) %>%
    #     dplyr::mutate(estimate = format(round(.data$Estimate, round), nsmall = round)) %>%
    #     dplyr::mutate(p_value = .data$`Pr(>|z|)`) %>%
    #     dplyr::mutate(sig. = dplyr::case_when(
    #       .data$p_value <= 0.001 ~ "***",
    #       .data$p_value <= 0.01 & .data$p_value > 0.001 ~ "**",
    #       .data$p_value < 0.05 & .data$p_value > 0.01 ~ "*",
    #       .data$p_value > 0.05 ~ ""
    #     )) %>%
    #     dplyr::mutate(p_value = format(round(.data$`Pr(>|z|)`, round), nsmall = round)) %>%
    #     dplyr::select("variable", "estimate", "z_value", "p_value", "sig.")
    ################################################ Linear Regression  ################################################
  } else if (class(model) == "lm") {
    # Parameters for output table use
    model_type <- "Linear regression"
    predict_var <- as.character(attributes(model$terms)$predvars)
    DV <- predict_var[2]
    IV <- predict_var[c(3:length(predict_var))]
    IV <- paste0(IV, collapse = ", ")

    # Assumptions check
    convergence_check <- F
    normality_check <- T
    outlier_check <- T
    autocorrelation_check <- T
    heteroscedasticity_check <- T
    collinearity_check <- T
    singular_check <- F


    summary <- as.data.frame(summary(model)$coefficients)
    model_summary_df <- summary %>%
      tibble::rownames_to_column(var = "variable") %>%
      dplyr::select("variable", "Estimate", "t value", "Pr(>|t|)") %>%
      dplyr::mutate(t_value = format(round(.data$`t value`, round), nsmall = round)) %>%
      dplyr::mutate(estimate = format(round(.data$Estimate, round), nsmall = round)) %>%
      dplyr::mutate(p_value = .data$`Pr(>|t|)`) %>%
      dplyr::mutate(sig. = dplyr::case_when(
        .data$p_value <= 0.001 ~ "***",
        .data$p_value <= 0.01 & .data$p_value > 0.001 ~ "**",
        .data$p_value < 0.05 & .data$p_value > 0.01 ~ "*",
        .data$p_value > 0.05 ~ ""
      )) %>%
      dplyr::mutate(p_value = format(round(.data$`Pr(>|t|)`, round), nsmall = round)) %>%
      dplyr::select("variable", "estimate", "t_value", "p_value", "sig.")
  }
  else {
    stop("The function currently only support lme,lmerMod,lmerModLmerTest, glmerMod object. You can coerced the function to fit by specifying the model_fit argument.Be aware that result is not teseted.")
  }

  ################################################  Output Table  ################################################
  if (streamlined_output == T) {
    Print("<<underline Model Estimates>>")
    # Print model estimates table and model performance table
    print_table(model_summary_df)
    Print("\n \n \n")
    Print("<<underline Model Performance>>")
    print_table(performance::performance(model))
  } else {
    # Print header
    header <- "<<underline Model Summary>> \n Model Type = {model_type} \n Outcome = {DV} \n Predictors = {IV}"
    Print(header)
    Print("\n \n \n")
    # Print model estaimtes table
    Print("<<underline Model Estimates>>")
    print_table(model_summary_df)
    Print("\n \n \n")
    # Print model performance table
    Print("<<underline Model Performance>>")
    print_table(performance::performance(model))


    # Check assumption
    if (convergence_check == T) {
      convergence_output <- performance::check_convergence(model)
      if (convergence_output[[1]] == T) {
        Print("<<green OK: Model is converged>>")
      } else {
        gradient <- attributes(convergence_output)$gradient
        Print("<<red Warning: Model is not converged with gradient of {gradient}>>")
      }
    }
    if (singular_check == T) {
      singular_output <- performance::check_singularity(model)
      if (singular_output == T) {
        Print("<<red Warning: Singularity is detected>>")
      } else {
        Print("<<green OK: No singularity is detected>>")
      }
    }
    if (autocorrelation_check == T) {
      tryCatch(
        {
          performance::check_autocorrelation(model)
          Print("\n")
        },
        error = function(cond) {
          warning("Unable to check autocorrelation. Perhaps change na.action to na.omit")
        }
      )
    }
    if (normality_check == T) {
      performance::check_normality(model)
    }
    if (outlier_check == T) {
      tryCatch(print(performance::check_outliers(model)),
        error = function(cond) {
          warning("Unable to check autocorrelation. Perhaps change na.action to na.omit")
        }
      )
    }
    if (heteroscedasticity_check == T) {
      performance::check_heteroscedasticity(model)
    }
  }
  if (collinearity_check == T) {
    collinearity_df <- performance::check_collinearity(model)
    if (all(collinearity_df$VIF < 5)) {
      Print("<<green OK: No multicolinearity detected (VIF < 5)>>")
    } else if (any(collinearity_df$VIF >= 5 & collinearity_df$VIF < 10)) {
      Print("<<yellow Cautious: Moderated multicolinearity detected  (5 < VIF < 10). Please inspect the following table to identify factors.>>  ")
      Print("<<underline Multicollinearity Table >>")
      print_table(collinearity_df)
    } else if (any(collinearity_df$VIF > 10)) {
      Print("<<red Warning: Severe multicolinearity detected (VIF > 10). Please inspect the following table to identify factors.>>")
      Print("<<underline Multicollinearity Table >>")
      print_table(collinearity_df)
    }
  }

  # Check assumption plot
  if (assumption_plot == T) {
    suppressMessages(print(performance::check_model(model)))
  }
  if (return_result == T) {
    return(model_summary_df)
  }
}
