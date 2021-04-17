#' Model Summary for Mixed Effect Model
#'
#' `r lifecycle::badge("stable")` \cr
#' The function will extract the relevant coefficients from the linear mixed effect models (see supported model below).
#'
#' @param model an object from nlme::lme, lmerTest::lmer, or lme4::glmer
#' @param round number of digit to round the values.
#' @param streamlined_output Only super_print model estimate and model performance. Default is `FALSE`
#' @param return_result return the model estimates data frame. Default is `FALSE`
#' @param assumption_plot Generate an panel of plots that check major assumptions. You can use this if the model summary show violation of assumption (those maybe unreliable due to the use of p-value which is sensitive to the sample size). In the background, it calls performance::check_model()
#' @param quite suppress printing output
#'
#' @references
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x
#'
#' @return If return_result is `TRUE`, it will return a data frame with estimate, df, p_value, and the p-value significance.
#'
#' @export
#' @details
#' If you working with `lm`, fixed factor estimate is just the model estimate. It
#' @examples
#' # I am going to show the more generic usage of this function
#' # You can also use this package's built in function to fit the models
#' # I recommend using the model_summary_with_plot to get everything
#'
#' # lme example
#' lme_fit <- lme4::lmer("popular ~ texp  + (1 | class)",
#'   data = popular
#' )
#'
#' model_summary(lme_fit) 
#' 
#' # lm example
#'
#' lm_fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
#'   data = iris
#' )
#'
#' model_summary(lm_fit, assumption_plot = TRUE)
model_summary <- function(model,
                          streamlined_output = FALSE,
                          round = 3,
                          assumption_plot = FALSE,
                          return_result = FALSE,
                          quite = FALSE) {

  ################################################ Linear Mixed Effect Model ################################################
  ## lme package
  if (class(model) == "lme") {
    model_type <- "Linear Mixed Effect Model (fitted using nlme)"
    predict_var <- as.character(attributes(model$terms)$variables)
    DV <- predict_var[2]
    IV <- predict_var[c(3:length(predict_var))]
    IV <- paste0(IV, collapse = ", ")

    # Assumptions check
    convergence_check <- FALSE
    normality_check <- FALSE
    outlier_check <- FALSE
    autocorrelation_check <- FALSE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- TRUE

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
    convergence_check <- TRUE
    normality_check <- TRUE
    outlier_check <- TRUE
    autocorrelation_check <- TRUE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- TRUE


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
    convergence_check <- FALSE
    normality_check <- TRUE
    outlier_check <- TRUE
    autocorrelation_check <- TRUE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- FALSE

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
  if (quite == FALSE) { # check whether quite the entire output table
    if (streamlined_output == TRUE) { # streamline model output
      super_print("underline|Model Estimates")
      # super_print model estimates table and model performance table
      print_table(model_summary_df)
      super_print("\n \n \n")
      super_print("underline|Model Performance")
      model_performance_df <- performance::performance(model)
      colnames(model_performance_df) <- stringr::str_replace_all(pattern = "R2", replacement = "R^2", string = colnames(model_performance_df))
      colnames(model_performance_df) <- stringr::str_replace_all(pattern = "Sigma", replacement = "$sigma$", string = colnames(model_performance_df))
      print_table(model_performance_df)
    } else { # full model output
      super_print("underline|Model Summary")
      super_print("Model Type = {model_type}")
      super_print("Outcome = {DV}")
      super_print("Predictors = {IV}")
      super_print("\n \n \n")
      # super_print model estimates table
      super_print("underline|Model Estimates")
      print_table(model_summary_df)
      super_print("\n \n \n")
      # super_print model performance table
      super_print("underline|Model Performance")
      model_performance_df <- performance::performance(model)
      colnames(model_performance_df) <- stringr::str_replace_all(pattern = "R2", replacement = "R^2", string = colnames(model_performance_df))
      colnames(model_performance_df) <- stringr::str_replace_all(pattern = "Sigma", replacement = "$sigma$", string = colnames(model_performance_df))
      print_table(model_performance_df)

      # Check assumption
      super_print("\n \n \n")
      super_print("underline|Model Assumption Check")
      super_print("\n")
      if (convergence_check == TRUE) {
        convergence_output <- performance::check_convergence(model)
        if (convergence_output[[1]] == TRUE) {
          super_print("green|OK: Model is converged")
        } else {
          gradient <- attributes(convergence_output)$gradient
          super_print("red|Warning: Model is not converged with gradient of {gradient}")
        }
      }

      if (singular_check == TRUE) {
        singular_output <- performance::check_singularity(model)
        if (singular_output == TRUE) {
          super_print("red|Warning: Singularity is detected. See ?lme4::isSingular()")
        } else {
          super_print("green|OK: No singularity is detected")
        }
      }

      if (autocorrelation_check == TRUE) {
        tryCatch(
          {
            performance::check_autocorrelation(model)
            super_print("\n")
          },
          error = function(cond) {
            warning("Unable to check autocorrelation. Perhaps change na.action to na.omit")
          }
        )
      }

      if (normality_check == TRUE) { # first check_normality, if failed, fallback to check_distribution, if failed, super_print failed message
        tryCatch(suppressMessages(performance::check_normality(model)),
          error = function(cond) {
            tryCatch(
              {
                # fall back to check_distribution
                dist_prob <- performance::check_distribution(model)
                norm_dist_pos <- which(dist_prob$Distribution == "normal")
                residual_norm_prob <- round(dist_prob$p_Residuals[norm_dist_pos] * 100, 0)
                response_norm_prob <- round(dist_prob$p_Response[norm_dist_pos] * 100, 0)
                norm_prob <- c(residual_norm_prob, response_norm_prob)
                if (all(norm_prob >= 80)) {
                  residual_norm_prob <- paste(norm_prob[1], "%", sep = "")
                  response_norm_prob <- paste(norm_prob[2], "%", sep = "")
                  super_print("green|OK. No non-normality is detected. Normal distribution proability: residual ({residual_norm_prob}) and response ({response_norm_prob}). check_normality() failed use fallback")
                } else if (any(norm_prob < 80) & all(norm_prob > 50)) {
                  residual_norm_prob <- paste(norm_prob[1], "%", sep = "")
                  response_norm_prob <- paste(norm_prob[2], "%", sep = "")
                  super_print("yellow|Cautious: Moderate non-normality is detected. Normal distribution proability: residual ({residual_norm_prob}) and  response ({response_norm_prob}). check_normality() failed use fallback")
                } else if (any(norm_prob <= 50)) {
                  residual_norm_prob <- paste(norm_prob[1], "%", sep = "")
                  response_norm_prob <- paste(norm_prob[2], "%", sep = "")
                  super_print("red|Warning: Severe non-normality is detected. Normal distribution proability: residual ({residual_norm_prob}) and  response ({response_norm_prob}). check_normality() failed use fallback")
                }
              },
              error = function(cond) {
                super_print("blue|Unable to check normality. All fallback failed.")
              }
            )
          }
        )
      }
    }

    if (outlier_check == TRUE) {
      tryCatch(super_print(performance::check_outliers(model)),
        error = function(cond) {
          super_print("blue|Unable to check autocorrelation. Try changing na.action to na.omit.")
        }
      )
    }

    if (heteroscedasticity_check == TRUE) {
      try(performance::check_heteroscedasticity(model))
    }

    if (collinearity_check == TRUE) {
      collinearity_df <- performance::check_collinearity(model)
      if (all(collinearity_df$VIF < 5)) {
        super_print("green|OK: No multicolinearity detected (VIF < 5)")
      } else if (any(collinearity_df$VIF >= 5) & all(collinearity_df$VIF < 10)) {
        super_print("yellow|Cautious: Moderate multicolinearity detected  (5 < VIF < 10). Please inspect the following table to identify high correlation factors.")
        super_print("underline|Multicollinearity Table ")
        print_table(collinearity_df)
      } else if (any(collinearity_df$VIF > 10)) {
        super_print("red|Warning: Severe multicolinearity detected (VIF > 10). Please inspect the following table to identify high correlation factors.")
        super_print("underline|Multicollinearity Table ")
        print_table(collinearity_df)
      }
    }
  } # quite stop here


  # Check assumption plot
  if (assumption_plot == TRUE) {
    if (all(unlist(lapply(c("gridExtra", "qqplotr", "see"), requireNamespace)))) {
      suppressMessages(print(performance::check_model(model)))
    } else {
      stop("please install.packages(c('gridExtra','qqplotr','see')) to use assumption_plot")
    }
  }
  cat("\n")
  if (return_result == TRUE) {
    return(model_summary_df)
  }
}
