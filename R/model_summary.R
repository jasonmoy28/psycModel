#' Model Summary for Regression Models
#'
#' `r lifecycle::badge("stable")` \cr
#' The function will extract the relevant coefficients from the regression models (see below for supported model).
#'
#' @param model an model object. The following model are tested for accuracy: `lm`, `glm`, `lme`, `lmer`, `glmer`. Other model object may work if it work with parameters::model_parameters()
#' @param digits number of digits to round to
#' @param streamline print streamlined output. Only print model estimate and performance.
#' @param return_result It set to `TRUE`, it return the model estimates data frame.
#' @param assumption_plot Generate an panel of plots that check major assumptions. It is usually recommended to inspect model assumption violation visually. In the background, it calls `performance::check_model()`.
#' @param quite suppress printing output
#'
#' @references
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x
#'
#' @return a list of model estimate data frame, model performance data frame, and the assumption plot (an `ggplot` object)
#'
#' @export
#' @examples
#' # I am going to show the more generic usage of this function
#' # You can also use this package's built in function to fit the models
#' # I recommend using the integrated_multilevel_model_summary to get everything
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
                          digits = 3,
                          assumption_plot = FALSE,
                          quite = FALSE,
                          streamline = FALSE,
                          return_result = FALSE) {

  ################################################ Linear Mixed Effect Model ################################################
  ## lme package
  if (class(model)[1] == "lme") {
    model_type <- "Linear Mixed Effect Model (fitted using nlme)"
    predict_var <- as.character(attributes(model$terms)$variables)
    DV <- predict_var[2]
    IV <- predict_var[c(3:length(predict_var))]
    IV <- paste0(IV, collapse = ", ")
    family <- NULL

    # Assumptions check
    convergence_check <- FALSE
    normality_check <- FALSE
    outlier_check <- FALSE
    autocorrelation_check <- FALSE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- TRUE

    lme_param <- parameters::model_parameters(model)
    model_summary_df <- lme_param %>%
      as.data.frame() %>%
      dplyr::rename(df = .data$df_error) %>%
      dplyr::rename(ci.lower = .data$CI_low) %>%
      dplyr::rename(ci.upper = .data$CI_high) %>%
      dplyr::select(-"CI") %>%
      dplyr::select("Parameter", "Effects", "Coefficient", "t", "df", "SE", "p", "ci.lower", "ci.upper", "p", tidyselect::everything())

    ## lmer package
  } else if (class(model)[1] == "lmerModLmerTest" | class(model)[1] == "lmerMod") {
    model_type <- "Linear Mixed Effect Model (fitted using lme4 or lmerTest)"
    formula_attribute <- stats::terms(model@call$formula)
    DV <- as.character(attributes(formula_attribute)$variables)[2]
    IV <- attributes(formula_attribute)$term.labels
    IV <- IV[!stringr::str_detect(IV, "\\+")]
    IV <- paste0(IV, collapse = ", ")
    family <- NULL
    # Assumptions check
    convergence_check <- TRUE
    normality_check <- TRUE
    outlier_check <- TRUE
    autocorrelation_check <- TRUE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- TRUE

    lme_param <- parameters::model_parameters(model)
    model_summary_df <- lme_param %>%
      as.data.frame() %>%
      dplyr::rename(df = .data$df_error) %>%
      dplyr::rename(ci.lower = .data$CI_low) %>%
      dplyr::rename(ci.upper = .data$CI_high) %>%
      dplyr::select(-"CI") %>%
      dplyr::select("Parameter", "Effects", "Coefficient", "t", "df", "SE", "p", "ci.lower", "ci.upper", "p", tidyselect::everything())

    ################################################ Generalized Linear Mixed Effect Model ################################################
    # glmer model
  } else if (class(model)[1] == "glmerMod") {
    model_type <- "Generalized Linear Mixed Effect Model"
    formula_attribute <- stats::terms(model@call$formula)
    DV <- as.character(attributes(formula_attribute)$variables)[2]
    IV <- attributes(formula_attribute)$term.labels
    IV <- IV[!stringr::str_detect(IV, "\\+")]
    IV <- paste0(IV, collapse = ", ")
    family <- model@call$family

    # Assumptions check
    convergence_check <- TRUE
    normality_check <- FALSE
    outlier_check <- FALSE
    autocorrelation_check <- TRUE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- TRUE

    glme_param <- parameters::parameters(model)
    model_summary_df <- glme_param %>%
      as.data.frame() %>%
      dplyr::rename(df = .data$df_error) %>%
      dplyr::rename(ci.lower = .data$CI_low) %>%
      dplyr::rename(ci.upper = .data$CI_high) %>%
      dplyr::select(-"CI") %>%
      dplyr::select("Parameter", "Effects", "Coefficient", "z", "df", "SE", "p", "ci.lower", "ci.upper", "p", tidyselect::everything())

    ################################################ Linear Regression  ################################################
  } else if (class(model)[1] == "lm") { # linear regression
    # Parameters for output table use
    model_type <- "Linear regression"
    predict_var <- as.character(attributes(model$terms)$predvars)
    DV <- predict_var[2]
    IV <- predict_var[c(3:length(predict_var))]
    IV <- paste0(IV, collapse = ", ")
    family <- NULL

    # Assumptions check
    convergence_check <- FALSE
    normality_check <- TRUE
    outlier_check <- TRUE
    autocorrelation_check <- TRUE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- FALSE

    lm_param <- parameters::parameters(model)
    model_summary_df <- lm_param %>%
      as.data.frame() %>%
      dplyr::rename(df = .data$df_error) %>%
      dplyr::rename(ci.lower = .data$CI_low) %>%
      dplyr::rename(ci.upper = .data$CI_high) %>%
      dplyr::select(-"CI") %>%
      dplyr::select("Parameter", "Coefficient", "t", "df", "SE", "p", "ci.lower", "ci.upper", "p", tidyselect::everything())
  } else if (class(model)[1] == "glm") { # glm
    model_type <- "Generazlied Linear regression"
    predict_var <- as.character(attributes(model$terms)$predvars)
    DV <- predict_var[2]
    IV <- predict_var[c(3:length(predict_var))]
    IV <- paste0(IV, collapse = ", ")
    family <- as.character(model$family)[1]

    convergence_check <- FALSE
    normality_check <- FALSE
    outlier_check <- FALSE
    autocorrelation_check <- TRUE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- FALSE


    glm_param <- parameters::parameters(model)
    model_summary_df <- glm_param %>%
      as.data.frame() %>%
      dplyr::rename(df = .data$df_error) %>%
      dplyr::rename(ci.lower = .data$CI_low) %>%
      dplyr::rename(ci.upper = .data$CI_high) %>%
      dplyr::select(-"CI") %>%
      dplyr::select("Parameter", "Coefficient", "z", "SE", "df", tidyselect::everything())
  } else {
    model_type <- "Unable to Determined for Unknown Model"
    DV <- "Unable to Determined for Unknown Model"
    IV <- "Unable to Determined for Unknown Model"

    convergence_check <- TRUE
    normality_check <- TRUE
    outlier_check <- TRUE
    autocorrelation_check <- TRUE
    heteroscedasticity_check <- TRUE
    collinearity_check <- TRUE
    singular_check <- TRUE


    other_param <- parameters::parameters(model)
    model_summary_df <- other_param %>%
      as.data.frame() %>%
      dplyr::rename(df = .data$df_error) %>%
      dplyr::rename(ci.lower = .data$CI_low) %>%
      dplyr::rename(ci.upper = .data$CI_high) %>%
      dplyr::select(-"CI")

    warning("This model is not formally supported. Please proceed with cautious. The model is passed to parameters::parameters() to extract relevant parameters")
  }

  performance_warning <- utils::capture.output(model_performance_df <- performance::model_performance(model))
  if (length(performance_warning) > 0) {
    warning(performance_warning)
  }
  colnames(model_performance_df) <- stringr::str_replace_all(pattern = "R2", replacement = "R^2", string = colnames(model_performance_df))
  colnames(model_performance_df) <- stringr::str_replace_all(pattern = "Sigma", replacement = "$sigma$", string = colnames(model_performance_df))
  ################################################  Output Table  ################################################
  if (quite == FALSE) { # check whether quite the entire output table
    if (streamline == FALSE) {
      cat("\n \n")
      super_print("underline|Model Summary")
      super_print("Model Type = {model_type}")
      super_print("Outcome = {DV}")
      super_print("Predictors = {IV}")
      if (!is.null(family)) {
        super_print("Family = {family}")
      }
      super_print("\n")
    }

    # super_print model estimates table
    super_print("underline|Model Estimates")
    print_table(model_summary_df)
    super_print("\n")
    # super_print model performance table
    super_print("underline|Goodness of Fit")
    print_table(model_performance_df)
    if (streamline == FALSE) {
      # Check assumption
      super_print("\n")
      super_print("underline|Model Assumption Check")
      if (convergence_check == TRUE) {
        try({
          convergence_output <- performance::check_convergence(model)
          if (convergence_output[[1]] == TRUE) {
            super_print("green|OK: Model is converged")
          } else {
            gradient <- attributes(convergence_output)$gradient
            super_print("red|Warning: Model is not converged with gradient of {gradient}")
          }
        })
      }

      if (singular_check == TRUE) {
        try({
          singular_output <- performance::check_singularity(model)
          if (singular_output == TRUE) {
            super_print("red|Warning: Singularity is detected. See ?lme4::isSingular()")
          } else {
            super_print("green|OK: No singularity is detected")
          }
        })
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
        try({
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
        })
      }
    }
  } # quite stop here


  # Check assumption plot
  if (assumption_plot == TRUE) {
    if (all(unlist(lapply(c("gridExtra", "qqplotr", "see"), requireNamespace)))) {
      tryCatch(
        {
          assumption_plot <- suppressMessages(performance::check_model(model))
          print(assumption_plot)
        },
        error = function(cond) {
          warning("assumption_plot does not support this model type")
          warning(cond)
        }
      )
    } else {
      stop("Please install.packages(c('gridExtra','qqplotr','see')) to use assumption_plot")
    }
  } else {
    assumption_plot <- NULL
  }
  cat("\n")
  if (return_result == TRUE) {
    return_list <- list(
      model_summary = model_summary_df,
      model_performance_df = model_performance_df,
      assumption_plot = assumption_plot
    )

    return(return_list)
  }
}
