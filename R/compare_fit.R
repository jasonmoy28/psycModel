#' Comparison of Model Fit
#'
#' `r lifecycle::badge("experimental")` \cr
#' Compare the fit indices models (see below for model support)
#'
#' @param models list of object from `lavaan` (for measurement invariance only at the moment) or lme, lmer models.
#' @param digits number of digit to round
#'
#' @return
#' data frame with fit indices and change in fit indices
#' @export
#' @examples
#' # lme model
#' fit1 <- lme_model(
#'   response_variable = JS_Individual,
#'   random_effect_factors = Age_Individual,
#'   non_random_effect_factors = Hofstede_IC_Country,
#'   id = Country,
#'   data = EWCS_2015_shorten,
#' )
#'
#' fit2 <- lme_model(
#'   response_variable = JS_Individual,
#'   random_effect_factors = Age_Individual,
#'   non_random_effect_factors = c(Hofstede_IC_Country, Hofstede_UA_Country),
#'   id = Country,
#'   data = EWCS_2015_shorten,
#' )
#'
#' compare_fit(list(fit1, fit2))
#'
#' # see ?measurement_invariance for measurement invariance example
compare_fit <- function(models,
                        digits = 3) {
  if (class(models) != "list") {
    stop(paste("models must be list. Considering wrapping the objects in a list"))
  }

  # lavaan models
  if (all(sapply(models, class) %in% "lavaan")) {
    blank_df <- tibble::tibble(chisq = "", df = "", pvalue = "", cfi = "", rmsea = "", srmr = "", tli = "", aic = "", bic = "", bic2 = "", rowname = ".") %>% tibble::column_to_rownames()
    return_df <- tibble::tibble(chisq = NULL, df = NULL, pvalue = NULL, cfi = NULL, rmsea = NULL, srmr = NULL, tli = NULL, aic = NULL, bic = NULL, bic2 = NULL)
    fit_indices_df <- tibble::tibble(chisq = NULL, df = NULL, pvalue = NULL, cfi = NULL, rmsea = NULL, srmr = NULL, tli = NULL, aic = NULL, bic = NULL, bic2 = NULL)
    model_name <- c("configural", "metric", "scalar")
    i <- 0
    for (model in models) {
      i <- i + 1
      fit_measure <- lavaan::fitmeasures(model)
      fit_indices <- c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr", "tli", "aic", "bic", "bic2")
      fit_indices_loop_df <- as.data.frame(fit_measure[fit_indices]) %>%
        tibble::rownames_to_column() %>%
        tidyr::pivot_wider(names_from = "rowname", values_from = "fit_measure[fit_indices]") %>%
        dplyr::mutate(model_name = model_name[i]) %>%
        tibble::column_to_rownames(var = "model_name")
      fit_indices_df <- rbind(fit_indices_df, fit_indices_loop_df)
    }
    if (nrow(fit_indices_df) == 2) { # config and metric model
      config_metric <- fit_indices_df[2, ] - fit_indices_df[1, ] %>% as.data.frame()
      rownames(config_metric) <- "metric - config"
      compare_fit_df <- config_metric
    } else if (nrow(fit_indices_df) == 3) {
      config_metric <- fit_indices_df[2, ] - fit_indices_df[1, ] %>% as.data.frame()
      metric_scalar <- fit_indices_df[3, ] - fit_indices_df[2, ] %>% as.data.frame()
      rownames(config_metric) <- "metric - config"
      rownames(metric_scalar) <- "scalar - metric"
      compare_fit_df <- rbind(config_metric, metric_scalar)
    }

    fit_indices_df <- fit_indices_df %>%
      dplyr::mutate(dplyr::across(tidyr::everything(), ~ format(round(., digits = digits), nsmall = digits)))
    compare_fit_df <- compare_fit_df %>%
      dplyr::mutate(dplyr::across(tidyr::everything(), ~ format(round(., digits = digits), nsmall = digits)))

    return_df <-
      rbind(fit_indices_df, blank_df, compare_fit_df)
    return(return_df)

    ## lme & glme models
  } else if (all(sapply(models, class) %in% c("lme", "lmerModLmerTest", "glmerMod", "lmerMod"))) {
    return_df <- tibble::tibble(AIC = NULL, BIC = NULL, R2_conditional = NULL, R2_marginal = NULL, ICC = NULL, RMSE = NULL, Sigma = NULL)
    for (model in models) {
      model_performance_df <- tibble::as_tibble(performance::model_performance(model)) %>%
        dplyr::rename(R2_full_model = .data$R2_conditional) %>%
        dplyr::rename(R2_fixed_effect = .data$R2_marginal)
      return_df <- rbind(return_df, model_performance_df)
    }
    message("R2_full_model is formally known as R2 conditional, R2_fixed_effect is formally known as R2 marginal")
    return_df <- return_df %>%
      dplyr::mutate(dplyr::across(tidyr::everything(), ~ format(round(., digits = digits), nsmall = digits)))
    return(return_df)
  } else {
    stop(paste("Object class", class(models[[1]]), "is not supported"))
  }
}
