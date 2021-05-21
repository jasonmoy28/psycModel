#' Comparison of Model Fit
#'
#' `r lifecycle::badge("stable")` \cr
#' Compare the fit indices of models (see below for model support)
#'
#' @param ... model. If it is a `lavaan` object, it will try to compute the measurement invariance. Other model types will be passed to `performance::compare_performance()`.
#' @param digits number of digits to round to
#' @param streamline print streamlined output
#' @param quite suppress printing output
#' @param return_result  If it is set to `TRUE`, it will return the the compare fit data frame.
#'
#' @return data frame with fit indices and change in fit indices
#' @export
#' @examples
#' # lme model
#'
#' fit1 <- lm_model(
#'   data = popular,
#'   response_variable = popular,
#'   predictor_var = c(sex, extrav)
#' )
#'
#' fit2 <- lm_model(
#'   data = popular,
#'   response_variable = popular,
#'   predictor_var = c(sex, extrav),
#'   two_way_interaction_factor = c(sex, extrav)
#' )
#'
#' compare_fit(fit1, fit2)
#'
#' # see ?measurement_invariance for measurement invariance example
compare_fit <- function(...,
                        digits = 3,
                        quite = FALSE,
                        streamline = FALSE,
                        return_result = FALSE) {

  # lavaan models
  if (class(list(...)[[1]]) == "lavaan") {
    models <- list(...)
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
      dplyr::mutate(dplyr::across(tidyselect::everything(), ~ format(round(., digits = digits), nsmall = digits)))
    compare_fit_df <- compare_fit_df %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(), ~ format(round(., digits = digits), nsmall = digits)))

    return_df <-
      rbind(fit_indices_df, blank_df, compare_fit_df) %>%
      dplyr::rename("$chi$^2" = "chisq")

    return(return_df)

    ## lme & glme models
  } else {
    output_table <- "compare_fit is temporialy disable due to unknown error caused by insight upgrade from 0.13.2 to 0.14.0. Follow instruction on the package load message to get back all the features."
    if (quite == FALSE) {
      if (streamline == FALSE) {
        super_print("underline|Model Summary")
        super_print("Model Type = Model Comparison")
        cat("\n")
      }
      # output_table <- output_table %>% dplyr::select(-1)
      print_table(output_table)
    }
    if (return_result == TRUE) {
      return(output_table)
    }
  }
}
