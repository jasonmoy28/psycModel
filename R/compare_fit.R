#' Comparison of Model Fit
#'
#' `r lifecycle::badge("experimental")` \cr
#' Compare the fit indices models (see below for model support)
#' 
#' @param ... model. If it is a lavaan object, it will try to perform measurement invariance. Other model type will passed to performance::compare_performance
#' @param digits number of digit to round
#'
#' @return
#' data frame with fit indices and change in fit indices
#' @export
#' @examples
#' # lme model
#'
#' fit1 <- lme_model(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = extrav,
#'   non_random_effect_factors = texp,
#'   id = class
#' )
#'
#' fit2 <- lme_model(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = extrav,
#'   non_random_effect_factors = texp,
#'   two_way_interaction_factor = c(extrav, texp),
#'   id = class
#' )
#'
#' compare_fit(fit1, fit2)
#'
#' # see ?measurement_invariance for measurement invariance example
compare_fit <- function(...,
                        digits = 3) {

  # lavaan models
  if (class(list(...)[[1]]) == 'lavaan') {
    models = list(...)
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
      dplyr::rename('$chi$^2' = 'chisq')
    return(return_df)

    ## lme & glme models
  } else {
    super_print('underline|Model Summary')
    super_print('Model Type = Model Comparison')
    cat('\n')
    output_table = performance::compare_performance(...)
    output_table = output_table %>% dplyr::select(-1)
    print_table(output_table)
  } 
}
