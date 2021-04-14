#' Measurement Invariance
#'
#' `r lifecycle::badge("stable")` \cr
#' Compute the measurement invariance model (i.e., measurement equivalence model) using multi-group confirmatory factor analysis (MGCFA; Jöreskog, 1971). This function uses the lavaan::cfa() in the backend.
#' Users can run the configural-metric or the configural-metric-scalar comparisons (see below for detail instruction).
#' All arguments (except the CFA items) must be explicitly named (like model = your-model; see example for inappropriate behavior).
#'
#' @param data data frame
#' @param ... CFA items. Multi-factor CFA items should be separated by comma (as different argument). See below for example
#' @param model explicit lavaan model. Either the `model` argument or the `items` argument must be specified.
#' @param group character. group variable.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param group_partial items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2'). See details for recommended practice. 
#' @param invariance_level "metric" or "scalar". Default is 'metric'. Set as 'metric' for configural-metric comparison, and set as 'scalar' for configural-metric-scalar comparison.
#' @param digits number of digit to round
#' @param return_result Default is `FALSE`. If it is `TRUE`, it will return a data frame of the fit measure summary
#' @param quite suppress all printing except the model summary. Default is `FALSE`.
#'
#' @details
#' Chen (2007) suggested that ΔCFI <= |-0.010| supplemented by RMSEA <= 0.015 indicate non-invariance when sample sizes were equal across groups and larger than 300 in each group (Chen, 2007). 
#' And, Chen (2007) suggested that ΔCFI <= |-0.005| and ΔRMSEA <= 0.010 for unequal sample size with each group smaller than 300. For SRMR, Chen (2007) recommend ΔSRMR < 0.030 for metric-invariance and ΔSRMR < 0.015 for scalar-invariance. 
#' For large group size, Rutowski & Svetina (2014) recommended a more liberal cut-off for metric non-invariance for CFI (ΔCFI <= |-0.020|) and RMSEA (RMSEA <= 0.030). However, this more liberal cut-off DOES NOT apply to testing scalar non-invariance.
#' If measurement-invariance is not achived, some researchers suggesting partial invariance is acceptable (by releasing the contraints on some factors). For example, Steenkamp and Baumgartner (1998) suggested that ideally more than half of items on a factor should be invariant. However, 
#' it is important to note that no empirical studies were cited to support the partial invariance guideline (Putnick & Bornstein, 2016). 
#' 
#' 
#' @references
#' Chen, F. F. (2007). Sensitivity of Goodness of Fit Indexes to Lack of Measurement Invariance. Structural Equation Modeling: A Multidisciplinary Journal, 14(3), 464–504. https://doi.org/10.1080/10705510701301834
#' 
#' Jöreskog, K. G. (1971). Simultaneous factor analysis in several populations. Psychometrika, 36(4), 409-426.
#'
#' Putnick, D. L., & Bornstein, M. H. (2016). Measurement Invariance Conventions and Reporting: The State of the Art and Future Directions for Psychological Research. Developmental Review : DR, 41, 71–90. https://doi.org/10.1016/j.dr.2016.06.004
#' 
#' Rutkowski, L., & Svetina, D. (2014). Assessing the Hypothesis of Measurement Invariance in the Context of Large-Scale International Surveys. Educational and Psychological Measurement, 74(1), 31–57. https://doi.org/10.1177/0013164413498257
#' 
#' Steenkamp, J.-B. E. M., & Baumgartner, H. (n.d.). Assessing Measurement Invariance in Cross-National Consumer Research. JOURNAL OF CONSUMER RESEARCH, 13.
#' 
#' 
#'
#' @export
#' @return
#' return the a data frame of the fit measure summary
#'
#' @examples
#' # REMEMBER, YOU MUST NAMED ALL ARGUMENT EXCEPT THE CFA ITEMS ARGUMENT
#' # Fitting a multiple-factor measurement invariance model by passing items.
#' measurement_invariance(
#'   x1:x3,
#'   x4:x6,
#'   x7:x9,
#'   data = lavaan::HolzingerSwineford1939,
#'   group = "school"
#' )
#'
#' # Fitting measurement invariance model by passing explicit lavaan model
#' # I am also going to only test for metric invariance instead of the default scalar invariance
#' measurement_invariance(
#'   model = "visual  =~ x1 + x2 + x3;
#'            textual =~ x4 + x5 + x6;
#'            speed   =~ x7 + x8 + x9",
#'   data = lavaan::HolzingerSwineford1939,
#'   group = "school",
#'   invariance_level = "metric"
#' )
#' \dontrun{
#' # This will fail because I did not add `model = ` in front of the lavaan model.
#' # Therefore,you must add the tag in front of all arguments
#' # For example, `return_result = 'model'` instaed of `model`
#' measurement_invariance(
#'   "visual  =~ x1 + x2 + x3;
#'              textual =~ x4 + x5 + x6;
#'              speed   =~ x7 + x8 + x9",
#'   data = lavaan::HolzingerSwineford1939
#' )
#' }
#'
measurement_invariance <- function(data,
                                   ...,
                                   model = NULL,
                                   group,
                                   ordered = F,
                                   group_partial = NULL,
                                   return_result = F,
                                   invariance_level = "scalar",
                                   digits = 3,
                                   quite = F) {
  if (is.null(model)) { # construct model if explicit model is not passed
    items <- enquos(...)
    model <- ""
    index <- 1
    for (item in items) {
      cfa_items <- data %>%
        dplyr::select(!!item) %>%
        names()
      factor_name <- paste("DV", index, sep = "")
      loop_model <- paste(factor_name, " =~ ", paste(cfa_items, collapse = " + "), "\n ", sep = "")
      model <- paste(model, loop_model)
      index <- index + 1
    }
  }
  group <- data %>%
    dplyr::select(!!enquo(group)) %>%
    names()

  # Print statement
  if (quite == F) {
    cat("Computing CFA using:\n", model)
  }

  if (invariance_level == "metric") {
    if (quite == F) {
      print("Computing for configural model")
    }

    config_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      ordered = ordered,
      group.partial = group_partial
    )

    if (quite == F) {
      print("Computing for metric model")
    }

    metric_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = "loadings",
      ordered = ordered,
      group.partial = group_partial
    )

    fit <- compare_fit(list(config_model, metric_model), digits = digits)
  } else if (invariance_level == "scalar") {
    if (quite == F) {
      print("Computing for configural model")
    }
    config_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      ordered = ordered,
      group.partial = group_partial
    )

    if (quite == F) {
      print("Computing for metric model")
    }
    metric_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = "loadings",
      ordered = ordered,
      group.partial = group_partial
    )

    if (quite == F) {
      print("Computing for scalar model")
    }
    scalar_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = c("loadings", "intercepts"),
      ordered = ordered,
      group.partial = group_partial
    )
    fit <- compare_fit(list(config_model, metric_model, scalar_model), digits = digits)
  } else {
    print("Error: Invariance level must be either metric or scalar")
    return()
  }

  if (invariance_level == "metric") {
    invariance_level_print <- "Configural-Metric Comparsion"
  } else if (invariance_level == "scalar") {
    invariance_level_print <- "Configural-Metric-Scalar Comparsion"
  }
  fit = fit %>% dplyr::rename(p = .data$pvalue)
  
  colnames(fit) <- stringr::str_to_upper(colnames(fit))
  
  Print("\n \n \n")
  Print("<<underline Model Summary>>")
  Print("Model Type = Measurement Invariance")
  Print("Comparsion Type = {invariance_level_print}")
  Print("Group = {group}")
  Print("Model Formula = \n .{model}")
  Print("\n \n \n")
  Print("<<underline Fit Measure Summary>>")
  print_table(fit)
  Print("Goodness of Fit:")
  fit = fit %>% dplyr::mutate(dplyr::across(tidyselect::everything(), as.numeric))
  CFI <- fit['metric - config',"CFI"]
  
  # metric invariance 
  if (abs(CFI) <= 0.005) {
    Print("<<green OK. Excellent measurement metric-invariance based on |delta CFI| < 0.005>>")
  } else if (abs(CFI) <= 0.01) {
    Print("<<green OK. Good measurement metric-invariance based on |delta CFI| < 0.01>>")
  } else if (abs(CFI) > 0.01 & abs(CFI) < 0.02) {
    Print("<<red Warning. Unacceptable measurement metric-invariance based on |delta CFI| > 0.01. Potentially acceptable with large number of group. In this case, the recommend cut-off is |delta CFI| <0.02 (metric-invariance only). See ?measurement_invariance detail section.>>")
  } else if (abs(CFI) >= 0.02) {
    Print("<<red Warning. Unacceptable measurement metric-invariance based on |delta CFI| > 0.01>>")
  }
  
  RMSEA <- fit['metric - config',"RMSEA"]
  if (all(abs(RMSEA) <= 0.01)) {
    Print("<<green OK. Excellent measurement metric-invariance based on |delta RMSEA| < 0.01>>")
  } else if (abs(RMSEA) > 0.01 & abs(RMSEA) < 0.015) {
    Print("<<yellow Cautious. Acceptable measurement metric-invariance based on 0.015 > |delta RMSEA| > 0.01>>")
  } else if (abs(RMSEA) >= 0.015 &  abs(RMSEA) < 0.030) {
    Print("<<red Warning. Unacceptable measurement metric-invariance based on |delta RMSEA| > 0.015. Potentially acceptable with large number of group. In this case, the recommend cut-off is |delta RMSEA| < 0.30  (metric-invariance only). See ?measurement_invariance detail section.w>>")
  } else if (abs(RMSEA) >= 0.030) {
    Print("<<red Warning. Unacceptable measurement metric-invariance based on |delta RMSEA| > 0.015>>")
  }
  
  SRMR <- fit['metric - config',"SRMR"]
  if (all(abs(SRMR) <= 0.03)) {
    Print("<<green OK. Good measurement metric-invariance based on delta SRMR < 0.03>>")
  } else if (any(abs(SRMR) > 0.03)) {
    Print("<<red Warning. Poor measurement metric-invariance based on delta SRMR > 0.03>>")
  }
  
  # scalar invariance 
  if (invariance_level == 'scalar') {
    CFI <- fit['scalar - metric',"CFI"]
    if (abs(CFI) <= 0.01) {
      Print("<<green OK. Good measurement scalar-invariance based on |delta CFI| < 0.01>>")
    } else if (abs(CFI) > 0.01) {
      Print("<<red Warning. Unacceptable measurement scalar-invariance based on |delta CFI| > 0.01>>")
    } 
    
    RMSEA <- fit['scalar - metric',"RMSEA"]
    if (abs(RMSEA) <= 0.01) {
      Print("<<green OK. Excellent measurement scalar-invariance based on |delta RMSEA| < 0.015>>")
    } else if (abs(RMSEA) > 0.01 & abs(RMSEA) < 0.015) {
      Print("<<yellow Cautious. Acceptable measurement scalar-invariance based on 0.015 > |delta RMSEA| > 0.01.>>")
    } else if (abs(RMSEA) >= 0.015) {
      Print("<<red Warning. Unacceptable measurement scalar-invariance based on |delta RMSEA| > 0.015.>>")
    }
    
    SRMR <- fit['scalar - metric',"SRMR"]
    if (abs(SRMR) <= 0.015) {
      Print("<<green OK. Good measurement scalar-invariance based on delta SRMR < 0.015>>")
    } else if (abs(SRMR) > 0.015) {
      Print("<<red Warning. Unacceptable measurement scalar-invariance based on delta SRMR > 0.015>>")
    }
  }
  
  
  if (return_result == T) {
    return(fit)
  }
}
