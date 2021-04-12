#' Measurement Invariance
#'
#' `r lifecycle::badge("stable")` \cr
#' Compute the measurement invariance model (i.e., measurement equivalence model) using multi-group confirmatory factor analysis (MGCFA; Jöreskog, 1971). This function uses the lavaan::cfa (Rosseel, 2012). Users can run the configural-metric or the configural-metric-scalar comparisons (see below for detail instruction).
#'
#' @param data data frame
#' @param ... CFA items. Multi-factor CFA items should be separated by comma (as different argument). See below for example
#' @param model explicit lavaan model. Either the `model` argument or the `items` argument must be specified.
#' @param group character. group variable.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param group_partial items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').
#' @param invariance_level "metric" or "scalar". Default is 'metric'. Set as 'metric' for configural-metric comparison, and set as 'scalar' for configural-metric-scalar comparison.
#' @param digits number of digit to round
#'
#' @details
#' All argument must be explicitly specified. If not, all arguments will be treated as CFA items.
#' @references
#' Jöreskog, K. G. (1971). Simultaneous factor analysis in several populations. Psychometrika, 36(4), 409-426.
#'
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1–36. https://www.jstatsoft.org/v48/i02/.
#'
#' @export
#' @return
#' return the compareFit object
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
                                   invariance_level = "scalar",
                                   digits = 3) {
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

  # Print statement
  cat("Computing CFA using:\n", model)

  if (invariance_level == "metric") {
    print("Computing for configural model")
    config_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      ordered = ordered,
      group.partial = group_partial
    )
    print("Computing for metric model")
    metric_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = "loadings",
      ordered = ordered,
      group.partial = group_partial
    )

    fit <- compare_fit(list(config_model, metric_model), digits = digits)
    return(fit)
  } else if (invariance_level == "scalar") {
    print("Computing for configural model")
    config_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      ordered = ordered,
      group.partial = group_partial
    )
    print("Computing for metric model")
    metric_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = "loadings",
      ordered = ordered,
      group.partial = group_partial
    )
    print("Computing for scalar model")

    scalar_model <- lavaan::cfa(
      model = model,
      data = data,
      group = group,
      group.equal = c("loadings", "intercepts"),
      ordered = ordered,
      group.partial = group_partial
    )
    fit <- compare_fit(list(config_model, metric_model, scalar_model), digits = digits)
    return(fit)
  } else {
    print("Error: Invariance level must be either metric or scalar")
    return()
  }
}
