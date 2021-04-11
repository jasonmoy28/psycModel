#' Confirmatory Factor Analysis
#'
#' `r lifecycle::badge("stable")` \cr
#' The function fits a CFA model using the lavaan::cfa (Rosseel, 2012) function. Users can fit single and multiple factors CFA (read details), and it also supports multilevel CFA (specifying the group). Users can pass the items (see below for example) or an explicit lavaan model for more versatile usage.
#'
#' @param data data frame
#' @param ... CFA items. Multi-factor CFA items should be separated by comma (as different argument).
#' @param model explicit lavaan model. Must be specify with `model = lavaan_model_syntax`.
#' @param group optional character. used for multi-level CFA. the nested variable for multilevel dataset (e.g., Country)
#' @param summary_item vector of fit indices. Default is CFI, RMSEA, TLI, and SRMR.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param return_result Default is model. Options are 'model' (lavaan model), 'short_summary' (fit index summary only), 'long_summary' (lavaan full summary).
#' @param quite default as F. If set to true, it will not print the running model statement.
#' @param group_partial Items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').
#'
#' @details
#' All argument must be explicitly specified. If not, all arguments will be treated as CFA items.
#'
#' @references
#' Bao, H.-W.-S. (2021). bruceR: Broadly useful convenient and efficient R functions. R package version 0.6.0. https://CRAN.R-project.org/package=bruceR
#'
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1–36. https://www.jstatsoft.org/v48/i02/.
#'
#' @export
#' @examples
#'
#' # Fitting a single factor CFA model
#' cfa_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   x1:x3
#' )
#'
#' # Fitting a multilevel single factor CFA model
#' cfa_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   x1:x3,
#'   group = "sex"
#' )
#'
#' # Fitting a multiple factor CFA model
#' cfa_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   x1:x3,
#'   x4:x6,
#'   x7:x9
#' )
#'
#' # Fitting a CFA model by passing explicit lavaan model (equivalent to the above model)
#' # Note in the below function how I added `model = ` in front of the lavaan model.
#' # Similarly, the same rule apply for all arguments (e.g., `ordered = F` instead of `F`)
#' cfa_summary(
#'   model = "visual  =~ x1 + x2 + x3
#'                      textual =~ x4 + x5 + x6
#'                      speed   =~ x7 + x8 + x9 ",
#'   data = lavaan::HolzingerSwineford1939
#' )
#' \dontrun{
#' # This will fail because I did not add `model = ` in front of the lavaan model.
#' # Therefore,you must add the tag in front of all arguments
#' # For example, `return_result = 'model'` instaed of `model`
#' cfa_summary("visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 ",
#'   data = lavaan::HolzingerSwineford1939
#' )
#' }
cfa_summary <- function(data,
                        ...,
                        model = NULL,
                        group = NULL,
                        summary_item = c("cfi", "rmsea", "tli", "srmr"),
                        ordered = F,
                        return_result = "model",
                        quite = F,
                        group_partial = NULL) {
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
  if (quite == F) {
    cat("Computing CFA using:\n", model)
  }

  # for long summary result, run CFA using bruceR if(return_result == 'bruceR_summary') {
  #   if (!is.null(group)) {
  #     warning('Group variable is ignored. Multilevel CFA is only supported with returning model or short_summary')
  #   }
  #
  #   }
  # }

  cfa_model <- lavaan::cfa(
    model = model,
    data = data,
    group = group,
    ordered = ordered,
    group.partial = group_partial
  )
  if (ordered) {
    summary_item <- paste(summary_item, ".scaled", sep = "")
  }
  if (return_result == "model") {
    return(cfa_model)
  } else if (return_result == "short_summary") {
    cfa_short_summary <- lavaan::fitMeasures(cfa_model)[summary_item]
    return(cfa_short_summary)
  } else if (return_result == "long_summary") {
    lavaan::summary(cfa_model, fit.measure = T, standardized = T)
  }
}
