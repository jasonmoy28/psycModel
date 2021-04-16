#' Confirmatory Factor Analysis (groupwise)
#'
#' `r lifecycle::badge("stable")` \cr
#' This function will run a series of CFA (n = length(group)) with respect to each group. The function is intended to help you get a better understanding of which group has abnormal fit indicator
#'
#' @param data data frame
#' @param model explicit lavaan model. Must be specify with `model = lavaan_model_syntax`.
#' @param group character. group variable.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param ... CFA items.
#'
#' @references
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1–36. https://www.jstatsoft.org/v48/i02/.
#'
#' @details
#' All argument must be explicitly specified. If not, all arguments will be treated as CFA items
#'
#' @return data frame with group-wise CFA result
#'
#' @export
#' @examples
#' # The example is used as the illustration of the function output only.
#' # It does not imply the data is appropriate for the analysis.
#' cfa_groupwise(
#'   data = lavaan::HolzingerSwineford1939,
#'   group = "school",
#'   x1:x3,
#'   x4:x6,
#'   x7:x9
#' )
cfa_groupwise <- function(data,
                          ...,
                          model = NULL,
                          group,
                          ordered = F) {
  group <- enquo(group)
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

  groups <- data %>%
    dplyr::select(!!group) %>%
    dplyr::distinct()
  groups <- c(groups)[[1]]
  return_df <- data.frame(group = NULL, cfi = NULL, rmsea = NULL, tli = NULL)
  for (i in groups) {
    cfa_data <- data %>%
      dplyr::filter(dplyr::across(!!group) == i)
    cfa_model_summary <- lavaan::cfa(model = model, data = cfa_data, ordered = ordered)
    cfa_model_summary <- as.data.frame(lavaan::fitmeasures(cfa_model_summary))
    summary_df <- data.frame(group = i, cfi = cfa_model_summary["cfi", ], rmsea = cfa_model_summary["rmsea", ], tli = cfa_model_summary["tli", ])
    return_df <- rbind(return_df, summary_df)
  }
  return(return_df)
}
