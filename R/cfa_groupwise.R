#' Confirmatory Factor Analysis (groupwise)
#'
#' `r lifecycle::badge("stable")` \cr
#' This function will run N number of CFA where N = `length(group)`, and report the fit measures of CFA in each group.
#' The function is intended to help you get a better understanding of which group has abnormal fit indicator
#'
#' @param data data frame
#' @param model explicit `lavaan` model. Must be specify with `model = lavaan_model_syntax`. `r lifecycle::badge("experimental")`
#' @param group character. group variable. Support `dplyr::select()` syntax.
#' @param ordered logical. default is `FALSE`. If it is set to `TRUE`, lavaan will treat it as a ordinal variable and use `DWLS` instead of `ML`
#' @param ... CFA items. Support `dplyr::select()` syntax.
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
                          group,
                          model = NULL,
                          ordered = FALSE) {
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
