#' Correlation table
#'
#' `r lifecycle::badge("stable")` \cr
#' This function uses the `correlation::correlation()` to generate the correlation table.
#'
#' @param data data frame
#' @param cols correlation items. Support `dplyr::select()` syntax.
#' @param digits number of digits to round to
#' @param quite suppress printing output
#' @param return_result If it is set to `TRUE`, it will return the data frame of the correlation table
#' @param ... additional arguments passed to correlation::correlation(). See ?correlation::correlation. Note that the return data.frame from correlation::correlation() must contains `r` and `p` (e.g., passing `baysesian = TRUE` will not work)
#' @param method Default is "pearson". Options are "kendall", "spearman","biserial", "polychoric", "tetrachoric", "biweight", "distance", "percentage", "blomqvist", "hoeffding", "gamma", "gaussian","shepherd", or "auto". See ?correlation::correlation for detail
#' @param p_adjust Default is "holm". Options are "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "somers" or "none". See ?stats::p.adjust for more detail
#' @param streamline print streamlined output.
#'
#' @return data frame of the correlation table
#'
#' @export
#'
#' @examples
#' cor_test(iris, where(is.numeric))
cor_test <- function(data,
                     cols,
                     ...,
                     digits = 3,
                     method = "pearson",
                     p_adjust = "holm",
                     streamline = FALSE,
                     quite = FALSE,
                     return_result = FALSE) {
  if (!requireNamespace("correlation", quietly = TRUE)) {
    stop("please install.packages('correlation')")
  }

  cols <- enquo(cols)
  data <- data %>% dplyr::select(!!cols)

  cor <- correlation::correlation(
    data = data,
    method = method,
    p_adjust = p_adjust,
    ...
  )
  cor_df <- cor %>%
    as.data.frame() %>%
    dplyr::select("Parameter1", "Parameter2", "r", "p") %>%
    dplyr::mutate(
      p =
        dplyr::case_when(
          p < 0.001 ~ paste(format_round(r, digits = digits), "***"),
          p < 0.01 & p >= 0.001 ~ paste(format_round(r, digits = digits), " **"),
          p < 0.05 & p >= 0.01 ~ paste(format_round(r, digits = digits), "  *"),
          TRUE ~ paste(format_round(r, digits = digits), "   ")
        )
    ) %>%
    dplyr::select(-"r") %>%
    tidyr::pivot_wider(names_from = .data$Parameter1, values_from = .data$p) %>%
    dplyr::rename(Var = .data$Parameter2)

  cor_df <- tibble::tibble(Var = colnames(data)) %>%
    dplyr::full_join(cor_df, by = "Var") %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), function(x) {
      tidyr::replace_na(data = x, replace = "")
    }))

  if (quite == FALSE) {
    if (streamline == FALSE) {
      super_print("underline|Model Summary")
      super_print("Model Type = Correlation")
      super_print("Model Method = {method}")
      super_print("Adjustment Method = {p_adjust}")
      cat("\n")
    }
    print_table(cor_df, digits = digits)
  }

  if (return_result == TRUE) {
    return(cor_df)
  }
}
