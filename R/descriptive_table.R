#' Descriptive Statistics Table
#'
#' `r lifecycle::badge("stable")` \cr
#' This function generates a table of descriptive statistics (mainly using psych::describe; Revelle, 2021) and or a correlation table. User can export this to a csv file (optionally, using the file_path argument). Users can open the csv file with MS Excel then copy and paste the table into MS Word table.
#'
#' @param data data frame
#' @param cols  column(s) need to be included in the table. Support `dplyr::select` syntax.
#' @param cor_sig_test adjusted or raw. Default as adjusted. See psych::corr.test to learn more.
#' @param cor_digit number of digit for correlation table
#' @param descriptive_indicator Default is mean, sd, cor. Options are missing (missing value count), non_missing (non-missing value count), cor (correlation table), n, mean, sd, median, trimmed (trimmed mean), median, mad (median absolute deviation from the median), min, max, range, skew, kurtosis, se (standard error)
#' @param descriptive_indicator_digit number of digit for the descriptive table
#' @param file_path file path for export. The function will implicitly pass this argument to the write.csv(file = file_path)
#' @param quite suppress printing output
#' @param return_result return the data frame of the descriptive table
#' @param streamline print streamlined output
#'
#' @return data frame of the descriptive table
#'
#' @export
#'
#' @examples
#' descriptive_table(iris, cols = where(is.numeric)) # all numeric columns
#'
#' descriptive_table(iris,
#'   cols = where(is.numeric),
#'   # get missing count, non-missing count, and mean & sd & correlation table
#'   descriptive_indicator = c("missing", "non_missing", "mean", "sd", "cor")
#' )
descriptive_table <- function(data,
                              cols,
                              cor_sig_test = "raw",
                              cor_digit = 3,
                              descriptive_indicator = c("mean", "sd", "cor"),
                              descriptive_indicator_digit = 3,
                              streamline = F,
                              quite = F,
                              return_result = F,
                              file_path = NULL) {
  cols <- enquo(cols)
  data <- data %>% dplyr::select(!!cols)
  data <- data_check(data)

  # check whether to compute cor_test
  compute_cor_table <- any(descriptive_indicator %in% "cor")

  # init return_df
  return_df <- tibble::tibble(rowname = colnames(data))

  # compute the missing table
  if (any(descriptive_indicator %in% "missing")) {
    missing_df <- data %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(is.na(.)))) %>%
      tidyr::pivot_longer(tidyselect::everything(), names_to = "rowname", values_to = "missing_n")
    return_df <- return_df %>% dplyr::full_join(missing_df, by = "rowname")
  }

  # compute the non-missing table
  if (any(descriptive_indicator %in% "non_missing")) {
    non_missing_df <- data %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.)))) %>%
      tidyr::pivot_longer(tidyselect::everything(), names_to = "rowname", values_to = "non_missing_n")
    return_df <- return_df %>% dplyr::full_join(non_missing_df, by = "rowname")
  }

  # compute the descriptive table
  # remove cor, non_missing, missing indicator as they have been processed
  descriptive_indicator <- descriptive_indicator[!descriptive_indicator %in% c("missing", "non_missing", "cor")]
  if (length(descriptive_indicator) > 0) {
    descriptive_indicator <- enquo(descriptive_indicator)
    descriptive_table <- psych::describe(x = data) %>%
      as.data.frame() %>%
      dplyr::select(!!descriptive_indicator) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ format(round(., descriptive_indicator_digit), nsmall = descriptive_indicator_digit))) %>%
      tibble::rownames_to_column(var = "rowname")
    return_df <- return_df %>% dplyr::full_join(descriptive_table, by = "rowname")
  }
  # compute the correlation table
  if (compute_cor_table == T) {
    cor_table <- data %>% cor_test(cols = !!cols, sig_test = cor_sig_test, digit = cor_digit, descriptive_table_use = T)
    return_df <- return_df %>% dplyr::full_join(cor_table, by = "rowname")
  }

  return_df <- return_df %>% dplyr::rename(Var = .data$rowname)
  if (quite == F) {
    if (streamline == F) {
      cat("\n")
      super_print("underline|Model Summary")
      super_print("Model Type = Descriptive Statistics")
    }
    print_table(return_df)
    cat("\n")
  }
  if (!is.null(file_path)) {
    utils::write.csv(x = return_df, file = file_path)
  }
  if (return_result == T) {
    return(return_df)
  }
}
