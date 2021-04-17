#' Correlation table
#'
#' `r lifecycle::badge("stable")` \cr
#' This function uses the psych::corr.test function to generated the Pearson correlation table and their associated significance values.
#'
#' @param data data frame
#' @param cols columns. tidyselect syntax or helpers.
#' @param digit number of digits
#' @param sig_test Default is raw. Options are 'adjusted' or 'raw'. Adjusted use holm adjustment method. See ?stats::p.adjust to learn why.
#' @param quite suppress printing output
#' @param return_result return the data frame of the descriptive table
#' @param ... additional argument for internal use
#'
#' @return data frame of the correlation table
#'
#' @export
#'
#' @examples
#' cor_test(iris, where(is.numeric))
#' cor_test(iris, where(is.numeric), sig_test = "adjusted") # use adjusted correlation
cor_test <- function(data,
                     cols,
                     sig_test = "raw",
                     digit = 3,
                     quite = F,
                     return_result = F,
                     ...) {
  cols <- enquo(cols)
  data <- data %>% dplyr::select(!!cols)
  data <- data_check(data)

  cor_test_df <- data %>%
    dplyr::mutate(dplyr::across(!!cols, as.numeric)) %>%
    psych::corr.test()

  cor_df_raw <- as.data.frame(cor_test_df$r) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ format(round(., digit), nsmall = digit)))

  cor_df <- cor_df_raw %>%
    dplyr::mutate("rowname" = colnames(cor_df_raw)) %>%
    dplyr::select("rowname", tidyselect::everything())


  sig_df <- tidyr::as_tibble(cor_test_df$p) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~
    dplyr::case_when(
      . < 0.001 ~ "***",
      . < 0.01 & . >= 0.001 ~ "**",
      . < 0.05 & . >= 0.01 ~ "*",
      T ~ ""
    )))

  # concat sig_df with cor_df
  for (i in c(1:ncol(sig_df))) {
    c_vec <- stringr::str_c(cor_df[[i + 1]], sig_df[[i]])
    cor_df[[i + 1]] <- c_vec
  }
  if (sig_test == "raw") {
    index <- 1
    for (columns in colnames(cor_df[-1])) {
      cor_df[1:index, columns] <- ""
      index <- index + 1
    }
  } else if (sig_test == "adjusted") {
    index <- 1
    for (columns in colnames(cor_df)[-1]) {
      cor_df[index:nrow(cor_df), columns] <- ""
      index <- index + 1
    }
  }

  dots <- list(...)
  if (length(dots) > 0) {
    if (dots$descriptive_table_use == T) {
      return(cor_df)
    }
  }
  cor_df <- cor_df %>% dplyr::rename(Var = .data$rowname)

  if (quite == F) {
    super_print("underline|Model Summary")
    super_print("Model Type = Pearson Correlation")
    cat("\n")
    print_table(cor_df)
  }

  if (return_result == T) {
    return(cor_df)
  }

  # printing warning message, non-essential block
  coreced_name <- NULL
  coreced_name <- data %>%
    dplyr::select(!where(is.numeric)) %>%
    names()
  if (length(coreced_name) != 0) {
    warning_message <- paste(paste(coreced_name, collapse = ", "), "were coreced into numeric")
    warning(warning_message)
  }
}
