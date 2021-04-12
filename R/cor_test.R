#' Correlation table
#'
#' `r lifecycle::badge("stable")` \cr
#' This function uses the psych::corr.test (Revelle, 2021) function to generated the pearson correlation table and their associated significance values.
#'
#' @param data data frame
#' @param cols columns. tidyselect syntax or helpers.
#' @param digit number of digits
#' @param sig_test Default is raw. Options are 'adjusted' or 'raw'. Adjusted use holm adjustment method. See ?stats::p.adjust to learn why.
#' @param ... additional argument.
#'
#' @export
#' @references
#' Revelle, W. (2021). psych: Procedures for Psychological, Psychometric, and Personality Research. Northwestern University, Evanston, Illinois. R package version 2.1.3, https://CRAN.R-project.org/package=psych.
#'
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' @examples
#' cor_test(iris, where(is.numeric))
#' cor_test(iris, where(is.numeric), sig_test = "adjusted") # use adjusted correlation
cor_test <- function(data, cols, sig_test = "raw", digit = 3, ...) {
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
    dplyr::select("rowname", tidyr::everything())


  sig_df <- tidyr::as_tibble(cor_test_df$p) %>%
    dplyr::mutate(dplyr::across(tidyr::everything(), ~
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
  cor_df <- cor_df %>% dplyr::select(-"rowname")

  # printing warning message, non-essential block
  coreced_name <- NULL
  coreced_name <- data %>%
    dplyr::select(!where(is.numeric)) %>%
    names()
  if (length(coreced_name) != 0) {
    warning_message <- paste(paste(coreced_name, collapse = ", "), "were coreced into numeric")
    warning(warning_message)
  }
  return(cor_df)
}
