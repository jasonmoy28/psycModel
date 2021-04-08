#' Mean, SD, and Correlation table
#'
#' This function uses the psych::corr.test (Revelle, 2021) function to generated the correlation. One potential problem with the current version of the function is that if the correlation between the two item is too high (rounded as 1), then that cell will become blank. This is a very rare problem, and I have not yet figure out how to solve this.
#'
#' @param data a dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) need to be included in the table.
#' @param cor_sig_test adjusted or raw. Default as adjusted. See psych::corr.test to learn more.
#' @param cor_digit number of digit for correlation table
#' @param descriptive_indicator Default is mean, sd, cor. Options are missing (missing value count), non_missing (non-missing value count), cor (correlation table), n, mean, sd, median, trimmed (trimmed mean), median, mad (median absolute deviation from the median), min, max, range, skew, kurtosis, se (standard error)
#' @param descriptive_indicator_digit number of digit for the descriptive table 
#' @param filepath provide full path to pass into the write.csv(file = filepath)
#'
#' @export
#' @references 
#' Revelle, W. (2021). psych: Procedures for Psychological, Psychometric, and Personality Research. Northwestern University, Evanston, Illinois. R package version 2.1.3, https://CRAN.R-project.org/package=psych.
#' 
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modelling in R. R package version 0.1.0, https://github.com/jasonmoy28/psycModel.#'
#' 
#' @examples
#' descriptive_table(iris,cols = tidyr::everything()) # all columns
#' descriptive_table(iris,cols = where(is.numeric)) # all numeric columns

descriptive_table =  function(data,
                              cols,
                              cor_sig_test = 'raw',
                              cor_digit = 3,
                              descriptive_indicator = c('mean','sd','cor'),
                              descriptive_indicator_digit = 3,
                              filepath = NULL) {
  cols = ggplot2::enquo(cols)
  data = data %>% dplyr::select(!!cols)
  compute_cor_table = any(descriptive_indicator %in% 'cor')
  
  datatype = as.vector(sapply(data, class))
  if(all(datatype == 'numeric'| datatype == 'factor' | datatype == 'integer')){
    data = data %>% dplyr::mutate(dplyr::across(tidyr::everything(),as.numeric))
  } else{
    print('Error: All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()')
    return()
  }
  return_df = tibble::tibble(rowname = colnames(data))
  
  # compute the missing table 
  if (any(descriptive_indicator %in% 'missing')) {
    missing_df = data %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(is.na(.)))) %>%
      tidyr::pivot_longer(tidyr::everything(),names_to = 'rowname', values_to = 'missing_n')
    return_df = return_df %>% dplyr::full_join(missing_df,by = 'rowname')
  }
  
  # compute the non-missing table 
  if (any(descriptive_indicator %in% 'non_missing')) {
    non_missing_df = data %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.)))) %>%
      tidyr::pivot_longer(tidyr::everything(),names_to = 'rowname', values_to = 'non_missing_n')
    return_df = return_df %>% dplyr::full_join(non_missing_df,by = 'rowname')
  }

  # compute the descriptive table  
  # remove cor, non_missing, missing indicator as they have been processed
  descriptive_indicator = descriptive_indicator[!descriptive_indicator %in% c('missing','non_missing','cor')]
  if (length(descriptive_indicator) > 0) {
    descriptive_indicator = ggplot2::enquo(descriptive_indicator)
    descriptive_table = psych::describe(x = data) %>% 
      as.data.frame() %>% 
      dplyr::select(!!descriptive_indicator) %>% 
      dplyr::mutate(dplyr::across(where(is.numeric), ~ format(round(., descriptive_indicator_digit),nsmall = descriptive_indicator_digit))) %>% 
      tibble::rownames_to_column(var = 'rowname') 
    return_df = return_df %>% dplyr::full_join(descriptive_table,by = 'rowname')
  }
  # compute the correlation table
  if (compute_cor_table == T) {
    cor_table = data %>% cor_test(cols = !!cols, sig_test = cor_sig_test,digit = cor_digit,descriptive_table_use = T)
    return_df = return_df %>% dplyr::full_join(cor_table,by = 'rowname')
  }
  
  return_df = return_df %>% tibble::column_to_rownames()

  if (!is.null(filepath)) {
    utils::write.csv(x = return_df, file = filepath)
  }
  return(return_df)
}
