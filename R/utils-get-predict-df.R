#' get factor df to combine with mean_df
#'
#' @param data data
#'
#' @return factor_df
#'
get_predict_df <- function(data) {
  mean_df <- dplyr::summarise(data, dplyr::across(!where(is.factor), ~ mean(., na.rm = TRUE)))
  factors = data %>% dplyr::select(where(is.factor)) %>% colnames(.)
  if (length(factors) != 0) {
    factor_df = dplyr::tibble(var = NA)
    for (i in factors) {
      in_loop_df = dplyr::tibble(levels(data[[i]])[1])
      colnames(in_loop_df) = i
      factor_df = cbind(factor_df,in_loop_df)
    }
    factor_df = factor_df[-1]
    mean_df = cbind(mean_df,factor_df) %>% dplyr::mutate(dplyr::across(where(is.character), as.factor))
  }
  
  upper_df <- dplyr::summarise(data, dplyr::across(!where(is.factor), ~ mean(., na.rm = TRUE) + stats::sd(., na.rm = TRUE)))
  factors = data %>% dplyr::select(where(is.factor)) %>% colnames(.)
  if (length(factors) != 0) {
    upper_df = cbind(upper_df,factor_df) %>% dplyr::mutate(dplyr::across(where(is.character), as.factor))
  }
  
  lower_df <- dplyr::summarise(data, dplyr::across(!where(is.factor), ~ mean(., na.rm = TRUE) - stats::sd(., na.rm = TRUE)))
  factors = data %>% dplyr::select(where(is.factor)) %>% colnames(.)
  if (length(factors) != 0) {
    lower_df = cbind(lower_df,factor_df) %>% dplyr::mutate(dplyr::across(where(is.character), as.factor))
  }
  
  return(list(upper_df = upper_df,
              mean_df = mean_df,
              lower_df = lower_df))
}
