#' Correlation table
#'
#' `r lifecycle::badge("stable")` \cr
#' This function uses the psych::corr.test function to generated the Pearson correlation table and their associated significance values.
#'
#' @param data data frame
#' @param cols correlation items. Support `dplyr::select()` syntax.
#' @param digits number of digits to round to
#' @param quite suppress printing output
#' @param return_result If it is set to `TRUE`, it will return the data frame of the correlation table
#' @param ... additional arguments passed to correlation::correlation(). See ?correlation::correlation. Note that the return data.frame from correlation::correlation() must contains `r` and `p` (e.g., passing `baysesian = T` will not work)
#'
#' @return data frame of the correlation table
#'
#' @export
#'
#' @examples
#' cor_test(iris, where(is.numeric))
#' 
cor_test <- function(data,
                     cols,
                     ...,
                     digits = 3,
                     quite = F,
                     method = 'pearson',
                     p_adjust = 'holm',
                     return_result = F) {
  
  if (!requireNamespace("correlation", quietly = TRUE)) {
    stop("please install.packages('correlation')")
  }
  
  cols <- enquo(cols)
  data <- data %>% dplyr::select(!!cols)

  cor = correlation::correlation(data = data,
                                 method = method,
                                 p_adjust = p_adjust,
                                 ...)
  cor_df = cor %>% 
    as.data.frame() %>% 
    select(Parameter1,Parameter2,r,p) %>% 
    mutate(p = 
             dplyr::case_when(
               p < 0.001 ~ paste(format_round(r,digits = digits),"***"),
               p < 0.01 & p >= 0.001 ~ paste(format_round(r,digits = digits)," **"),
               p < 0.05 & p >= 0.01 ~ paste(format_round(r,digits = digits),"  *"),
               T ~  paste(format_round(r,digits = digits),"   ")
             )) %>% 
    select(-r) %>% 
    pivot_wider(names_from = Parameter1, values_from = p) %>% 
    rename(Var = .data$Parameter2)
  
  cor_df = tibble::tibble(Var = colnames(data)) %>% 
    full_join(cor_df,by = 'Var') %>% 
    dplyr::mutate(dplyr::across(tidyselect::everything(), function(x) {tidyr::replace_na(data = x, replace = '')}))
  
  if (quite == F) {
    super_print("underline|Model Summary")
    super_print("Model Type = Correlation")
    super_print("Model Method = {method}")
    super_print("Adjustment Method = {p_adjust}")
    cat("\n")
    print_table(cor_df,digits = digits)
  }

  if (return_result == T) {
    return(cor_df)
  }
}
