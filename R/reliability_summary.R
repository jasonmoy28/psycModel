#' Reliability Analysis
#'
#' For multidimensional scale (default), it will uses the psych::omega function to get the omega total, omega hierarchical, and alpha. For uni-dimensional scale, it will compute the alpha and a single-factor CFA model.  
#'
#' @param data data frame
#' @param cols items for reliability analysis.  Support `dplyr::select` syntax. 
#' @param CFA Run confirmatory factor analysis. Default is `TRUE`
#' @param descriptive_table Get descriptive statistics. Default is `TRUE`
#' @param digits number of digits
#'
#' @export
#'
#' @examples
#' fit <- reliability_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   cols = x1:x3
#' )
#' 
reliability_summary = function(data, 
                               cols,
                               digits = 3,
                               CFA = T,
                               descriptive_table = T){
  cols = data %>% dplyr::select(!!enquo(cols)) %>% names
  super_print('underline|Model Summary')
  super_print('Model Type = Reliability Analysis')
  cat('\n')
  if (descriptive_table == T) {
    super_print('Descriptive Statistics Table:')
    descriptive_table(data = data,
                      cols = all_of(cols),
                      cor_digit = digits,
                      descriptive_indicator_digit = digits,
                      streamline = T)
  }
  if (CFA == T) {
    cat('\n')
    super_print('CFA Model:')
    cfa_summary(data = data,
                cols,
                model_variance = F,
                model_covariance = F,
                digits = digits,
                streamline = T)
  }
}