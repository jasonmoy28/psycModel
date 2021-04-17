#' Reliability Analysis
#'
#' First, it will determine whether the data is uni-dimensional or multi-dimensional using parameters::n_factors(). If the data is uni-dimensional, then it will print a summary
#' consists of alpha, G6, single-factor CFA, and descriptive statistics result. If it is multi-dimensional, it will print a summary consist of alpha, G6, omega result. You can 
#' bypass this by specifying the dimensionality argument.
#'
#' @param data data frame
#' @param cols items for reliability analysis.  Support `dplyr::select` syntax. 
#' @param descriptive_table Get descriptive statistics. Default is `TRUE`
#' @param digits number of digits
#' @param dimensionality Specify the dimensionality. Either `uni` (uni-dimensionality) or `multi` (multi-dimensionality). Default is `NULL` that determines the dimensionality using EFA.  
#' @param return_result If set to `TRUE` (default is `FALSE`), it will return `psych::alpha` for unidimensional scale, and `psych::omega` for multidimensional scale.
#'
#' @return `psych::alpha` for unidimensional scale, and `psych::omega` for multidimensional scale.
#' @export
#'
#' @examples
#' fit <- reliability_summary(data = lavaan::HolzingerSwineford1939,cols = x1:x3)
#' fit <- reliability_summary(data = lavaan::HolzingerSwineford1939,cols = x1:x9)
#' 
reliability_summary = function(data, 
                               cols,
                               dimensionality = NULL, 
                               digits = 3,
                               descriptive_table = T,
                               return_result = F){
  cols = data %>% dplyr::select(!!enquo(cols)) %>% names()
  data = data %>% dplyr::select(dplyr::all_of(cols))

    if (is.null(dimensionality)) {
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    n_factor_df = parameters::n_factors(data)
    n_factor = getmode(n_factor_df$n_Factors)
    ifelse(test = n_factor == 1,yes = {dimensionality = 'uni'},no = {dimensionality = 'multi'})
  }
  
  super_print('underline|Model Summary')
  super_print('Model Type = Reliability Analysis')
  super_print("Dimensionality = {paste(dimensionality,'-dimensionality', sep = '')}")
  cat('\n')
  
  if (dimensionality == 'uni') {
    alpha_fit = suppressMessages(data %>% psych::alpha())
    alpha_fit_measure = alpha_fit$total[1:3]
    alpha_item_statistics = alpha_fit$alpha.drop %>% 
      dplyr::select('raw_alpha', 'std.alpha', 'G6(smc)') %>% 
      tibble::rownames_to_column('Var') %>% 
      dplyr::rename(Alpha = .data$raw_alpha) %>% 
      dplyr::rename(Alpha.Std = .data$std.alpha) %>% 
      dplyr::rename(`G6 (smc)` = .data$`G6(smc)`) 
    names(alpha_fit_measure) = c('Alpha','Alpha.Std','G6 (smc)')
    
    super_print('underline|Composite Reliability Measures')
    print_table(alpha_fit_measure)
    cat('\n')
    cat('\n')
    super_print('underline|Item Reliability (item dropped)')
    print_table(alpha_item_statistics)
    cat('\n')
    cat('\n')
    
    cat('\n')
    super_print('CFA Model:')
    cfa_summary(data = data,
                cols,
                model_variance = F,
                model_covariance = F,
                digits = digits,
                streamline = T,
                plot = F)
    cat('\n')
    cat('\n')
  } else {
    alpha_fit = suppressMessages(data %>% psych::alpha())
    omega_fit = data %>% psych::omega()
    
    composite_measure = tibble::tibble(Alpha = round(alpha_fit$total['raw_alpha'],digits = digits), 
                           Alpha.Std = round(alpha_fit$total['std.alpha'],digits = digits), 
                           G.6 = omega_fit$G6, 
                           Omega.Hierarchical = omega_fit$omega_h,
                           Omega.Total = omega_fit$omega.tot)
    
     alpha_item_statistics = alpha_fit$alpha.drop %>% 
      dplyr::select('raw_alpha', 'std.alpha', 'G6(smc)') %>% 
      tibble::rownames_to_column('Var') %>% 
       dplyr::rename(Alpha = .data$raw_alpha) %>% 
       dplyr::rename(Alpha.Std = .data$std.alpha) %>% 
       dplyr:: rename(`G6 (smc)` = .data$`G6(smc)`) 
    
    super_print('underline|Composite Reliability Measures')
    print_table(composite_measure)
    cat('\n')
    cat('\n')
    super_print('underline|Item Reliability (item dropped)')
    print_table(alpha_item_statistics)
    cat('\n')
    cat('\n')
  }
  if (descriptive_table == T) {
    super_print('Descriptive Statistics Table:')
    descriptive_table(data = data,
                      cols = dplyr::all_of(cols),
                      cor_digit = digits,
                      descriptive_indicator_digit = digits,
                      streamline = T)
  }
  if (return_result == T) {
    if (dimensionality == 'uni') {
      return(alpha_fit)
    }else{
      return(omega_fit)
    }
  }

}