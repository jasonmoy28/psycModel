#' Cronbach alpha 
#' `r lifecycle::badge("stable")` \cr
#' 
#' Computing the Cronbach alphas for multiple factors.  
#' 
#' @param ... Items. Group each latent factors using c() with when computing Cronbach alpha for 2+ factors (see example below) 
#' @param data `data.frame`. Must specify
#' @param var_name character or a vector of characters. The order of `var_name` must be same as the order of the `...`
#' @param return_result  If it is set to `TRUE`, it will return a `dataframe` object
#' @param group optional character. Specify this argument for computing Cronbach alpha for group separetely  
#'
#' @return a `data.frame` object if return_result is `TRUE`
#' @export
#'
#' @examples
#' cronbach_alpha(
#'   data = lavaan::HolzingerSwineford1939,
#'   var_name = c('Visual','Textual','Speed'),
#'   c(x1,x2,x3), # one way to pass the items of a factor is by wrapping it with c()
#'   x4:x6, # another way to pass the items is use tidyselect syntax 
#'   x7:x9)
cronbach_alpha = function(...,
                          data,
                          var_name,
                          return_result = FALSE,
                          group = NULL) {
  items = enquos(...)
  group <- data %>%
    dplyr::select(!!enquo(group)) %>%
    names()
  if (length(group) == 0) {
    group <- NULL
  }
  #################################### Alpha estimation no group ####################################
  if (is.null(group)) {
    return_df = data.frame()
    item_print = '\n '
    for (i in 1:length(items)) {
      in_loop_data = data %>% dplyr::select(!!items[[i]])
      raw_alpha = psych::alpha(in_loop_data)[['total']][['raw_alpha']]
      std_alpha = psych::alpha(in_loop_data)[['total']][['std.alpha']]
      return_df = rbind(return_df,data.frame(Var = var_name[i],raw_alpha = raw_alpha,std_alpha = std_alpha))
      item_print = paste(item_print,var_name[i],'=',paste(colnames(in_loop_data),collapse = ' + '),'\n ')
    }
    #################################### Alpha estimation group-wise ####################################
  } else{
    try({if(!rlang::is_symbol(group)) {group <- dplyr::sym(group)}},silent = TRUE)
    group = dplyr::enquo(group)
    
    groups <- data %>% dplyr::select(!!group) %>% dplyr::distinct() %>% dplyr::pull()
    return_df = data.frame()
    item_print = '\n '
    for (i in 1:length(items)) {
      for (j in 1:length(groups)) {
        in_loop_data = data %>% dplyr::filter(!!group == groups[j]) %>% dplyr::select(!!items[[i]])
        raw_alpha = psych::alpha(in_loop_data)[['total']][['raw_alpha']]
        std_alpha = psych::alpha(in_loop_data)[['total']][['std.alpha']]
        return_df = rbind(return_df,
                          data.frame(Var = var_name[i],
                                     group = groups[j],
                                     raw_alpha = raw_alpha,
                                     std_alpha = std_alpha))
      }
      item_print = paste(item_print,var_name[i],'=',paste(colnames(in_loop_data),collapse = ' + '),'\n ')
    }
    return_df = return_df %>% dplyr::rename(!!group := group)
  }
  cat("\n \n")
  super_print("underline|Model Summary")
  super_print("Model Type = Cronbach Alpha Reliability Analysis")
  super_print("Model Specification: ")
  super_print(item_print)
  cat("\n")
  print_table(return_df)
  cat("\n")
  if (return_result) {
    return(return_df)
  }
}
