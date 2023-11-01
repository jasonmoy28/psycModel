#' change coefficient to p value for model_table
#'
#' @param data_frame data
#' @param marginal_alpha marginal_alpha leve
#' @param show_p show por not
#' @keywords internal
#' @export
#'
coefficent_to_p = function(data_frame,
                           marginal_alpha = 0.1,
                           show_p = FALSE) {
  return_df = data_frame %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(c("p", "P")), function(x) {
      dplyr::case_when(
        x == "" ~  "   ",
        x <= 0.001 ~ "***",
        x <= 0.01 & x > 0.001 ~ "** ",
        x <= 0.05 & x > 0.01 ~  "*  ",
        x <= marginal_alpha & x > 0.05 ~  ".  ",
        x > marginal_alpha ~  "   ",
        TRUE ~  "   "
      )
    })) %>% 
    dplyr::mutate(Coefficient = paste(format_round(.data$Coefficient,digits = 3),.data$p))
  
  if (show_p == TRUE) {
    return_df = data_frame %>% 
      dplyr::mutate(Coefficient = paste0(format_round(.data$Coefficient,digits = 3),' (',format_round(.data$p,digits = 3),')')) %>% 
      dplyr::mutate(dplyr::across(dplyr::any_of(c("p", "P")), function(x) {
        dplyr::case_when(
          x == "" ~  "   ",
          x <= 0.001 ~ "***",
          x <= 0.01 & x > 0.001 ~ "** ",
          x <= 0.05 & x > 0.01 ~  "*  ",
          x <= marginal_alpha & x > 0.05 ~  ".  ",
          x > marginal_alpha ~  "   ",
          TRUE ~  "   "
        )
      })) %>% 
      dplyr::mutate(Coefficient = paste(.data$Coefficient,.data$p))
    
    
  }
  
  return_df = return_df %>% 
    dplyr::select(dplyr::any_of(c('Parameter','Coefficient')))
  
  return(return_df)
}
