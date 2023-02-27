coefficent_to_p = function(data_frame,
                           alpha = 0.1) {
  return_df = data_frame %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(c("p", "P")), function(x) {
      dplyr::case_when(
        x == "" ~  "   ",
        x <= 0.001 ~ "***",
        x <= 0.01 & x > 0.001 ~ "** ",
        x <= 0.05 & x > 0.01 ~  "*  ",
        x <= alpha & x > 0.05 ~  ".  ",
        x > alpha ~  "   ",
        TRUE ~  "   "
      )
    })) %>% 
    dplyr::mutate(Coefficient = paste(format_round(.data$Coefficient,digits = 3),.data$p)) %>% 
    dplyr::select(-dplyr::any_of('p'))
  
  return(return_df)
}