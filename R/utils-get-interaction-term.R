#' get interaction term
#'
#' @param model model
#'
#' @return a list with predict vars names

get_interaction_term = function(model){
  # get interaction term
  interaction_term <- model %>%
    insight::find_interactions() %>%
    .$conditional
  
  if(is.null(interaction_term)){
    return(NULL)
  }
  
  three_way_interaction_exist = length(interaction_term[stringr::str_detect(":.+:", string = interaction_term)])
  
  # two-way interaction
  if (three_way_interaction_exist == 0) {
    interaction_term <- interaction_check(interaction_term)
    
    predict_var1 <- gsub(pattern = ":.+", "", x = interaction_term)
    predict_var2 <- gsub(pattern = ".+:", "", x = interaction_term)
    
    return(list(predict_var1 = predict_var1,
                predict_var2 = predict_var2))
    
    # three-way interaction
  } else if(three_way_interaction_exist != 0){
    
    interaction_term <- interaction_term[stringr::str_detect(":.+:", string = interaction_term)]
    predict_var1 <- gsub(pattern = ":.+", "", x = interaction_term)
    predict_var3 <- gsub(pattern = ".+:", "", x = interaction_term)
    remove1 <- stringr::str_remove(pattern = predict_var1, string = interaction_term)
    remove2 <- stringr::str_remove(pattern = predict_var3, string = remove1)
    predict_var2 <- gsub(pattern = ":", "", x = remove2)
    
    return(list(predict_var1 = predict_var1,
                predict_var2 = predict_var2,
                predict_var3 = predict_var3))
  }
}
