#' Interaction_value
#' 
#' Utility function for extracting interaction values
#'
#' @param model_data a data.frame
#' @param variable variable name
#'
#' @return
#' For continuous variables, it returns a vector of -1SD and +1SD. For binary variables, it returns the two unique levels.
#' @keywords internal
#' 
interaction_value <- function(model_data,variable) {
  
  variable_data = model_data %>% dplyr::pull(!!enquo(variable))
  if (length(unique(variable_data)) != 2) {
    var_level = c(mean(variable_data,na.rm = T) - stats::sd(variable_data,na.rm = T),
                           mean(variable_data,na.rm = T) + stats::sd(variable_data,na.rm = T))
  } else {
    var_level = c(unique(variable_data)[1],unique(variable_data)[2])
  }
  return(var_level)
}
