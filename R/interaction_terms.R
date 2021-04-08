#' Interaction term for Mixed Effect Model (do not call directly)
#'
#' Essential for HLM.Model::HLM_Model
#'
#' @param interaction_terms vector. names of the interaction terms
#' @return vector. The vector is essential for the lme formula used in function `HLM.Model::HLM_Model`
#' @export
#'
#' @examples
#'
#'
two_way_interaction_terms <- function(interaction_terms) {
  index = 1
  return_vector = c()
  for (interaction_term in interaction_terms) {
    subset_index = index + 1
    while(subset_index <= length(interaction_terms)){
      concated_term = paste(interaction_term,interaction_terms[subset_index],sep = '*')
      return_vector = c(return_vector,concated_term)

      subset_index = subset_index + 1
    }
    index = index + 1
  }
  return(return_vector)
}
