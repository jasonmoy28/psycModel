#' Interaction term for Mixed Effect Model (internal use only)
#' Create interaction terms for regression models
#'
#' @param interaction_terms vector. names of the interaction terms
#' @return a vector of all three-way interaction terms
#'
#' @keywords internal
#'
three_way_interaction_terms <- function(interaction_terms) {
  index1 <- 1
  return_vector <- c()
  
  for (term1 in interaction_terms) {
    index2 <- index1 + 1
    while (index2 <= length(interaction_terms)) {
      term2 <- interaction_terms[index2]
      index3 <- index2 + 1
      
      while (index3 <= length(interaction_terms)) {
        term3 <- interaction_terms[index3]
        concated_term <- paste(term1, term2, term3, sep = "*")
        return_vector <- c(return_vector, concated_term)
        index3 <- index3 + 1
      }
      index2 <- index2 + 1
    }
    index1 <- index1 + 1
  }
  
  return(return_vector)
}