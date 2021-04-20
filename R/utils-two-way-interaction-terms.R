#' Interaction term for Mixed Effect Model (internal use only)
#' Create interaction terms for regression models
#'
#' @param interaction_terms vector. names of the interaction terms
#' @return a vector of all two-way interaction terms
#'
#' @keywords internal
#'
two_way_interaction_terms <- function(interaction_terms) {
  index <- 1
  return_vector <- c()
  for (interaction_term in interaction_terms) {
    subset_index <- index + 1
    while (subset_index <= length(interaction_terms)) {
      concated_term <- paste(interaction_term, interaction_terms[subset_index], sep = "*")
      return_vector <- c(return_vector, concated_term)

      subset_index <- subset_index + 1
    }
    index <- index + 1
  }
  return(return_vector)
}
