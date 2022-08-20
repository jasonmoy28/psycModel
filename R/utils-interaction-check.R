#' interaction_check
#'
#' @param interaction_term
#'
#' @return nothing
#' @keywords internal
#'
interaction_check <- function(interaction_term) {
  if (length(interaction_term) > 1) {
    interaction_term <- interaction_term[1]
    warning(
      paste(
        "Inputted > 2 interaction terms. Plotting the first interaction term:\n ",
        interaction_term
      )
    )
  }
  return(interaction_term)
}
