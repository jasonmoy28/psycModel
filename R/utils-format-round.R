#' Format digits (internal use only)
#'
#' @param x object
#' @param digits number of digit
#'
#' @keywords internal
format_round <- function(x, digits = 3) {
  format(round(x, digits = digits), nsmall = digits, scientific = F)
}
