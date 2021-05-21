#' Format digits (internal use only)
#'
#' @param x object
#' @param digits number of digits to round to
#' @keywords internal
#' @return a rounded string
format_round <- function(x, digits = 3) {
  format(round(x, digits = digits), nsmall = digits, scientific = FALSE)
}
