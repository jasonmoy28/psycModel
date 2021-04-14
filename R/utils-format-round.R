#' Format digits (internal use only)
#'
#' @param x object
#' @param nsmall number of digit
#'
#' @export
#'
#'
format_round <- function(x, nsmall = 3) {
  format(x, digits = 0, nsmall = nsmall, scientific = F)
}
