#' text_convert for super_print (internal use only)
#'
#' Command options are bold, italic, underline, red, green, yellow, blue, silver.
#' Greek letter options are alpha, beta, gamma, delta, epsilon, eta, theta, lamda, pi, rho, sigma, chi, omega. If capital letter greek letter, use all capital letter command (e.g., ALPHA)
#'
#' @param text character. inputted text
#' @param type either "greek" (convert greek character) or "command" (convert color text and font style command). Command options are bold, italic, underline, red, green, yellow, blue, silver.
#'
#' @return converted unicode-based character for greek letter, color, font style
#' @keywords internal
#'
text_convert <- function(text, type) {
  text <- gsub(pattern = "\\^2", replacement = "\U00B2", x = text)
  greek_text_convert <- function(text) {
    text <- switch(text,
      "alpha" = "\U03B1",
      "beta" = "\U03B2",
      "gamma" = "\U03B3",
      "delta" = "\U03B4",
      "epsilon" = "\U03B5",
      "eta" = "\U03B7",
      "theta" = "\U03B8",
      "lamda" = "\U03BB",
      "pi" = "\U03C0",
      "rho" = "\U03C1",
      "sigma" = "\U03C3",
      "chi" = "\U03C7",
      "omega" = "\U03C9",
      "ALPHA" = "\U0391",
      "BETA" = "\U0392",
      "GAMMA" = "\U0393",
      "DELTA" = "\U0394",
      "EPSILON" = "\U0395",
      "ETA" = "\U0397",
      "THETA" = "\U0398",
      "LAMDA" = "\U039B",
      "PI" = "\U03A0",
      "RHO" = "\U03A1",
      "SIGMA" = "\U03A3",
      "CHI" = "\U03A7",
      "OMEGA" = "\U03A9",
      "abs" = "\U007C"
    )
  }
  greek_text <- stringr::str_match_all(string = text, pattern = "\\$\\s*(.*?)\\s*\\$")[[1]][, 2]
  if (length(greek_text) > 0) {
    greek_text_list <- sapply(greek_text, FUN = function(x) {
      greek_text_convert(text = x)
    })
    greek_text_raw <- names(greek_text_list)
    greek_text_processed <- greek_text_list %>% as.vector()
    for (i in c(1:length(greek_text_raw))) {
      text <- stringr::str_replace(
        string = text,
        pattern = greek_text_raw[i],
        replacement = greek_text_processed[i]
      )
    }
    text <- gsub(pattern = "\\$", replacement = "", x = text)
  }
  if (type == "command") {
    text <- switch(text,
      "red" = c("31", "39"),
      "green" = c("32", "39"),
      "yellow" = c("33", "39"),
      "silver" = c("90", "39"),
      "blue" = c("34", "39"),
      "bold" = c("1", "24"),
      "italic" = c("3", "23"),
      "underline" = c("4", "24"),
    )
  }
  return(text)
}
