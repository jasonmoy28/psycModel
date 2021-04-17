#' super_print (internal use only)
#'
#' print color, greek-letter, font-style formatted text. Inputted text need to be 'command|$greek letter$'.
#' Command options are bold, italic, underline, red, green, yellow, blue, silver.
#' Greek letter options are alpha, beta, gamma, delta, epsilon, eta, theta, lamda, pi, rho, sigma, chi, omega. If capital letter greek letter, use all capital letter command (e.g., ALPHA)
#' If you wish to print normal text, then the inputted text should '|text'
#'
#' @param text inputted text
#' @param env environment.
#'
#' @return formatted text (greek letter, color, font style formatted)
#'
#' @keywords internal
#'
super_print <- function(text,
                        env = parent.frame()) {
  text <- glue::glue(text, .envir = env)
  command_text <- gsub(pattern = "\\|.+", replacement = "", x = text) %>% stringr::str_trim("left")
  output_text <- gsub(pattern = ".+\\|", replacement = "", x = text)
  output_text <- text_convert(text = output_text, type = "greek")

  # format text based on command
  if (command_text != "") {
    command_text <- text_convert(text = command_text, type = "command")
    output_text <- paste("\033[", command_text[1], "m", output_text, "\033[", command_text[2], "m", sep = "")
  }
  cat(output_text)
  cat("\n")
}
