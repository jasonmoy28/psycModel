#' Print from bruceR (internal-use only)
#'
#' @param ... ...
#'
#' @describeIn Print Paste strings.
#'
#' @importFrom glue glue glue_col
#' @importFrom crayon bold italic underline reset blurred inverse hidden strikethrough
#' @importFrom crayon black white silver red green blue yellow cyan magenta
#' @importFrom crayon bgBlack bgWhite bgRed bgGreen bgBlue bgYellow bgCyan bgMagenta
#'
#' @export
#'
#'
Print <- function(...) {
  tryCatch(
    {
      output <- glue::glue(..., .transformer = sprintf_transformer, .envir = parent.frame())
      output_color <- glue::glue_col(gsub("<<", "{", gsub(">>", "}", output)))
      print(output_color)
    },
    error = function(e) {
      warning(e)
      print(...)
    }
  )
}

Glue <- function(...) {
  output <- glue(..., .transformer = sprintf_transformer, .envir = parent.frame())
  output_color <- glue_col(gsub("<<", "{", gsub(">>", "}", output)))
  return(output_color)
}

sprintf_transformer <- function(text, envir) {
  text <- glue(text, .envir = envir)
  m <- regexpr(":.+$", text)
  if (m != -1) {
    format <- substring(regmatches(text, m), 2)
    regmatches(text, m) <- ""
    res <- eval(parse(text = text, keep.source = FALSE), envir)
    do.call(sprintf, list(glue("%{format}f"), res))
  } else {
    eval(parse(text = text, keep.source = FALSE), envir)
  }
}
