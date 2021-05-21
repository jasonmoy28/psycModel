#' Knit Rmd Files Instruction
#'
#' This is a helper function that instruct users of the package how to knit a R Markdown (Rmd) files
#' @return no return value
#' @export
#'
#' @examples knit_to_Rmd()
knit_to_Rmd <- function() {
  if (requireNamespace("fansi", quietly = TRUE)) {
    super_print('green|OK. Required package "fansi" is installed')
    cat("\n")
    super_print('Note: To knit Rmd to HTML, add the following line to the setup chunk of your Rmd file: \n \U00A0"old.hooks <- fansi::set_knit_hooks(knitr::knit_hooks)"')
    cat("\n")
    super_print("Note: Use html_to_pdf to convert it to PDF. See ?html_to_pdf for more info")
  } else {
    prompt <- readline("Require fansi to knit Rmd. Install fansi? Y/N ")
    if (prompt == "Y" | prompt == "y") {
      utils::install.packages("fansi")
    } else {
      cat("Install package halt. Nothing is installed")
    }
  }
}
