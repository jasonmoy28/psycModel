#' Convert HTML to PDF
#'
#' `r lifecycle::badge("experimental")` \cr
#'  This is a helper function for knitting Rmd. Due to technological limitation, the output cannot knit to PDF in Rmd directly (the problem is with the latex engine printing unicode character).  
#'  Therefore, to bypass this problem, you will first need to knit to html file first, then use this function to convert it to a PDF file. 
#'
#' @param file_path file path to the HTML file (can be relative if you are in a R project)
#' @param dir file path to the directory of all HTML files (can be relative if you are in a R project)
#' @param scale the scale of the PDF
#' @param render_exist overwrite exist PDF. Default is `FALSE`
#'
#' @return no return value
#' @export
#'
#' @examples
#' \dontrun{
#' html_to_pdf(file_path = "html_name.html")
#' # all HTML files in the my_html_folder will be converted
#' html_to_pdf(dir = "Users/Desktop/my_html_folder")
#' }
html_to_pdf <- function(file_path = NULL,
                        dir = NULL,
                        scale = 1,
                        render_exist = FALSE) {
  if (requireNamespace("pagedown", quietly = TRUE)) {
    pagedown::find_chrome()
    if (!is.null(file_path)) {
      pagedown::chrome_print(input = file_path, options = list(scale = scale))
    } else if (!is.null(dir)) {
      files <- list.files(dir)
      if (render_exist == FALSE) {
        pdf_files <- files[stringr::str_detect(pattern = ".pdf", string = files)]
        pdf_cleaned <- stringr::str_replace(pdf_files, ".pdf", "")
        files <- files[stringr::str_detect(pattern = ".html", string = files)]
        files <- files[!stringr::str_detect(pattern = pdf_cleaned, string = files)]
      }
      files <- files[stringr::str_detect(pattern = ".html", string = files)]
      print(paste("Rendering", length(files), "files"))
      for (file in files) {
        file_path <- paste(dir, "/", file, sep = "")
        skip_to_next <- FALSE
        tryCatch(pagedown::chrome_print(file_path), error = function(e) {
          skip_to_next <<- TRUE
        })

        if (skip_to_next) {
          next
        }
      }
    } else {
      stop("Either file_path or dir must be specified")
    }
  } else {
    stop("Please install.packages('pagedown')")
  }
}
