#' Convert HTML to PDF
#'
#' `r lifecycle::badge("stable")` \cr
#' This is a helper function for knitting Rmd. Due to technological limitation, the output cannot knit to PDF in Rmd directly. It uses the pagedown::chrome_print() in the backend.
#'  You must first knit to HTML, then you can use this function to covert them to PDF if you wish. I am sorry. I can't find any way to implement it.
#'  If you know how to implement it, please submit an issue on github to let me know.
#'
#' @param file_path file path to the HTML file (can be relative if you are in a R project)
#' @param dir file path to the directory of all HTML files (can be relative if you are in a R project)
#' @param render_exist overwrite exist PDF. Default is `F` 
#'
#' @export
#'
html_to_pdf <- function(file_path = NULL,
                        dir = NULL, 
                        render_exist = F) {
  if (requireNamespace("pagedown", quietly = TRUE)) {
    if (!is.null(file_path)) {
      pagedown::chrome_print(input = file_path)
    } else if (!is.null(dir)) {
      files <- list.files(dir)
      if (render_exist == F) {
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
    stop("please install.packages('pagedown')")
  }
}
