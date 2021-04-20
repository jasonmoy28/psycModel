#' Data Check (internal use only)
#'
#' @param data data frame
#' @keywords internal
#' @return a data frame with all columns converted to numeric 

data_check <- function(data) {
  datatype <- as.vector(sapply(data, class))
  if (any(class(data) == "NULL")) {
    return(data)
  } else if (all(datatype == "numeric" | datatype == "factor" | datatype == "integer")) {
    non_numeric_columns <- data %>%
      dplyr::select(where(~ !is.numeric(.))) %>%
      names()
    data <- data %>% dplyr::mutate_all(as.numeric)
    if (length(non_numeric_columns) > 0) {
      warning(paste("The following columns are coerced into numeric:", paste(non_numeric_columns, collapse = ", ")))
    }
    return(data)
  } else {
    stop("All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()")
  }
}
