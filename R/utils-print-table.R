#' print_table (internal use only)
#'
#' Rich-text fitted table printing in console
#'
#' @param data_frame data frame object
#' @param digits number of digits to round to
#'
#' @return none
#' @export
print_table <- function(data_frame, digits = 3) {
  data_frame <- data_frame %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ format_round(x = ., digits = digits))) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ as.character(.))) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ tidyr::replace_na(., replace = "NA")))

  # Calculate the white space need to insert for each cell
  column_name <- sapply(colnames(data_frame), function(x) {
    text_convert(x, type = "greek")
  }) %>% as.vector()
  colname_nchar <- nchar(column_name)
  colname_nchar_max <- nchar(column_name) %>%
    as.matrix() %>%
    t()
  data_frame_nchar_max <- apply(data_frame, 2, function(x) {
    max(nchar(x))
  }) %>%
    as.matrix() %>%
    t()
  combined_nchar_max <- rbind(colname_nchar_max, data_frame_nchar_max)
  combined_nchar_max <- apply(combined_nchar_max, 2, max) + 2
  data_frame_nchar <- apply(data_frame, 2, function(x) {
    nchar(x)
  })
  colname_white_space_insert <- combined_nchar_max - nchar(column_name)
  white_space_insert <- combined_nchar_max - data_frame_nchar
  combined_nchar_max_matrix <- matrix(combined_nchar_max) %>% t()

  if (nrow(data_frame) != 1) {
    for (i in 1:(nrow(data_frame_nchar) - 1)) {
      combined_nchar_max_matrix <- rbind(combined_nchar_max_matrix, combined_nchar_max_matrix[1, ])
    }
  }
  white_space_insert <- combined_nchar_max_matrix - data_frame_nchar
  linewidth <- sum(combined_nchar_max)
  # Output
  cat(paste(rep("\u2500", linewidth), collapse = "")) # print the first output line
  cat("\n")
  for (j in 1:length(column_name)) {
    # insert white-space to column name
    column_name[j] <- paste(paste(rep("\U00A0", times = colname_white_space_insert[j]), collapse = ""), column_name[j], sep = "")
  }
  # print column name
  output_column_name <- paste(column_name, collapse = "")
  cat(output_column_name)
  cat("\n")
  cat(paste(rep("\u2500", linewidth), collapse = "")) # print the second output line
  # insert whitespace to each row
  for (i in 1:nrow(data_frame)) {
    for (j in 1:ncol(data_frame)) {
      data_frame[i, j] <- paste(paste(rep("\U00A0", times = white_space_insert[i, j]), collapse = ""), data_frame[i, j], sep = "")
    }
    output_row <- paste(data_frame[i, ], collapse = "")
    cat("\n")
    # print row
    cat(output_row)
  }
  cat("\n")
  cat(paste(rep("\u2500", linewidth), collapse = "")) # print the last output line
}