#' Correlation table
#'
#' This function uses the psych::corr.test (Revelle, 2021) function to generated the correlation. One potential problem with the current version of the function is that if the correlation between the two item is too high (rounded as 1), then that cell will become blank. This is a very rare problem, and I have not yet figure out how to solve this.
#' @param data a dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be recoded.
#' @param digit number of digits
#' @param sig_test adjusted or raw. Default as adjusted. See psych::corr.test to learn more.
#'
#' @export
#' @references 
#' Revelle, W. (2021). psych: Procedures for Psychological, Psychometric, and Personality Research. Northwestern University, Evanston, Illinois. R package version 2.1.3, https://CRAN.R-project.org/package=psych.
#' 
#' Moy, J. H. (2021). psycModel: Intergrated Toolkit for Pyschological Analysis and Modelling in R. Retrieved from https://github.com/jasonmoy28/psycModel.
#'
#' @examples
#' cor_test(iris,1:4)
#'

cor_test =  function(data,cols,sig_test = 'raw',digit = 3,...) {
  cols = ggplot2::enquo(cols)
  data = data %>% dplyr::select(!!cols)

  datatype = as.vector(sapply(data, class))
  if(all(datatype == 'numeric'| datatype == 'factor' | datatype == 'integer')){
    data = data %>% dplyr::mutate(dplyr::across(!!cols,as.numeric))
  } else{
    print('Error: All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()')
    return()
  }

  cor_test_df = data %>%
    dplyr::mutate(dplyr::across(!!cols,as.numeric)) %>%
    psych::corr.test()

  cor_df_raw = as.data.frame(cor_test_df$r) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ format(round(., digit),nsmall = digit)))

  cor_df = cor_df_raw %>%
    dplyr::mutate(rowname = colnames(cor_df_raw)) %>%
    dplyr::select('rowname',tidyr::everything())


  sig_df = tidyr::as_tibble(cor_test_df$p) %>%
    dplyr::mutate(dplyr::across(tidyr::everything(), ~
                                  dplyr::case_when(. < 0.001 ~ '***',
                                                   . < 0.01 & . >= 0.001 ~ '**',
                                                   . < 0.05 & . >= 0.01 ~ '*',
                                                   T ~ '')))


  for (i in c(1:ncol(sig_df))) {
    c_vec = stringr::str_c(cor_df[[i + 1]], sig_df[[i]])
    cor_df[[i + 1]] = c_vec
  }
  if (sig_test == 'raw') {
    for (columns in colnames(cor_df)) {
      if (columns != 'rowname') { # skip the first column
        diagonal_value = which(grepl(paste(cor_df_raw[1,1],'\\*+',sep = ''),cor_df[,columns]))
        cor_df[1:diagonal_value,columns] = ''
      }
    }
  } else if (sig_test == 'adjusted'){
    for (columns in colnames(cor_df)) {
      if (columns != 'rowname') { # skip the first column
        diagonal_value = which(grepl(paste(cor_df_raw[1,1],'\\*+',sep = ''),cor_df[,columns]))
        cor_df[diagonal_value:nrow(cor_df),columns] = ''
      }
    }
  }
  dots = list(...)
  if (length(dots) > 0) {
    if (dots$descriptive_table_use == T){
      return(cor_df)
    }
  }

  cor_df = cor_df %>% dplyr::select(-rowname)

    # printing warning meesage, non-essential block
  coreced_name = NULL
  coreced_name = data %>% dplyr::select(!where(is.numeric)) %>% names(.)
  if (length(coreced_name) != 0) {
    warning_message = paste(paste(coreced_name, collapse = ', '),'were coreced into numeric')
    warning(warning_message)
  }
  return(cor_df)
}

