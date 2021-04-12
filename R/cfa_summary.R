#' Confirmatory Factor Analysis
#'
#' `r lifecycle::badge("stable")` \cr
#' The function fits a CFA model using the lavaan::cfa (Rosseel, 2012) function. Users can fit single and multiple factors CFA (read details), and it also supports multilevel CFA (specifying the group). Users can pass the items (see below for example) or an explicit lavaan model for more versatile usage.
#'
#' @param data data frame
#' @param ... CFA items. Multi-factor CFA items should be separated by comma (as different argument). See below for example
#' @param model explicit lavaan model. Must be specify with `model = lavaan_model_syntax`.
#' @param group optional character. used for multi-level CFA. the nested variable for multilevel dataset (e.g., Country)
#' @param summary_item vector of fit indices. Default is CFI, RMSEA, TLI, and SRMR.
#' @param ordered logical. default is F. If it is set to T, lavaan will treat it as a ordinal variable and use DWLS instead of ML
#' @param return_result default is model. Options are 'model' (lavaan model), 'short_summary' (fit index summary only), 'long_summary' (lavaan full summary).
#' @param quite default as F. If set to true, it will not print the running model statement.
#' @param group_partial Items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').
#' @param jmv_result default is F. Print a good-lookng summary use jmv::cfa. It currently only support one-level CFA with latent factors and latent variable. It does not support complex lavaan structure at the momenet. I will work on supporting residual covariance. `r lifecycle::badge("experimental")` \cr
#'
#' @details
#' All argument must be explicitly specified. If not, all arguments will be treated as CFA items.
#'
#' @references
#' Bao, H.-W.-S. (2021). bruceR: Broadly useful convenient and efficient R functions. R package version 0.6.0. https://CRAN.R-project.org/package=bruceR
#'
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' Rosseel Y (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1–36. https://www.jstatsoft.org/v48/i02/.
#'
#' @export
#' @examples
#' # REMEMBER, YOU MUST NAMED ALL ARGUMENT EXCEPT THE CFA ITEMS ARGUMENT
#' # Fitting a single factor CFA model
#' fit = cfa_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   x1:x3,
#'   jmv_result = TRUE
#' )
#'
#' # Fitting a multilevel single factor CFA model
#' fit = cfa_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   x1:x3,
#'   group = "sex"
#' )
#'
#' # Fitting a multiple factor CFA model
#' fit = cfa_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   x1:x3,
#'   x4:x6,
#'   x7:x9,
#' )
#'
#' # Fitting a CFA model by passing explicit lavaan model (equivalent to the above model)
#' # Note in the below function how I added `model = ` in front of the lavaan model.
#' # Similarly, the same rule apply for all arguments (e.g., `ordered = F` instead of `F`)
#' fit = cfa_summary(
#'  model = "visual  =~ x1 + x2 + x3;textual =~ x4 + x5 + x6;",
#'  data = lavaan::HolzingerSwineford1939,
#'  jmv_result = TRUE
#' )
#' 
#' \dontrun{
#' # This will fail because I did not add `model = ` in front of the lavaan model.
#' # Therefore,you must add the tag in front of all arguments
#' # For example, `return_result = 'model'` instaed of `model`
#' cfa_summary("visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 ",
#'   data = lavaan::HolzingerSwineford1939
#' )
#' }
cfa_summary <- function(data,
                        ...,
                        model = NULL,
                        group = NULL,
                        summary_item = c("cfi", "rmsea", "tli", "srmr"),
                        ordered = F,
                        return_result = "model",
                        jmv_result = F, 
                        quite = F,
                        group_partial = NULL) {
  if (is.null(model)) { # construct model if explicit model is not passed
    items <- enquos(...)
    model <- ""
    index <- 1
    for (item in items) {
      cfa_items <- data %>%
        dplyr::select(!!item) %>%
        names()
      factor_name = paste("DV", index, sep = "")
      lavaan_lopp_model = paste(factor_name, " =~ ", paste(cfa_items, collapse = " + "), "\n ", sep = "")
      jmv_model = 
        model <- paste(model, lavaan_lopp_model)
      index <- index + 1
    }
  }
  
  # Print statement
  if (quite == F) {
    cat("Computing CFA using:\n", model)
  }
  # for long summary result, run CFA using bruceR if(return_result == 'bruceR_summary') {
  #   if (!is.null(group)) {
  #     warning('Group variable is ignored. Multilevel CFA is only supported with returning model or short_summary')
  #   }
  #
  #   }
  # }
  
  cfa_model <- lavaan::cfa(
    model = model,
    data = data,
    group = group,
    ordered = ordered,
    group.partial = group_partial
  )
  
  if (jmv_result == T) {
    if (!is.null(group)) {
      warning('Group variable is ignored for jmv result')
    }
    if (length(stringr::str_split(string = model,pattern = '=~')[[1]]) == 2) { 
      # single factor CFA
      jmv = NULL
      DV_loop = gsub(x = model,pattern = '=~.+',replacement = '') %>% stringr::str_trim()
      IV_loop = gsub(x = model,pattern = '.+=~',replacement = '') %>% stringr::str_trim() %>% gsub(pattern = ';',replacement = '')
      IV_loop = stringr::str_split(string = IV_loop, pattern = '\\+')[[1]] %>% stringr::str_trim()
      jmv_loop = list(label = DV_loop, vars = IV_loop)
      jmv = append(jmv,list(jmv_loop))
    } else { 
      # mutiple-factor CFA
      if (grepl(pattern = ';',model) == F & grepl(pattern = '\n',model) == T) { # seperated by \n
        split = stringr::str_split(string = model, pattern = '\n')[[1]]
      } else if (grepl(pattern = ';',model) == T & grepl(pattern = '\n',model) == F) { # seperated by ; 
        split = stringr::str_split(string = model, pattern = ';')[[1]]
      } else if(grepl(pattern = ';',model) == T & grepl(pattern = '\n',model) == T){
        stop("Why can't you just choose either ; or \\n? Why do you want to make my life harder?")
      } else if(grepl(pattern = ';',model) == F & grepl(pattern = '\n',model) == F){
        stop('You need to seperate the factor using ";" or "\\n"')
      } 
      DV = NULL
      IV = NULL
      jmv = NULL
      for (variable in split) {
        DV_loop = gsub(x = variable,pattern = '=~.+',replacement = '') %>% stringr::str_trim()
        IV_loop = gsub(x = variable,pattern = '.+=~',replacement = '') %>% stringr::str_trim()
        IV_loop = stringr::str_split(string = IV_loop, pattern = '\\+')[[1]] %>% stringr::str_trim()
        jmv_loop = list(label = DV_loop, vars = IV_loop)
        jmv = append(jmv,list(jmv_loop))
      }
    }
    # remove empty list object from jmv 
    delete_vector = c()
    for (i in c(1:length(lengths(jmv)))) {
      if (any(jmv[[i]] == '')) {
        delete_vector = c(delete_vector,i)
      }
    }
    jmv[delete_vector] = NULL
    
    # run jmv model
    jmv_model = jmv::cfa(data = data,factors = jmv,resCov = NULL,stdEst = T)
    print(jmv_model)
  }
 
  if (ordered == T) {
    summary_item <- paste(summary_item, ".scaled", sep = "")
  }
  ## return result 
  if (return_result == "model") {
    return(cfa_model)
  } else if (return_result == "short_summary") {
    cfa_short_summary <- lavaan::fitMeasures(cfa_model)[summary_item]
    return(cfa_short_summary)
  } else if (return_result == "long_summary") {
    lavaan::summary(cfa_model, fit.measure = T, standardized = T)
  } else if(return_result == 'jmv_summary')
    return_result(jmv_model)
}
