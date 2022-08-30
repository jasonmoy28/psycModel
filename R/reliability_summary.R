#' Reliability Analysis
#'
#' `r lifecycle::badge("stable")` \cr
#' First, it will determine whether the data is uni-dimensional or multi-dimensional using `parameters::n_factors()`. If the data is uni-dimensional, then it will print a summary
#' consists of alpha, G6, single-factor CFA, and descriptive statistics result. If it is multi-dimensional, it will print a summary consist of alpha, G6, omega result. You can
#' bypass this by specifying the dimensionality argument.
#'
#' @param data `data.frame`
#' @param cols items for reliability analysis.  Support `dplyr::select()` syntax.
#' @param descriptive_table Get descriptive statistics. Default is `TRUE`
#' @param digits number of digits to round to
#' @param dimensionality Specify the dimensionality. Either `uni` (uni-dimensionality) or `multi` (multi-dimensionality). Default is `NULL` that determines the dimensionality using EFA.
#' @param return_result If it is set to `TRUE` (default is `FALSE`), it will return `psych::alpha` for uni-dimensional scale, and `psych::omega` for multidimensional scale.
#' @param streamline print streamlined output
#' @param quite suppress printing output
#'
#' @return a `psych::alpha` object for unidimensional scale, and a `psych::omega` object for multidimensional scale.
#' @export
#'
#' @examples
#'
#' fit <- reliability_summary(data = lavaan::HolzingerSwineford1939, cols = x1:x3)
#' fit <- reliability_summary(data = lavaan::HolzingerSwineford1939, cols = x1:x9)
reliability_summary <- function(data,
                                cols,
                                dimensionality = NULL,
                                digits = 3,
                                descriptive_table = TRUE,
                                quite = FALSE,
                                streamline = FALSE,
                                return_result = FALSE) {
  cols <- data %>%
    dplyr::select(!!enquo(cols)) %>%
    names()
  data <- data %>% dplyr::select(dplyr::all_of(cols))
  ############################################ Unidimensionality Model ################################################################
  if (is.null(dimensionality)) { # check dimensionality to use
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    n_factor_df <- parameters::n_factors(data)
    n_factor <- getmode(n_factor_df$n_Factors)
    ifelse(test = n_factor == 1, yes = {
      dimensionality <- "uni"
    }, no = {
      dimensionality <- "multi"
    })
  }

  ############################################ Unidimensionality Model ################################################################
  if (dimensionality == "uni") {
    alpha_fit <- suppressMessages(data %>% psych::alpha())
    alpha_fit_measure <- alpha_fit$total[1:3]
    alpha_item_statistics <- alpha_fit$alpha.drop %>%
      dplyr::select("raw_alpha", "std.alpha", "G6(smc)") %>%
      tibble::rownames_to_column("Var") %>%
      dplyr::rename(Alpha = .data$raw_alpha) %>%
      dplyr::rename(Alpha.Std = .data$std.alpha) %>%
      dplyr::rename(`G6 (smc)` = .data$`G6(smc)`)
    names(alpha_fit_measure) <- c("Alpha", "Alpha.Std", "G6 (smc)")

    if (quite == FALSE) {
      if (streamline == FALSE) {
        super_print("underline|Model Summary")
        super_print("Model Type = Reliability Analysis")
        super_print("Dimensionality = {paste(dimensionality,'-dimensionality', sep = '')}")
        cat("\n")
      }
      super_print("underline|Composite Reliability Measures")
      print_table(alpha_fit_measure)
      cat("\n")
      super_print("underline|Item Reliability (item dropped)")
      print_table(alpha_item_statistics)
      cat("\n")
      super_print("CFA Model:")
      cfa_summary(
        data = data,
        dplyr::all_of(cols),
        model_variance = FALSE,
        model_covariance = FALSE,
        digits = digits,
        streamline = TRUE,
        plot = FALSE
      )
      cat("\n")
    }
  } else {
    ############################################ Multidimensionality Model ################################################################
    alpha_fit <- suppressMessages(data %>% psych::alpha())
    if (requireNamespace("GPArotation", quietly = TRUE)) {
      omega_fit <- data %>% psych::omega()
    } else {
      stop("Please install.packages('GPArotation') first")
    }

    composite_measure <- tibble::tibble(
      Alpha = round(alpha_fit$total["raw_alpha"], digits = digits),
      Alpha.Std = round(alpha_fit$total["std.alpha"], digits = digits),
      G.6 = omega_fit$G6,
      Omega.Hierarchical = omega_fit$omega_h,
      Omega.Total = omega_fit$omega.tot
    )

    alpha_item_statistics <- alpha_fit$alpha.drop %>%
      dplyr::select("raw_alpha", "std.alpha", "G6(smc)") %>%
      tibble::rownames_to_column("Var") %>%
      dplyr::rename(Alpha = .data$raw_alpha) %>%
      dplyr::rename(Alpha.Std = .data$std.alpha) %>%
      dplyr::rename(`G6 (smc)` = .data$`G6(smc)`)
    if (quite == FALSE) {
      if (streamline == FALSE) {
        super_print("underline|Model Summary")
        super_print("Model Type = Reliability Analysis")
        super_print("Dimensionality = {paste(dimensionality,'-dimensionality', sep = '')}")
        cat("\n")
      }
      super_print("underline|Composite Reliability Measures")
      print_table(composite_measure)
      cat("\n")
      super_print("underline|Item Reliability (item dropped)")
      print_table(alpha_item_statistics)
      cat("\n")
    }
  }

  if (descriptive_table == TRUE & quite == FALSE) {
    super_print("underline|Descriptive Statistics Table:")
    descriptive_table(
      data = data,
      cols = dplyr::all_of(cols),
      cor_digit = digits,
      descriptive_indicator_digit = digits,
      streamline = TRUE
    )
  }
  if (return_result == TRUE) {
    if (dimensionality == "uni") {
      return(alpha_fit)
    } else {
      return(omega_fit)
    }
  }
}
