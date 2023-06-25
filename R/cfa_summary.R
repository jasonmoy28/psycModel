#' Confirmatory Factor Analysis
#'
#' `r lifecycle::badge("stable")` \cr
#' The function fits a CFA model using the `lavaan::cfa()`. Users can fit single and multiple factors CFA, and it also supports multilevel CFA (by specifying the group).
#' Users can fit the model by passing the items using `dplyr::select()` syntax or an explicit lavaan model for more versatile usage.
#' All arguments (except the CFA items) must be explicitly named (e.g., model = your-model; see example for inappropriate behavior).
#'
#' @param data data frame
#' @param ... CFA items. Multi-factor CFA items should be separated by comma (as different argument). See below for examples. Support `dplyr::select()` syntax.
#' @param model explicit `lavaan` model. Must be specify with `model = lavaan_model_syntax`. `r lifecycle::badge("experimental")`
#' @param group optional character. used for multi-level CFA. the nested variable for multilevel dataset (e.g., Country). Support `dplyr::select()` syntax.
#' @param ordered Default is `FALSE`. If it is set to `TRUE`, `lavaan` will treat it as a ordinal variable and use `DWLS` instead of `ML`
#' @param group_partial Items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').
#' @param digits number of digits to round to
#' @param return_result  If it is set to `TRUE`, it will return the `lavaan` model
#' @param quite suppress printing output
#' @param model_covariance print model covariance. Default is `TRUE`
#' @param model_variance print model variance. Default is `TRUE`
#' @param streamline print streamlined output
#' @param plot print a path diagram. Default is `TRUE`
#' @param estimator estimator for lavaan. Default is `ML`
#'
#' @return a `lavaan` object if return_result is `TRUE`
#' @details
#' First, just like researchers have argued against p value of 0.05 is not a good cut-of, researchers have also argue against that fit indicies (more importantly, the cut-off criteria) are not completely representative of the goodness of fit.
#' Nonetheless, you are required to report them if you are publishing an article anyway. I will summarize the general recommended cut-off criteria for CFA model below.
#' Researchers consider models with CFI (Bentler, 1990) that is > 0.95 to be excellent fit (Hu & Bentler, 1999), and > 0.9 to be acceptable fit. Researchers considered a model is excellent fit if CFI > 0.95 (Hu & Bentler, 1999), RMSEA < 0.06 (Hu & Bentler, 1999), TLI > 0.95, SRMR < 0.08.
#' The model is considered an acceptable fit if CFI > 0.9 and RMSEA < 0.08. I need some time to find all the relevant references, but this should be the general consensus.
#'
#' @references
#' Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6, 1–55. https://doi.org/10.1080/10705519909540118
#'
#'
#' @export
#' @examples
#' # REMEMBER, YOU MUST NAMED ALL ARGUMENT EXCEPT THE CFA ITEMS ARGUMENT
#' # Fitting a multilevel single factor CFA model
#' fit <- cfa_summary(
#'   data = lavaan::HolzingerSwineford1939,
#'   x1:x3,
#'   x4:x6,
#'   x7:x9,
#'   group = "sex",
#'   model_variance = FALSE, # do not print the model_variance
#'   model_covariance = FALSE # do not print the model_covariance
#' )
#'
#'
#' # Fitting a CFA model by passing explicit lavaan model (equivalent to the above model)
#' # Note in the below function how I added `model = ` in front of the lavaan model.
#' # Similarly, the same rule apply for all arguments (e.g., `ordered = FALSE` instead of just `FALSE`)
#' \donttest{
#' fit <- cfa_summary(
#'   model = "visual  =~ x1 + x2 + x3",
#'   data = lavaan::HolzingerSwineford1939,
#'   quite = TRUE # silence all output
#' )
#' }
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
                        ordered = FALSE,
                        digits = 3,
                        estimator = 'ML',
                        model_covariance = TRUE,
                        model_variance = TRUE,
                        plot = TRUE,
                        group_partial = NULL,
                        streamline = FALSE,
                        quite = FALSE,
                        return_result = FALSE) {
  if (is.null(model)) { # construct model if explicit model is not passed
    items <- enquos(...)
    model <- ""
    index <- 1
    for (item in items) {
      cfa_items <- data %>%
        dplyr::select(!!item) %>%
        names()
      factor_name <- paste("DV", index, sep = "")
      lavaan_loop_model <- paste(factor_name, " =~ ", paste(cfa_items, collapse = " + "), "\n ", sep = "")
      index <- index + 1
      model <- paste(model, lavaan_loop_model)
    }
  }

  group <- data %>%
    dplyr::select(!!enquo(group)) %>%
    names()
  if (length(group) == 0) {
    group <- NULL
  }
  
  cfa_model <- lavaan::cfa(
    model = model,
    estimator = estimator,
    data = data,
    group = group,
    ordered = ordered,
    group.partial = group_partial
  )

  ############################################### Get Output from Lavaan ###################################################################
  if (quite == FALSE) {
    fit_indices <- c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr", "tli", "aic", "bic", "bic2")
    if (ordered == TRUE) {
      fit_indices <- c("chisq", "df", "pvalue", "cfi", "rmsea", "tli")
      fit_indices <- paste(fit_indices, ".scaled", sep = "")
    }

    fit_measure_df <- data.frame(
      variable = names(lavaan::fitMeasures(cfa_model)[fit_indices]),
      fit_measure = lavaan::fitmeasures(cfa_model)[fit_indices]
    ) %>%
      tidyr::pivot_wider(names_from = .data$variable, values_from = .data$fit_measure) %>%
      dplyr::rename(p = .data$pvalue) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ format_round(., digits = digits))) %>%
      dplyr::rename("$chi$^2" = .data$chisq)

    colnames(fit_measure_df) <- stringr::str_to_upper(colnames(fit_measure_df))

    standardized_df <- lavaan::standardizedsolution(cfa_model, output = "data.frame")

    factors_loadings_df <- standardized_df %>%
      dplyr::filter(.data$op == "=~") %>%
      dplyr::select(-"op") %>%
      dplyr::rename(Latent.Factor = "lhs") %>%
      dplyr::rename(Observed.Var = "rhs") %>%
      dplyr::rename(Std.Est = "est.std") %>%
      dplyr::rename(SE = "se") %>%
      dplyr::rename(Z = "z") %>%
      dplyr::rename(P = "pvalue") %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ format_round(x = ., digits = 3)))

    if (is.null(group)) { # no group variable
      factors_loadings_df <- factors_loadings_df %>%
        dplyr::mutate(dplyr::across(.data$Latent.Factor, ~ replace(., duplicated(.), "")))
    } else { # yes group variable
      factors_loadings_df <- factors_loadings_df %>%
        dplyr::mutate(group = as.integer(group)) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(dplyr::across(.data$Latent.Factor, ~ replace(., duplicated(.), ""))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(dplyr::across(group, ~ replace(., duplicated(.), "")))
    }

    covariance_df <- standardized_df %>%
      dplyr::filter(.data$op == "~~") %>%
      dplyr::filter(!.data$lhs == .data$rhs) %>%
      dplyr::select(-"op") %>%
      dplyr::rename(Var.1 = "lhs") %>%
      dplyr::rename(Var.2 = "rhs") %>%
      dplyr::rename(Est = "est.std") %>%
      dplyr::rename(SE = "se") %>%
      dplyr::rename(Z = "z") %>%
      dplyr::rename(P = "pvalue")

    variance_df <- standardized_df %>%
      dplyr::filter(.data$op == "~~") %>%
      dplyr::filter(.data$lhs == .data$rhs) %>%
      dplyr::select(-"lhs") %>%
      dplyr::select(-"op") %>%
      dplyr::rename(Var = "rhs") %>%
      dplyr::rename(Est = "est.std") %>%
      dplyr::rename(SE = "se") %>%
      dplyr::rename(Z = "z") %>%
      dplyr::rename(P = "pvalue")

    ################################################## Model Output ###################################################################
    if (streamline == FALSE) {
      cat("\n \n")
      super_print("underline|Model Summary")
      super_print("Model Type = Confirmatory Factor Analysis")
      super_print('Estimator: {estimator}')
      if (length(group) != 0) {
        super_print("Group = {group}")
      }
      super_print("Model Formula = \n.{model}")
    }
    if (nrow(factors_loadings_df) > 3) {
      super_print("underline|Fit Measure")
      print_table(fit_measure_df) 
      cat("\n \n")
    } else{
      message('Fit measure is not printed due to factor <= 3 ')
    }

    super_print("underline|Factor Loadings")
    print_table(factors_loadings_df)

    if (streamline == FALSE) {
      if (length(row.names(covariance_df)) != 0 & model_covariance == TRUE) {
        cat("\n \n")
        super_print("underline|Model Covariances")
        print_table(covariance_df)
      }

      if (length(row.names(variance_df)) != 0 & model_variance == TRUE) {
        cat("\n \n")
        super_print("underline|Model Variance")
        print_table(variance_df)
      }
    }
    ########################################## Goodness of Fit ###################################################
    if (nrow(factors_loadings_df) > 3) {
      if (ordered == FALSE) {
        cat("\n \n")
        super_print("Goodness of Fit:")
        P <- fit_measure_df["P"]
        if (P >= 0.05 & !is.na(P)) {
          super_print("green| OK. Excellent $chi$^2 fit (p > 0.05)")
        } else if (P < 0.05 & !is.na(P)) {
          super_print("yellow| Warning. Poor $chi$^2 fit (p < 0.05). It is common to get p < 0.05. Check other fit measure.")
        }

        CFI <- fit_measure_df["CFI"]
        if (CFI >= 0.95) {
          super_print("green| OK. Excellent CFI fit (CFI > 0.95)")
        } else if (CFI >= 0.9) {
          super_print("green| OK. Acceptable CFI fit (CFI > 0.90)")
        } else if (CFI < 0.9) {
          super_print("red| Warning. Poor CFI fit (CFI < 0.90)")
        }

        RMSEA <- fit_measure_df["RMSEA"]
        if (RMSEA <= 0.05) {
          super_print("green| OK. Excellent RMSEA fit (RMSEA < 0.05)")
        } else if (RMSEA <= 0.08) {
          super_print("green| OK. Acceptable RMSEA fit (RMSEA < 0.08)")
        } else if (RMSEA > 0.08) {
          super_print("red| Warning. Poor RMSEA fit (RMSEA > 0.08)")
        }

        SRMR <- fit_measure_df["SRMR"]
        if (SRMR <= 0.08) {
          super_print("green| OK. Good SRMR fit (SRMR < 0.08)")
        } else if (SRMR > 0.08) {
          super_print("red| Warning. Poor SRMR fit (SRMR > 0.08)")
        }

        TLI <- fit_measure_df["TLI"]
        if (TLI >= 0.95) {
          super_print("green| OK. Excellent TLI fit (TLI > 0.95)")
        } else if (TLI >= 0.9) {
          super_print("green| OK. Acceptable TLI fit (TLI > 0.90)")
        } else if (TLI < 0.9) {
          super_print("red| Warning. Poor TLI fit (TLI < 0.90)")
        }

        if (all(factors_loadings_df$Std.Est >= 0.7)) {
          super_print("green| OK. Excellent factor loadings (all loadings > 0.7)")
        } else if (any(factors_loadings_df$Std.Est < 0.7) & all(factors_loadings_df$Std.Est >= 0.4)) {
          super_print("yellow| OK. Barely acceptable factor loadings (0.4 < some loadings < 0.7)")
        } else if (any(factors_loadings_df$Std.Est < 0.4)) {
          super_print("red| Warning. Some poor factor loadings (some loadings < 0.4)")
        }
      } else {
        print("No recommended cut-off criteria is avaliable for CFA fitted using DWLS (non-continuous variable). See ?cfa_summary details.")
      }
    }
  } # quite == FALSE
  if (plot == TRUE) {
    if (requireNamespace("semPlot", quietly = TRUE)) {
      semPlot::semPaths(cfa_model,
        what = "std",
        edge.color = "black",
        sizeMan = 5,
        sizeLat = 8,
        edge.label.cex = 1,
        nCharEdges = 5,
        esize = 1,
        trans = 1
      )
    } else {
      message("Please install.packages('semPlot') for path diagram")
    }
  }
  ## return result
  if (return_result == TRUE) {
    return(cfa_model)
  }
}
