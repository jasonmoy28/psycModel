#' Exploratory Factor Analysis
#'
#' `r lifecycle::badge("stable")` \cr
#' The function is used to fit a exploratory factor analysis model. It will first find the optimal number of factors using parameters::n_factors. Once the optimal number of factor is determined, the function will fit the model using
#' `psych::fa()`. Optionally, you can request a post-hoc CFA model based on the EFA model which gives you more fit indexes (e.g., CFI, RMSEA, TLI)
#'
#' @param data data frame
#' @param cols columns. Support `dplyr::select()` syntax.
#' @param rotation the rotation to use in estimation. Default is 'oblimin'. Options are 'none', 'varimax', 'quartimax', 'promax', 'oblimin', or 'simplimax'
#' @param n_factor number of factors for EFA. It will bypass the initial optimization algorithm, and fit the EFA model using this specified number of factor
#' @param efa_plot show explained variance by number of factor plot. default is `TRUE`.
#' @param digits number of digits to round to
#' @param optimal_factor_method Show a summary of the number of factors by optimization method (e.g., BIC, VSS complexity, Velicer's MAP)
#' @param post_hoc_cfa a CFA model based on the extracted factor
#' @param return_result If it is set to `TRUE` (default is `FALSE`), it will return a `fa` object from `psych`
#' @param streamline print streamlined output
#' @param quite suppress printing output
#'
#' @return a `fa` object from `psych`
#'
#' @export
#'
#' @examples
#' efa_summary(lavaan::HolzingerSwineford1939, starts_with("x"), post_hoc_cfa = TRUE)
efa_summary <- function(data,
                        cols,
                        rotation = "varimax",
                        optimal_factor_method = FALSE,
                        efa_plot = TRUE,
                        digits = 3,
                        n_factor = NULL,
                        post_hoc_cfa = FALSE,
                        quite = FALSE,
                        streamline = FALSE,
                        return_result = FALSE) {
  data <- data %>% dplyr::select(!!enquo(cols))
  ######################################## Optimal Factor ##########################################################
  if (is.null(n_factor)) {
    if (requireNamespace("nFactors", quietly = TRUE)) {
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      n_factor_df <- parameters::n_factors(data, rotation = rotation)
      n_factor_count <- n_factor_df$n_Factors %>%
        tibble::as_tibble_col() %>%
        dplyr::rename("Optimal Factor #" = .data$value)
      n_factor <- getmode(n_factor_df$n_Factors)
      n_factor_output <- n_factor_df$Method %>%
        tibble::as_tibble_col() %>%
        dplyr::rename("Method" = .data$value) %>%
        dplyr::bind_cols(n_factor_count)
    } else {
      stop("Please install.packages('nFactors') for finding the optimal number of EFA factors")
    }
  } else {
    n_factor_output <- NULL
  }

  ######################################## Factor Analysis ##########################################################
  efa_result <- data %>% psych::fa(nfactors = n_factor, rotate = rotation)
  efa_loadings <- parameters::model_parameters(efa_result) %>%
    dplyr::mutate(dplyr::across(tidyselect::contains("MR"), function(x) {
      dplyr::if_else(x < 0.4, true = "", false = as.character(format_round(x, digits = digits)))
    }))

  colnames(efa_loadings) <- stringr::str_replace_all(string = colnames(efa_loadings), pattern = "MR", replacement = "Factor ")

  efa_variance <-
    as.data.frame(efa_result$Vaccounted) %>%
    tibble::rownames_to_column(var = "Var")
  colnames(efa_variance) <- stringr::str_replace_all(string = colnames(efa_variance), pattern = "MR", replacement = "Factor ")

  factor_structure_test <- parameters::check_factorstructure(data)
  sphericity_p <- factor_structure_test$sphericity$p

  if (sphericity_p < 0.001) {
    sphericity_p_output <- "p < 0.001"
  } else {
    sphericity_p_format <- format_round(sphericity_p, digits)
    sphericity_p_output <- glue::glue("p = {sphericity_p_format}")
  }
  sphericity_chi <- format_round(factor_structure_test$sphericity$chisq, digits)
  sphericity_df <- factor_structure_test$sphericity$dof

  KMO_MSA_var <- factor_structure_test$KMO$MSA_variable %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Var") %>%
    dplyr::rename(`KMO Value` = .data$.)

  KMO_MSA_overall <- as.numeric(format_round(factor_structure_test$KMO$MSA, digits = digits))

  KMO_MSA_table <- factor_structure_test$KMO$MSA %>%
    tibble::as_tibble_col(column_name = "KMO Value") %>%
    tibble::add_column(Var = "Overall") %>%
    dplyr::bind_rows(KMO_MSA_var) %>%
    dplyr::select("Var", "KMO Value")
  #################################################### Output Model ##############################################
  if (quite == FALSE) {
    if (streamline == FALSE) {
      cat("\n \n \n")
      super_print("underline|Model Summary")
      super_print("Model Type = Exploratory Factor Analysis")
      super_print("Optimal Factors = {n_factor}")
      cat("\n")
    }
    super_print("underline|Factor Loadings")
    print_table(efa_loadings)
    cat("\n")
    cat("\n")
    super_print("underline|Explained Variance")
    print_table(efa_variance)
    cat("\n")
    cat("\n")

    if (!is.null(n_factor_output) & optimal_factor_method == TRUE) {
      super_print("underline|Optimal Factor by Method")
      print_table(n_factor_output, digits = 0)
      cat("\n")
    }

    super_print("EFA Model Assumption Test:")
    if (sphericity_p < 0.05) {
      super_print("green|OK. Bartlett's test of sphericity suggest the data is appropriate for factor analysis. $chi$^2({sphericity_df}) = {sphericity_chi}, {sphericity_p_output}")
    } else {
      super_print("red|Warning. Bartlett's test of sphericity suggest the data is not appropriate for factor analysis. $chi$^2({sphericity_df}) = {sphericity_chi}, {sphericity_p_output})")
    }

    if (KMO_MSA_overall > 0.7) {
      super_print("green|OK. KMO measure of sampling adequacy suggests the data is appropriate for factor analysis. KMO = {KMO_MSA_overall}")
    } else {
      super_print("red|Warning. KMO measure of sampling adequacy suggests the data is not appropriate for factor analysis. KMO = {KMO_MSA_overall}")
    }
    cat("\n")
    super_print("underline|KMO Measure of Sampling Adequacy")
    print_table(KMO_MSA_table, digits = digits)
    cat("\n")
  }
  if (efa_plot == TRUE) {
    plot <- efa_variance %>%
      dplyr::filter(.data$Var %in% c("Proportion Var")) %>%
      tidyr::pivot_longer(cols = dplyr::contains("Factor")) %>%
      dplyr::mutate(value = as.numeric(format_round(.data$value * 100, digits = 0))) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$name, y = .data$value, fill = .data$Var)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.4) +
      ggplot2::labs(y = "Proportion of Explained Variance", x = "Factor #") +
      ggplot2::ylim(0, 100) +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = c("#2171b5", "#6baed6")) +
      ggplot2::geom_text(ggplot2::aes(label = paste(.data$value, "%", sep = "")), position = ggplot2::position_dodge(width = 0.9), vjust = -0.25) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
        legend.position = "none"
      )
    print(plot)
  }
  # run CFA
  if (post_hoc_cfa == TRUE) {
    cfa_model <- parameters::efa_to_cfa(efa_result)
    cfa_model <- cfa_model %>% stringr::str_replace_all(pattern = "MR", replacement = "Factor.")
    cat("\n")
    super_print("underline|Post-hoc CFA Model Summary")
    cat("\n")
    cfa_summary(
      data = data,
      model = cfa_model,
      model_variance = FALSE,
      streamline = TRUE
    )
  }
  if (return_result == TRUE) {
    return(efa_result)
  }
}
