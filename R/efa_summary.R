#' Exploratory Factor Analysis
#'
#' `r lifecycle::badge("stable")` \cr
#' The function fits a exploratory factor analysis model using the jmv::efa (The Jamovi Project, 2021) function. Users can fit the model by passing items using dplyr::select syntax
#'
#' @param data data frame
#' @param cols columns. dplyr::select syntax.
#' @param rotation the rotation to use in estimation. Default is oblimin. Options are 'none', 'varimax', 'quartimax', 'promax', 'oblimin', or 'simplimax'
#' @param fit_measures show model fit measures and test. Default is `T`
#' @param factor_summary show factor summary showing the explained variance for each factor. Default is `T`
#' @param bartlett_test show Bartlett's test of sphericity result (you want this to be significant). Default is `T`
#' @param kmo_test show Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy (MSA) results (you want the overall MSA to be higher than 0.7). Default is `T`
#' @param scree_plot show explained variance by number of factor plot. default is `T`.
#' @param n_factors the number of factors in the model. Default is data-driven estimation of most appropriate number of factors 
#'
#' @details
#' As a thumb of rule, you want factor loadings to be above 0.5.
#' @references
#' The Jamovi Project. (2021). jmv: The 'jamovi' Analyses. R package version 1.2.23. Retrieved from https://github.com/jamovi/jmv.
#' @export
#'
#' @examples
#' efa_summary(lavaan::HolzingerSwineford1939, starts_with("x"), scree_plot = TRUE)
#' 
efa_summary <- function(data,
                        cols,
                        rotation = "oblimin",
                        n_factors = "default",
                        scree_plot,
                        fit_measures = T,
                        factor_summary = T,
                        bartlett_test = T,
                        kmo_test = T) {
  data <- data %>% dplyr::select(!!enquo(cols))
  if (n_factors == "default") {
    n_factors <- 1
    n_factor_method <- "parallel" # if n_factors is inputted
  } else {
    n_factor_method <- "fixed" # default behavior
  }

  efa_model <- jmv::efa(
    data = data,
    rotation = rotation,
    nFactors = n_factors,
    bartlett = bartlett_test,
    kmo = kmo_test,
    modelFit = fit_measures,
    factorSummary = factor_summary,
    nFactorMethod = n_factor_method
  )
  if (scree_plot == T) {
    plot <-
      efa_model$factorStats$factorSummary$asDF %>%
      tidyr::pivot_longer(cols = c(.data$varProp, .data$varCum), names_to = "Type", values_to = "explained_variance") %>%
      dplyr::mutate(explained_variance = round(.data$explained_variance, 0)) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$comp, y = .data$explained_variance, group = .data$Type)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(linetype = .data$Type)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::labs(y = "Explained Variance", x = "Factor #") +
      ggplot2::scale_linetype_discrete(labels = c("Cumulative Exp. Var.", "Single Factor Exp. Var.")) +
      ggplot2::ylim(0, 100) +
      ggplot2::geom_text(ggplot2::aes(label = paste(.data$explained_variance, "%", sep = ""), vjust = -1))
    print(plot)
  }

  return(efa_model)
}
