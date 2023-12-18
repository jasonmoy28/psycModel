#' Two-way Interaction Plot
#'
#' `r lifecycle::badge("superseded")` \cr
#' The function creates a three-way interaction plot. By default, it will create an interaction plot with -1 SD and +1 SD of the two continuous variables, or the two levels of the binary variables or dummy-coded multi-categorical variable
#' It has been superseded by \code{\link{interaction_plot}}.
#'
#'
#' @param model a regression model object from \link[effects]{effect}.
#' @param interaction_term default is the first interaction term in the model. The term should be given explicitly if you want to plot other interaction terms.
#' @param response_var_name The name of the response variable can be changed using this value. 
#' @param predict_var1_name The name of the first predictor can be changed using this value. 
#' @param predict_var2_name The name of the second predictor can be changed using this value. 
#' @param predict_var1_level default is the -1 SD and +1 SD for continuous variable. They can be changed using this value.
#' @param predict_var2_level default is the -1 SD and +1 SD for continuous variable. They can be changed using this value.
#' @param predict_var1_level_name The labels of the level can be change using this value (e.g., `c('-1 SD','+1 SD')`). The order should be from the left to right on the x-axis. 
#' @param predict_var2_level_name The labels of the level can be change using this value (e.g., `c('-1 SD','+1 SD')`). The order should be from the top to down on the legend. 
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color default if `FALSE`. Set to `TRUE` if you want to plot in color
#' @param return_plot_data default is `FALSE`. Set to `TRUE` to return the plot data.
#' @param return_plot default is `FALSE`. Set to `TRUE` to return the plot.
#' @param print_plot default is `TRUE`. Set to `TRUE` to print the plot. 
#' @param verbose deafult is `TRUE`.
#' @param data Optional data.frame. Only used when it is not possible to extract data from the model object. 
#'
#' @details It appears that ``predict` cannot handle categorical factors. All variables are converted to numeric before plotting.
#' @return an object of class `ggplot`
#' @export
#'
#' @examples
#' lm_fit <- lm(Sepal.Length ~ Sepal.Width * Petal.Width,
#'   data = iris
#' )
#' 
#' two_way_interaction_plot(lm_fit)
#'
two_way_interaction_plot <- function(model,
                                     interaction_term = NULL,
                                     response_var_name = NULL,
                                     predict_var1_name = NULL,
                                     predict_var2_name = NULL,
                                     predict_var1_level = NULL,
                                     predict_var2_level = NULL,
                                     predict_var1_level_name = NULL,
                                     predict_var2_level_name = NULL,
                                     y_lim = NULL,
                                     plot_color = FALSE,
                                     return_plot_data = FALSE,
                                     return_plot = FALSE,
                                     verbose = FALSE,
                                     print_plot = TRUE,
                                     data = NULL) {
  model_data <- NULL
  model_data <- insight::get_data(model)
  predict_var <- model %>%
    insight::find_predictors() %>%
    .$conditional # maybe problem with unconditional?
  
  response_var <- model %>% insight::find_response()
  
  if (is.null(interaction_term)) {
    interaction_term <- model %>%
      insight::find_interactions() %>%
      .$conditional %>% 
      .[1]
  }
  
  
  # get variable from model
  if (length(interaction_term) == 0) {
    stop("No two-way interaction term is found in the model")
  }
  
  predict_var1 <- gsub(pattern = ":.+", "", x = interaction_term)
  predict_var2 <- gsub(pattern = ".+:", "", x = interaction_term)
  
  
  if (any(class(model_data) == "data.frame")) {
    data <- model_data
  } else {
    if (is.null(data)) {
      stop("You need to pass the data directly")
    }
    if (!any(class(data) == "data.frame")) {
      stop("Data must be dataframe-like object")
    }
  }
  
  if (is.null(predict_var1_level)) {
    if (length(unique(model_data[[predict_var1]])) != 2) {
      predict_var1_level = c(round(mean(model_data[[predict_var1]],na.rm = T) - sd(model_data[[predict_var1]],na.rm = T),digits = 2),
                             round(mean(model_data[[predict_var1]],na.rm = T) + sd(model_data[[predict_var1]],na.rm = T),digits = 2))
    } else {
      predict_var1_level = c(unique(model_data[[predict_var1]])[1],unique(model_data[[predict_var1]])[2])
    }
  }
  
  if (is.null(predict_var2_level)) {
    if (length(unique(model_data[[predict_var2]])) != 2) {
      predict_var2_level = c(round(mean(model_data[[predict_var2]],na.rm = T) - sd(model_data[[predict_var2]],na.rm = T),digits = 2),
                             round(mean(model_data[[predict_var2]],na.rm = T) + sd(model_data[[predict_var2]],na.rm = T),digits = 2))
    } else {
      predict_var2_level = c(unique(model_data[[predict_var2]])[1],unique(model_data[[predict_var2]])[2])
    }
  }
  
  
  
  final_df = as.data.frame(effects::effect(term = interaction_term,
                                           mod = model,
                                           xlevels = setNames(list(predict_var1_level,predict_var2_level),c(predict_var1,predict_var2)))) %>% 
    dplyr::mutate(across(c(!!enquo(predict_var1),!!enquo(predict_var2)),as.character))
  
  
  if (is.null(response_var_name)) {
    response_var_name = response_var
  }
  
  if (is.null(predict_var1_name)) {
    predict_var1_name = predict_var1
  }
  if (is.null(predict_var2_name)) {
    predict_var2_name = predict_var2
  }
  
  if (is.null(y_lim)) {
    y_lim <- c(floor(min(final_df$fit)) - 0.5, ceiling(max(final_df$fit)) + 0.5)
  }
  ################################## Plotting ##################################
  if (plot_color) {
    plot <- ggplot2::ggplot(final_df, ggplot2::aes(y = .data$fit, x = .data[[predict_var1]],
                                                   color = .data[[predict_var2]])) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(group = .data[[predict_var2]])) +
      ggplot2::labs(y = response_var_name,
                    x = predict_var1_name,
                    color = predict_var2_name) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  } else {
    plot <- ggplot2::ggplot(final_df, ggplot2::aes(y = .data$fit, x = .data[[predict_var1]], group = .data[[predict_var2]]
    )
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(linetype = .data[[predict_var2]])) +
      ggplot2::labs(y = response_var_name,
                    x = predict_var1_name,
                    linetype = predict_var2_name) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  }
  
  if (!is.null(predict_var1_level_name)) {
    plot = plot+ggplot2::scale_x_discrete(labels = predict_var1_level_name)
  }
  
  if (!is.null(predict_var2_level_name)) {
    plot = plot+ggplot2::scale_linetype_discrete(labels = predict_var2_level_name)
  }
  
  ################################## Printing ################################## 
  if (print_plot == TRUE) {
    print(plot)
  }
  
  if (verbose == TRUE) {
    super_print('underline|Plot Specification')
    super_print('Interaction Term = {interaction_term}')
    super_print('')
    super_print('Plot Data')
    print_table(final_df)
  }
  
  if (return_plot_data == TRUE) {
    return(final_df)
  }
  if (return_plot == TRUE){
    return(plot)
  }
}
