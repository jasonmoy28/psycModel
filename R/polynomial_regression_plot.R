#' Polynomial Regression Plot
#'
#' `r lifecycle::badge("experimental")` \cr
#' The function create a simple regression plot (no interaction). Can be used to visualize polynomial regression.
#'
#'
#' @param model object from `lm`
#' @param model_data optional dataframe (in case data cannot be retrieved from the model)
#' @param predictor predictor variable name (must be character)
#' @param graph_label_name vector of length 3 or function. Vector should be passed in the form of `c(response_var, predict_var1, predict_var2)`. Function should be passed as a switch function that return the label based on the name passed (e.g., a switch function)
#' @param x_lim the plot's upper and lower limit for the x-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color default if `FALSE`. Set to `TRUE` if you want to plot in color
#'
#' @details It appears that `predict` cannot handle categorical factors. All variables are converted to numeric before plotting.
#' @return an object of class `ggplot`
#' @export
#'

polynomial_regression_plot <- function(model,
                                       model_data = NULL,
                                       predictor,
                                       graph_label_name = NULL,
                                       x_lim = NULL,
                                       y_lim = NULL,
                                       plot_color = FALSE) {
  
  if (is.null(model_data)) {model_data <- insight::get_data(model)}
  
  response_var <- model %>% insight::find_response()
  
  interaction_term = get_interaction_term(model)
  
  mean_df = get_predict_df(data = model_data)$mean_df
  above_mean_df = get_predict_df(data = model_data)$upper_df
  below_mean_df = get_predict_df(data = model_data)$lower_df
  
  ##################################### Polynomial Regression Plot without interaction ##############################################
  if (is.null(interaction_term)) {
    plot_df = dplyr::tibble()
    for (i in seq(min(model_data[[predictor]]), max(model_data[[predictor]]), stats::sd(model_data[[predictor]]/20))) {
      mean_df[predictor] = i
      predicted_value = stats::predict(object = model,
                                       newdata = mean_df,
                                       allow.new.levels = TRUE)
      in_loop_df = data.frame(x = i, y = predicted_value)
      plot_df = rbind(plot_df,in_loop_df)
    }
    label_name = label_name(graph_label_name = graph_label_name,
                            response_var_name = response_var,
                            predict_var1_name = predictor,
                            predict_var2_name = NULL,
                            predict_var3_name = NULL)
    
    main_plot = ggplot2::ggplot(data = plot_df,ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_line()
    
    ##################################### Polynomial Regression Plot with interaction ##############################################
  } else {
    
    predict_var1 = predictor
    predict_var2 = get_interaction_term(model)[!stringr::str_detect(get_interaction_term(model),predictor)] %>% as.character(.)
    
    upper_df = mean_df
    upper_df[predict_var2] <- above_mean_df[predict_var2]
    
    lower_df = mean_df
    lower_df[predict_var2] <- below_mean_df[predict_var2]
    
    plot_df = tibble::tibble()
    for (i in seq(min(model_data[[predict_var1]]),max(model_data[[predict_var1]]),stats::sd(model_data[[predict_var1]] / 20))) {
      
      upper_df[predict_var1] = i
      lower_df[predict_var1] = i
      
      upper_value = stats::predict(object = model, newdata = upper_df,allow.new.levels = TRUE)
      lower_value = stats::predict(object = model, newdata = lower_df,allow.new.levels = TRUE)
      
      in_loop_df = data.frame(x = c(i, i), y = c(upper_value, lower_value), condition = c('High', 'Low'))
      plot_df = rbind(plot_df, in_loop_df)
    }
    label_name = label_name(graph_label_name = graph_label_name,
                            response_var_name = response_var,
                            predict_var1_name = predict_var1,
                            predict_var2_name = predict_var2,
                            predict_var3_name = NULL)
    main_plot = ggplot2::ggplot(data = plot_df, ggplot2::aes(x = .data$x, y = .data$y,group = .data$condition)) +
      ggplot2::geom_line(ggplot2::aes(linetype = .data$condition))
  }
  
  ##################################### Plotting  ##############################################
  plot = main_plot +
    ggplot2::labs(
      y = label_name[1],
      x = label_name[2]) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black")
    )
  if (!is.null(x_lim)) {plot = plot + ggplot2::xlim(x_lim[1], x_lim[2])}
  if (!is.null(y_lim)) {plot = plot + ggplot2::ylim(y_lim[1], y_lim[2])}
  
  return(plot)
  
}
