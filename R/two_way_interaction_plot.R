#' Two-way Interaction Plot
#'
#' `r lifecycle::badge("deprecated")` \cr
#' The function creates a two-way interaction plot. It will creates a plot with ± 1 SD from the mean of the independent variable. See supported model below.
#' I recommend using concurrently with `lm_model` or `lme_model`.
#'
#'
#' @param model object from `lm`, `nlme`, `lme4`, or `lmerTest`
#' @param data `data.frame`. If the function is unable to extract data frame from the object, then you may need to pass it directly
#' @param graph_label_name vector of length 3 or function. Vector should be passed in the form of `c(response_var, predict_var1, predict_var2)`. Function should be passed as a switch function that return the label based on the name passed (e.g., a switch function)
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color default if `FALSE`. Set to `TRUE` if you want to plot in color
#'
#' @details It appears that ``predict` cannot handle categorical factors. All variables are converted to numeric before plotting.
#' @return an object of class `ggplot`
#' @export
#'
#' @examples
#' lm_fit <- lm(Sepal.Length ~ Sepal.Width * Petal.Width,
#'   data = iris
#' )
#' two_way_interaction_plot(lm_fit, data = iris)
#'
two_way_interaction_plot <- function(model,
                                     data = NULL,
                                     graph_label_name = NULL,
                                     cateogrical_var = NULL,
                                     y_lim = NULL,
                                     plot_color = FALSE) {
  model_data <- NULL
  model_data <- insight::get_data(model)
  predict_var <- model %>%
    insight::find_predictors() %>%
    .$conditional # maybe problem with unconditional?
  response_var <- model %>% insight::find_response()
  interaction_term <- model %>%
    insight::find_interactions() %>%
    .$conditional
  if (any(class(model) %in% c("lmerMod", "lmerModLmerTest", "lm", "lme"))) {
    interaction_term <- interaction_check(interaction_term)
  } else {
    warning("Only models from lm, nlme, lme4, and lmerTest are supported")
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
  
  mean_df = get_predict_df(data = model_data)$mean_df
  upper_df = get_predict_df(data = model_data)$upper_df
  lower_df = get_predict_df(data = model_data)$lower_df
  
  # Specify the categorical variable upper and lower bound directly
  if (!is.null(cateogrical_var)) {
    for (name in names(cateogrical_var)) {
      upper_df[name] <- cateogrical_var[[name]][1]
      lower_df[name] <- cateogrical_var[[name]][2]
    }
  }
  
  # Update values in the new_data_df to the values in predicted_df & get the predicted value
  upper_upper_df <- mean_df
  upper_upper_df[predict_var1] <- upper_df[predict_var1]
  upper_upper_df[predict_var2] <- upper_df[predict_var2]
  
  upper_lower_df <- mean_df
  upper_lower_df[predict_var1] <- upper_df[predict_var1]
  upper_lower_df[predict_var2] <- lower_df[predict_var2]
  
  lower_upper_df <- mean_df
  lower_upper_df[predict_var1] <- lower_df[predict_var1]
  lower_upper_df[predict_var2] <- upper_df[predict_var2]
  
  lower_lower_df <- mean_df
  lower_lower_df[predict_var1] <- lower_df[predict_var1]
  lower_lower_df[predict_var2] <- lower_df[predict_var2]
  
  if (inherits(model,'lme')) {
    upper_upper_predicted_value <-
      stats::predict(model, newdata = upper_upper_df, level = 0)
    upper_lower_predicted_value <-
      stats::predict(model, newdata = upper_lower_df, level = 0)
    lower_upper_predicted_value <-
      stats::predict(model, newdata = lower_upper_df, level = 0)
    lower_lower_predicted_value <-
      stats::predict(model, newdata = lower_lower_df, level = 0)
  } else if (inherits(model,'lmerMod') | inherits(model,'glmerMod')) {
    upper_upper_predicted_value <-
      stats::predict(model, newdata = upper_upper_df, allow.new.levels = TRUE)
    upper_lower_predicted_value <-
      stats::predict(model, newdata = upper_lower_df, allow.new.levels = TRUE)
    lower_upper_predicted_value <-
      stats::predict(model, newdata = lower_upper_df, allow.new.levels = TRUE)
    lower_lower_predicted_value <-
      stats::predict(model, newdata = lower_lower_df, allow.new.levels = TRUE)
  } else if (inherits(model,'lm')) {
    upper_upper_predicted_value <-
      stats::predict(model, newdata = upper_upper_df)
    upper_lower_predicted_value <-
      stats::predict(model, newdata = upper_lower_df)
    lower_upper_predicted_value <-
      stats::predict(model, newdata = lower_upper_df)
    lower_lower_predicted_value <-
      stats::predict(model, newdata = lower_lower_df)
  }
  
  final_df <- data.frame(
    value = c(
      upper_upper_predicted_value,
      upper_lower_predicted_value,
      lower_upper_predicted_value,
      lower_lower_predicted_value
    ),
    var1_category = factor(c("High", "High", "Low", "Low"), levels = c("Low", "High")),
    var2_category = c("High", "Low", "High", "Low")
  )
  
  
  
  # Get the correct label for the plot
  label_name = label_name(
    graph_label_name = graph_label_name,
    response_var_name = response_var,
    predict_var1_name = predict_var1,
    predict_var2_name = predict_var2,
    predict_var3_name = NULL
  )
  
  if (is.null(y_lim)) {
    y_lim <- c(floor(min(final_df$value)) - 0.5, ceiling(max(final_df$value)) + 0.5)
  }
  
  if (plot_color) {
    plot <-
      ggplot2::ggplot(
        final_df,
        ggplot2::aes(
          y = .data$value,
          x = .data$var1_category,
          color = .data$var2_category
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(group = .data$var2_category)) +
      ggplot2::labs(y = label_name[1],
                    x = label_name[2],
                    color = label_name[3]) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  } else {
    plot <-
      ggplot2::ggplot(
        final_df,
        ggplot2::aes(
          y = .data$value,
          x = .data$var1_category,
          group = .data$var2_category
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(linetype = .data$var2_category)) +
      ggplot2::labs(y = label_name[1],
                    x = label_name[2],
                    linetype = label_name[3]) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  }
  
  return(plot)
}
