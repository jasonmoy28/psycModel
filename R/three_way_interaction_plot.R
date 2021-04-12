#' Three-way Interaction Plot
#'
#' `r lifecycle::badge("experimental")` \cr
#' The function creates a two-way interaction plot. It will creates a plot with ± 1 SD from the mean of the independent variable. See below for supported model
#'
#' @param model lme, lmerMod, lmerModLmerTest object.
#' @param graph_label_name vector of length 3 or a switch function (see ?two_way_interaction_plot example). Vector should be passed in the form of c(response_var, predict_var1, predict_var2, predict_var3).
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param y_lim vector of two number. set the y_lim of the plot
#' @param plot_color logical. default as F. Set to T if you want to plot in color
#'
#' @references
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' @return ggplot object.
#'
#' @export
#'
#' @examples
#' fit <- lme_model(
#'   response_variable = JS_Individual,
#'   level_1_factors = c(Age_Individual, Education_Individual),
#'   level_2_factors = contains("Country"),
#'   three_way_interaction_factor = c(
#'     "Age_Individual",
#'     "Education_Individual",
#'     "Hofstede_IC_Country"
#'   ),
#'   id = Country,
#'   data = EWCS_2015_shorten
#' )
#'
#' three_way_interaction_plot(fit)
#' three_way_interaction_plot(fit,plot_color = T) # plots with color 
three_way_interaction_plot <- function(model,
                                       cateogrical_var = NULL,
                                       graph_label_name = NULL,
                                       y_lim = NULL,
                                       plot_color = F) {
  interaction_plot_check <- function(interaction_term) {
    if (length(interaction_term) > 1) {
      interaction_term <- interaction_term[1]
      warning(paste("Inputted > 2 interaction terms. Plotting the first interaction term:\n ", interaction_term))
    }
    return(interaction_term)
  }

  # get attributes based on mdeol
  if (class(model) == "lme") {
    formula_attribute <- model$terms
    data <- model$data
  } else if (any(class(model) %in% c("lmerMod", "lmerModLmerTest"))) {
    formula_attribute <- stats::terms(model@call$formula)
    data <- model@call$data
  } else {
    stop("It only support linear mixed effect model object from nlme, lme4, and lmerTest")
  }

  predict_var <- attributes(formula_attribute)$term.labels
  response_var <- as.character(attributes(formula_attribute)$variables)[2]
  interaction_term <- predict_var[stringr::str_detect(predict_var, ":.+:")]
  interaction_term <- interaction_plot_check(interaction_term)
  predict_var1 <- gsub(pattern = ":.+", "", x = interaction_term)
  predict_var3 <- gsub(pattern = ".+:", "", x = interaction_term)
  remove1 <- stringr::str_remove(pattern = predict_var1, string = interaction_term)
  remove2 <- stringr::str_remove(pattern = predict_var3, string = remove1)
  predict_var2 <- gsub(pattern = ":", "", x = remove2)

  mean_df <- dplyr::summarise_all(data, mean, na.rm = T)
  upper_df <- dplyr::summarise_all(data, .funs = function(.) {
    mean(., na.rm = T) + 1 * stats::sd(., na.rm = T)
  })

  lower_df <- dplyr::summarise_all(data, .funs = function(.) {
    mean(., na.rm = T) - 1 * stats::sd(., na.rm = T)
  })

  # Specify the categorical variable upper and lower bound directly
  if (!is.null(cateogrical_var)) {
    for (name in names(cateogrical_var)) {
      upper_df[name] <- cateogrical_var[[name]][1]
      lower_df[name] <- cateogrical_var[[name]][2]
    }
  }
  # Get the variable names of the model
  # Update values in the new_data_df to the values in predicted_df & get the predicted value
  # Plot 1
  upper_upper_upper_df <- mean_df
  upper_upper_upper_df[predict_var1] <- upper_df[predict_var1]
  upper_upper_upper_df[predict_var2] <- upper_df[predict_var2]
  upper_upper_upper_df[predict_var3] <- upper_df[predict_var3]

  upper_lower_upper_df <- mean_df
  upper_lower_upper_df[predict_var1] <- upper_df[predict_var1]
  upper_lower_upper_df[predict_var2] <- lower_df[predict_var2]
  upper_lower_upper_df[predict_var3] <- upper_df[predict_var3]

  lower_upper_upper_df <- mean_df
  lower_upper_upper_df[predict_var1] <- lower_df[predict_var1]
  lower_upper_upper_df[predict_var2] <- upper_df[predict_var2]
  lower_upper_upper_df[predict_var3] <- upper_df[predict_var3]


  lower_lower_upper_df <- mean_df
  lower_lower_upper_df[predict_var1] <- lower_df[predict_var1]
  lower_lower_upper_df[predict_var2] <- lower_df[predict_var2]
  lower_lower_upper_df[predict_var3] <- upper_df[predict_var3]

  if (class(model) == "lme") {
    upper_upper_upper_predicted_value <- stats::predict(model, newdata = upper_upper_upper_df, level = 0)
    upper_lower_upper_predicted_value <- stats::predict(model, newdata = upper_lower_upper_df, level = 0)
    lower_upper_upper_predicted_value <- stats::predict(model, newdata = lower_upper_upper_df, level = 0)
    lower_lower_upper_predicted_value <- stats::predict(model, newdata = lower_lower_upper_df, level = 0)
  } else if (class(model) == "lmerModLmerTest" | class(model) == "lmerMod") {
    upper_upper_upper_predicted_value <- stats::predict(model, newdata = upper_upper_upper_df, allow.new.levels = T)
    upper_lower_upper_predicted_value <- stats::predict(model, newdata = upper_lower_upper_df, allow.new.levels = T)
    lower_upper_upper_predicted_value <- stats::predict(model, newdata = lower_upper_upper_df, allow.new.levels = T)
    lower_lower_upper_predicted_value <- stats::predict(model, newdata = lower_lower_upper_df, allow.new.levels = T)
  }

  # Second plot
  upper_upper_lower_df <- mean_df
  upper_upper_lower_df[predict_var1] <- upper_df[predict_var1]
  upper_upper_lower_df[predict_var2] <- upper_df[predict_var2]
  upper_upper_lower_df[predict_var3] <- lower_df[predict_var3]

  upper_lower_lower_df <- mean_df
  upper_lower_lower_df[predict_var1] <- upper_df[predict_var1]
  upper_lower_lower_df[predict_var2] <- lower_df[predict_var2]
  upper_lower_lower_df[predict_var3] <- lower_df[predict_var3]

  lower_upper_lower_df <- mean_df
  lower_upper_lower_df[predict_var1] <- lower_df[predict_var1]
  lower_upper_lower_df[predict_var2] <- upper_df[predict_var2]
  lower_upper_lower_df[predict_var3] <- lower_df[predict_var3]
  lower_lower_lower_df <- mean_df

  lower_lower_lower_df[predict_var1] <- lower_df[predict_var1]
  lower_lower_lower_df[predict_var2] <- lower_df[predict_var2]
  lower_lower_lower_df[predict_var3] <- lower_df[predict_var3]

  if (class(model) == "lme") {
    upper_upper_lower_predicted_value <- stats::predict(model, newdata = upper_upper_lower_df, level = 0)
    upper_lower_lower_predicted_value <- stats::predict(model, newdata = upper_lower_lower_df, level = 0)
    lower_upper_lower_predicted_value <- stats::predict(model, newdata = lower_upper_lower_df, level = 0)
    lower_lower_lower_predicted_value <- stats::predict(model, newdata = lower_lower_lower_df, level = 0)
  } else if (class(model) == "lmerModLmerTest" | class(model) == "glmerMod" | class(model) == "lmerMod") {
    upper_upper_lower_predicted_value <- stats::predict(model, newdata = upper_upper_lower_df, allow.new.levels = T)
    upper_lower_lower_predicted_value <- stats::predict(model, newdata = upper_lower_lower_df, allow.new.levels = T)
    lower_upper_lower_predicted_value <- stats::predict(model, newdata = lower_upper_lower_df, allow.new.levels = T)
    lower_lower_lower_predicted_value <- stats::predict(model, newdata = lower_lower_lower_df, allow.new.levels = T)
  }

  # Get the correct label for the plot
  if (!is.null(graph_label_name)) {
    # If a vector of string is passed as argument, slice the vector
    if (class(graph_label_name) == "character") {
      response_var_plot_label <- graph_label_name[1]
      predict_var1_plot_label <- graph_label_name[2]
      predict_var2_plot_label <- graph_label_name[3]
      predict_var2_plot_label <- graph_label_name[4]
      # if a function of switch_case is passed as an argument, use the function
    } else if (class(graph_label_name) == "function") {
      response_var_plot_label <- graph_label_name(response_var)
      predict_var1_plot_label <- graph_label_name(predict_var1)
      predict_var2_plot_label <- graph_label_name(predict_var2)
      predict_var3_plot_label <- graph_label_name(predict_var3)

      # All other case use the original label
    } else {
      response_var_plot_label <- response_var
      predict_var1_plot_label <- predict_var1
      predict_var2_plot_label <- predict_var2
      predict_var3_plot_label <- predict_var3
    }
    # All other case use the original label
  } else {
    response_var_plot_label <- response_var
    predict_var1_plot_label <- predict_var1
    predict_var2_plot_label <- predict_var2
    predict_var3_plot_label <- predict_var3
  }

  high <- stringr::str_c("High", " ", predict_var3_plot_label)
  low <- stringr::str_c("Low", " ", predict_var3_plot_label)

  final_df <- data.frame(
    value = c(
      upper_upper_upper_predicted_value,
      upper_lower_upper_predicted_value,
      lower_upper_upper_predicted_value,
      lower_lower_upper_predicted_value,
      upper_upper_lower_predicted_value,
      upper_lower_lower_predicted_value,
      lower_upper_lower_predicted_value,
      lower_lower_lower_predicted_value
    ),
    var1_category = factor(c("High", "High", "Low", "Low", "High", "High", "Low", "Low"), levels = c("Low", "High")),
    var2_category = factor(c("High", "Low", "High", "Low", "High", "Low", "High", "Low"), levels = c("Low", "High")),
    var3_category = factor(c(high, high, high, high, low, low, low, low), levels = c(low, high))
  )

  # Set auto ylim
  if (is.null(y_lim)) {
    y_lim <- c(floor(min(final_df$value)) - 0.5, ceiling(max(final_df$value)) + 0.5)
  }


  if (plot_color) {
    plot <-
      ggplot2::ggplot(data = final_df, ggplot2::aes(y = .data$value, x = .data$var1_category, color = .data$var2_category)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(group = .data$var2_category)) +
      ggplot2::labs(
        y = response_var_plot_label,
        x = predict_var1_plot_label,
        color = predict_var2_plot_label
      ) +
      ggplot2::facet_wrap(~var3_category) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  } else {
    plot <-
      ggplot2::ggplot(data = final_df, ggplot2::aes(y = .data$value, x = .data$var1_category, group = .data$var2_category)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(linetype = .data$var2_category)) +
      ggplot2::labs(
        y = response_var_plot_label,
        x = predict_var1_plot_label,
        linetype = predict_var2_plot_label
      ) +
      ggplot2::facet_wrap(~var3_category) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  }

  return(plot)
}
