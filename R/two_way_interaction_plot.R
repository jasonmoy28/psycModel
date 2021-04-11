#' Two-way Interaction Plot
#'
#' `r lifecycle::badge("experimental")` \cr
#' The function creates a two-way interaction plot. It will creates a plot with ± 1 SD from the mean of the independent variable. See supported model below
#'
#' @param model object from nlme, lme4, or lmerTest (linear mixed effect model)
#' @param graph_label_name vector of length 3 or function. Vector should be passed in the form of `c(response_var, predict_var1, predict_var2)`. Function should be passed as a switch function that return the label based on the name passed (e.g., a switch function)
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param y_lim vector of length 2. c(lower_limit, upper_limit). Default is +0.5 of the max -0.5 of the min Y value
#' @param plot_color logical. default as F. Set to T if you want to plot in color
#'
#' @references
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#' @return ggplot object.
#' @export
#'
#' @examples # run the fit model first
#' fit <- lme_model(
#'   response_variable = JS_Individual,
#'   level_1_factors = Age_Individual,
#'   level_2_factors = contains("Country"),
#'   two_way_interaction_factor = c("Age_Individual", "Hofstede_IC_Country"),
#'   id = "Country",
#'   data = EWCS_2015_shorten
#' )
#'
#' two_way_interaction_plot(
#'   model = fit,
#'   # this will change the plot name
#'   graph_label_name = c(
#'     "Job Satisfaction",
#'     "Age",
#'     "Individualism/Collectivism"
#'   )
#' )
#'
#' # Below is an equivalent way of changing graph_label_name by using the switch function
#' # You should store this in a separate script, and load this function using source().
#' # It is primarily use if you are running many models
#' graph_label_name <- function(var_name) {
#'   var_name_processed <- switch(var_name,
#'     "JS_Individual" = "Job Satisfaction",
#'     "Age_Individual" = "Age",
#'     "Hofstede_IC_Country" = "Individualism/Collectivism"
#'   )
#'   if (is.null(var_name_processed)) {
#'     var_name_processed <- var_name
#'   }
#'   return(var_name_processed)
#' }
#'
#' two_way_interaction_plot(
#'   model = fit,
#'   # this will change the plot name
#'   graph_label_name = graph_label_name
#' )
two_way_interaction_plot <- function(model,
                                     graph_label_name = NULL,
                                     cateogrical_var = NULL,
                                     y_lim = NULL,
                                     plot_color = FALSE) {

  # warning functions of more than two interaction
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

  # get variable from model
  predict_var <- attributes(formula_attribute)$term.labels
  response_var <- as.character(attributes(formula_attribute)$variables)[2]
  interaction_term <- predict_var[stringr::str_detect(predict_var, ":")]
  interaction_term <- interaction_plot_check(interaction_term)

  predict_var1 <- gsub(pattern = ":.+", "", x = interaction_term)
  predict_var2 <- gsub(pattern = ".+:", "", x = interaction_term)


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

  if (class(model) == "lme") {
    upper_upper_predicted_value <- stats::predict(model, newdata = upper_upper_df, level = 0)
    upper_lower_predicted_value <- stats::predict(model, newdata = upper_lower_df, level = 0)
    lower_upper_predicted_value <- stats::predict(model, newdata = lower_upper_df, level = 0)
    lower_lower_predicted_value <- stats::predict(model, newdata = lower_lower_df, level = 0)
  } else if (class(model) == "lmerModLmerTest" | class(model) == "glmerMod" | class(model) == "lmerMod") {
    upper_upper_predicted_value <- stats::predict(model, newdata = upper_upper_df, allow.new.levels = T)
    upper_lower_predicted_value <- stats::predict(model, newdata = upper_lower_df, allow.new.levels = T)
    lower_upper_predicted_value <- stats::predict(model, newdata = lower_upper_df, allow.new.levels = T)
    lower_lower_predicted_value <- stats::predict(model, newdata = lower_lower_df, allow.new.levels = T)
  }

  final_df <- data.frame(
    value = c(upper_upper_predicted_value, upper_lower_predicted_value, lower_upper_predicted_value, lower_lower_predicted_value),
    var1_category = factor(c("High", "High", "Low", "Low"), levels = c("Low", "High")),
    var2_category = c("High", "Low", "High", "Low")
  )

  # Get the correct label for the plot
  if (!is.null(graph_label_name)) {
    # If a vector of string is passed as argument, slice the vector
    if (class(graph_label_name) == "character") {
      response_var_plot_label <- graph_label_name[1]
      predict_var1_plot_label <- graph_label_name[2]
      predict_var2_plot_label <- graph_label_name[3]
      # if a function of switch_case is passed as an argument, use the function
    } else if (class(graph_label_name) == "function") {
      response_var_plot_label <- graph_label_name(response_var)
      predict_var1_plot_label <- graph_label_name(predict_var1)
      predict_var2_plot_label <- graph_label_name(predict_var2)
      # All other case use the original label
    } else {
      response_var_plot_label <- response_var
      predict_var1_plot_label <- predict_var1
      predict_var2_plot_label <- predict_var2
    }
    # All other case use the original label
  } else {
    response_var_plot_label <- response_var
    predict_var1_plot_label <- predict_var1
    predict_var2_plot_label <- predict_var2
  }


  if (is.null(y_lim)) {
    y_lim <- c(floor(min(final_df$value)) - 0.5, ceiling(max(final_df$value)) + 0.5)
  }

  if (plot_color) {
    plot <-
      ggplot2::ggplot(final_df, ggplot2::aes(y = .data$value, x = .data$var1_category, color = .data$var2_category)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(group = .data$var2_category)) +
      ggplot2::labs(
        y = response_var_plot_label,
        x = predict_var1_plot_label,
        color = predict_var2_plot_label
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  } else {
    plot <-
      ggplot2::ggplot(final_df, ggplot2::aes(y = .data$value, x = .data$var1_category, group = .data$var2_category)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(linetype = .data$var2_category)) +
      ggplot2::labs(
        y = response_var_plot_label,
        x = predict_var1_plot_label,
        linetype = predict_var2_plot_label
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black")
      ) +
      ggplot2::ylim(y_lim[1], y_lim[2])
  }

  return(plot)
}
