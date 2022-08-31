#' Interaction plot 
#' 
#' `r lifecycle::badge("stable")` \cr
#' The function creates a two-way or three-way interaction plot. It will creates a plot with ± 1 SD from the mean of the independent variable. See below for supported model. I recommend using concurrently with `lm_model()`, `lme_model()`.
#'
#' @param model object from `lme`, `lme4`, `lmerTest` object.
#' @param data data frame. If the function is unable to extract data frame from the object, then you may need to pass it directly
#' @param graph_label_name vector of length 4 or a switch function (see ?two_way_interaction_plot example). Vector should be passed in the form of c(response_var, predict_var1, predict_var2, predict_var3).
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color default if `FALSE`. Set to `TRUE` if you want to plot in color
#'
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
#' lm_fit_2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length +
#'   Sepal.Width*Petal.Length, data = iris)
#'   
#' interaction_plot(lm_fit_2)
#' 
#' lm_fit_3 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + 
#'   Sepal.Width*Petal.Length:Petal.Width, data = iris)
#'   
#' interaction_plot(lm_fit_3)
#' 
#' 
interaction_plot = function(model,
                            data = NULL,
                            graph_label_name = NULL,
                            cateogrical_var = NULL,
                            y_lim = NULL,
                            plot_color = FALSE) {
  if (length(suppressWarnings(get_interaction_term(model))) == 2) {
    two_way_interaction_plot(
      model = model,
      data = data,
      graph_label_name = graph_label_name,
      cateogrical_var = cateogrical_var,
      y_lim = y_lim,
      plot_color = plot_color
    )
  } else if (length(get_interaction_term(model)) == 3) {
    three_way_interaction_plot(
      model = model,
      data = data,
      graph_label_name = graph_label_name,
      cateogrical_var = cateogrical_var,
      y_lim = y_lim,
      plot_color = plot_color
    )
  }
}
