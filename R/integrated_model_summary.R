#' Integrated Function for Linear Regression
#'
#' `r lifecycle::badge("stable")` \cr
#' It will first compute the linear regression. Then, it will graph the interaction using the two_way_interaction_plot or the three_way_interaction_plot function.
#' If you requested simple slope summary, it will calls the interaction::sim_slopes (Long, 2019).
#'
#' @param data data frame
#' @param response_variable DV (i.e., outcome variable / response variable). Length of 1. Support `dplyr::select()` syntax.
#' @param predictor_variable IV.  Support `dplyr::select()` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select()` syntax.
#' @param graph_label_name optional vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, ...). Function should be passed as a switch function (see ?two_way_interaction_plot for an example)
#' @param return_result If it is set to `TRUE` (default is `FALSE`), it will return the `model`, `model_summary`, and `plot` (if the interaction term is included)
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color If it is set to `TRUE` (default is `FALSE`), the interaction plot will plot with color.
#' @param quite suppress printing output
#' @param digits number of digits to round to
#' @param simple_slope Compute the slope differing with ± 1 SD of the IVs. In the background, it calls interaction:sim_slopes()
#' @param assumption_plot Generate an panel of plots that check major assumptions. You can use this if the model summary show violation of assumption (those maybe unreliable due to the use of p-value which is sensitive to the sample size). In the background, it calls performance::check_model()
#' @param streamline print streamlined output
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select()` syntax.
#' @param family a GLM family. It will passed to the family argument in glm. See `?glm` for possible options. `r lifecycle::badge("experimental")`
#' @param model_summary print model summary
#' @param interaction_plot generate interaction plot
#'
#' @return
#' return a list of all requested items in the order of model, model_summary, plot
#' @export
#'
#' @examples
#' fit <- integrated_model_summary(
#'   data = iris,
#'   response_variable = "Sepal.Length",
#'   predictor_variable = tidyselect::everything(),
#'   two_way_interaction_factor = c(Sepal.Width, Species)
#' )
#' \dontrun{
#' fit <- integrated_model_summary(
#'   data = iris,
#'   response_variable = "Sepal.Length",
#'   predictor_variable = tidyselect::everything(),
#'   two_way_interaction_factor = c(Sepal.Width, Species),
#'   simple_slope = TRUE, # you can request simple slope
#'   assumption_plot = TRUE, # you can also request assumption plot
#'   plot_color = TRUE # you can also request the plot in color
#' )
#' }
integrated_model_summary <- function(data,
                                     response_variable = NULL,
                                     predictor_variable = NULL,
                                     two_way_interaction_factor = NULL,
                                     three_way_interaction_factor = NULL,
                                     family = NULL,
                                     cateogrical_var = NULL,
                                     graph_label_name = NULL,
                                     model_summary = TRUE,
                                     interaction_plot = TRUE,
                                     y_lim = NULL,
                                     plot_color = FALSE,
                                     digits = 3,
                                     simple_slope = FALSE,
                                     assumption_plot = FALSE,
                                     quite = FALSE,
                                     streamline = FALSE,
                                     return_result = FALSE) {

  ##################################### Set up #########################################
  # parse select syntax
  response_variable <- data %>%
    dplyr::select(!!enquo(response_variable)) %>%
    names()
  predictor_variable <- data %>%
    dplyr::select(!!enquo(predictor_variable)) %>%
    names()
  two_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(two_way_interaction_factor)) %>%
    names()
  three_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(three_way_interaction_factor)) %>%
    names()
  # coerced into numeric after selecting variables
  data <- data_check(data) 
  
  ##################################### Running Model #########################################
  if (is.null(family)) {
    model <- lm_model(
      data = data,
      response_variable = dplyr::all_of(response_variable),
      predictor_variable = dplyr::all_of(predictor_variable),
      two_way_interaction_factor = dplyr::all_of(two_way_interaction_factor),
      three_way_interaction_factor = dplyr::all_of(three_way_interaction_factor),
      quite = TRUE
    )
  } else {
    if (simple_slope == TRUE | interaction_plot == T) {
      simple_slope <- FALSE
      interaction_plot <- FALSE
      warning("interaction_plot & simple_slope is not avaliable for glme model for now")
    }
    model <- glm_model(
      data = data,
      response_variable = tidyselect::all_of(response_variable),
      predictor_variable = tidyselect::all_of(predictor_variable),
      two_way_interaction_factor = tidyselect::all_of(two_way_interaction_factor),
      three_way_interaction_factor = tidyselect::all_of(three_way_interaction_factor),
      family = family,
      quite = TRUE
    )
  }


  ############################### Generate Interaction Plots ###############################
  two_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(two_way_interaction_factor)) %>%
    names()
  three_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(three_way_interaction_factor)) %>%
    names()
  interaction_plot_object <- NULL
  if (length(two_way_interaction_factor) != 0 & (interaction_plot == TRUE | return_result == TRUE)) {
    interaction_plot_object <- two_way_interaction_plot(
      model = model,
      cateogrical_var = cateogrical_var,
      graph_label_name = graph_label_name,
      y_lim = y_lim,
      plot_color = plot_color
    )
  } else if (length(three_way_interaction_factor) != 0 & (interaction_plot == TRUE | return_result == TRUE)) {
    interaction_plot_object <- three_way_interaction_plot(
      model = model,
      cateogrical_var = cateogrical_var,
      graph_label_name = graph_label_name,
      y_lim = y_lim,
      plot_color = plot_color
    )
  } else {
    interaction_plot_object <- NULL
    interaction_plot <- FALSE
  }


  if (simple_slope == TRUE) {
    if (!requireNamespace("interactions", quietly = TRUE)) {
      stop("Please install.packages(c('interactions','sandwich')) use simple_slope with three-way interaction")
    }
    
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Please install.packages('sandwich') use simple_slope with three-way interaction")
    }
    
    if (length(two_way_interaction_factor) != 0) {
        simple_slope_model <- interactions::sim_slopes(
          data = data,
          model = model,
          pred = !!two_way_interaction_factor[1],
          modx = !!two_way_interaction_factor[2],
          jnplot = TRUE,
        )
        simple_slope_output <-
          rbind(simple_slope_model$slopes) %>%
          dplyr::mutate(dplyr::across(1, function(x) {
            dplyr::case_when(
              x > mean(x) ~ "High",
              x == mean(x) ~ "Mean",
              x < mean(x) ~ "Low "
            )
          })) %>%
          dplyr::rename(ci.lower = "2.5%") %>%
          dplyr::rename(ci.upper = "97.5%")

        colnames(simple_slope_output)[1] <- c(paste(two_way_interaction_factor[2], "Level"))
        jnp_plot <- simple_slope_model$jnplot
      }
    if (length(three_way_interaction_factor) != 0) {
      if (!requireNamespace("cowplot", quietly = TRUE)) {
        stop("Please install.packages('cowplot') use simple_slope with three-way interaction")
      }
      
        simple_slope_model <- interactions::sim_slopes(
          data = data,
          model = model,
          pred = !!three_way_interaction_factor[1],
          modx = !!three_way_interaction_factor[2],
          mod2 = !!three_way_interaction_factor[3],
          jnplot = TRUE
        )
        if (length(simple_slope_model$slopes) == 3) { # if mod 2 is continuous
          simple_slope_output <-
            rbind(simple_slope_model$slopes[[1]], simple_slope_model$slopes[[2]], simple_slope_model$slopes[[3]]) %>%
            dplyr::mutate(dplyr::across(1, function(x) {
              dplyr::case_when(
                x > mean(x) ~ "High",
                x == mean(x) ~ "Mean",
                x < mean(x) ~ "Low "
              )
            }))
          simple_slope_output <- simple_slope_output %>%
            dplyr::mutate(Mod_1_Level = rep(c("Low ", "Mean", "High"), each = nrow(simple_slope_output) / 3)) %>%
            dplyr::select("Mod_1_Level", tidyselect::everything())
        } else if (length(simple_slope_model$slopes) == 2) { # if mod 2 is binary
          simple_slope_output <-
            rbind(simple_slope_model$slopes[[1]], simple_slope_model$slopes[[2]]) %>%
            dplyr::mutate(dplyr::across(1, function(x) {
              dplyr::case_when(
                x > mean(x) ~ "High",
                x == mean(x) ~ "Mean",
                x < mean(x) ~ "Low "
              )
            }))
          simple_slope_output <- simple_slope_output %>%
            dplyr::mutate(Mod_1_Level = rep(c("Low ", "High"), each = nrow(simple_slope_output) / 2)) %>%
            dplyr::select("Mod_1_Level", tidyselect::everything())
        }

        simple_slope_output <- simple_slope_output %>%
          dplyr::rename(ci.lower = "2.5%") %>%
          dplyr::rename(ci.upper = "97.5%") %>%
          dplyr::mutate(dplyr::across("Mod_1_Level", ~ replace(., duplicated(.), "")))
        colnames(simple_slope_output)[c(1, 2)] <- c(paste(three_way_interaction_factor[3], "Level"), paste(three_way_interaction_factor[2], "Level"))

        jnp_plot <- simple_slope_model$jnplot
      } #three-way interaction end 
    } else{
    simple_slope_output = NULL
    jnp_plot = NULL
  }

  # Print result
  if (model_summary == TRUE | return_result == TRUE) {
    model_summary_list <- model_summary(
      model = model,
      streamline = streamline,
      digits = digits,
      return_result = TRUE,
      assumption_plot = assumption_plot,
      quite = quite
    )
  }

  if (interaction_plot == TRUE) {
    try(print(interaction_plot_object))
  }

  if (simple_slope == TRUE & quite == F) {
    super_print("underline|Slope Estimates at Each Level of Moderators")
    print_table(simple_slope_output)
    print(jnp_plot)
  }

  plot_logical <- c(interaction_plot, simple_slope, assumption_plot)
  number_of_plot_requested <- length(plot_logical[plot_logical])
  if (number_of_plot_requested > 1) {
    warning("You requested > 2 plots. Since 1 plot can be displayed at a time, considering using Rmd for better viewing experience.")
  }

  # Return Result
  if (return_result == TRUE) {
    return_list <- list(model = model,
                        summary = model_summary_list, 
                        interaction_plot = interaction_plot_object, 
                        simple_slope_df = simple_slope_output, 
                        jnp_plot = jnp_plot)
    return(return_list)
  }
}
