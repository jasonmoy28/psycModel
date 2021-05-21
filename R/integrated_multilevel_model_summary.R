#' Integrated Function for Mixed Effect Model
#'
#' `r lifecycle::badge("stable")` \cr
#' It will first compute the mixed effect model. It will use either the `nlme::lme` or the `lmerTest::lmer` for linear mixed effect model. It will use `lme4::glmer` for generalized linear mixed effect model.
#' Then, it will print the model summary and the panel of the plots that are useful for checking assumption (default is `FALSE`). If you requested the interaction plot (default is `TRUE`), it will graph the interaction (Currently only support `lme` model but not `glme`)
#' If you requested simple slope summary, it will uses the `interaction::sim_slopes()` to generate the slope estimate at varying level of the moderator (see `?simple_slope` for more detail)
#'
#' @param data data frame
#' @param model `lme4` model syntax. Support more complicated model structure from `lme4`. It is not well-tested to ensure accuracy `r lifecycle::badge("experimental")`
#' @param response_variable DV (i.e., outcome variable / response variable). Length of 1. Support `dplyr::select()` syntax.
#' @param random_effect_factors random effect factors (level-1 variable for HLM from a HLM perspective) Factors that need to estimate fixed effect and random effect (i.e., random slope / varying slope based on the id). Support `dplyr::select()` syntax.
#' @param non_random_effect_factors non-random effect factors (level-2 variable from a HLM perspective). Factors only need to estimate fixed effect. Support `dplyr::select()` syntax.
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select()` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select()` syntax.
#' @param id the nesting variable (e.g. group, time). Length of 1. Support `dplyr::select()` syntax.
#' @param graph_label_name optional vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, ...). Function should be passed as a switch function (see ?two_way_interaction_plot for an example)
#' @param estimation_method character. `ML` or `REML` default is `REML`.
#' @param return_result If it is set to `TRUE` (default is `FALSE`), it will return the `model`, `model_summary`, and `plot` (`plot` if the interaction term is included)
#' @param na.action default is `stats::na.omit`. Another common option is `na.exclude`
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param opt_control default is `optim` for `lme` and `bobyqa` for `lmerTest`.
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color If it is set to `TRUE` (default is `FALSE`), the interaction plot will plot with color.
#' @param use_package Default is `lmerTest`. Only available for linear mixed effect model. Options are `nlme`, `lmerTest`, or `lme4`(`'lme4` return similar result as `lmerTest` except the return model)
#' @param quite suppress printing output
#' @param digits number of digits to round to
#' @param simple_slope Slope estimate at ± 1 SD and the mean of the moderator. Uses `interactions::sim_slope()` in the background.
#' @param assumption_plot Generate an panel of plots that check major assumptions. It is usually recommended to inspect model assumption violation visually. In the background, it calls `performance::check_model()`.
#' @param streamline print streamlined output.
#' @param family a GLM family. It will passed to the family argument in glmer. See `?glmer` for possible options. `r lifecycle::badge("experimental")`
#' @param model_summary print model summary.  Required to be `TRUE` if you want `assumption_plot`.
#' @param interaction_plot generate interaction plot. Default is `TRUE`
#'
#' @return a list of all requested items in the order of model, model_summary, interaction_plot, simple_slope
#' @export
#'
#' @examples
#' fit <- integrated_multilevel_model_summary(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = c(extrav),
#'   non_random_effect_factors = texp,
#'   two_way_interaction_factor = c(extrav, texp),
#'   graph_label_name = c("popular", "extraversion", "teacher experience"),
#'   id = class
#' )
#' \donttest{
#' fit <- integrated_multilevel_model_summary(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = c(extrav, sex),
#'   non_random_effect_factors = texp,
#'   three_way_interaction_factor = c(extrav, sex, texp), # three-way interaction
#'   graph_label_name = c("popular", "extraversion", "sex", "teacher experience"),
#'   id = class,
#'   simple_slope = TRUE, # you can request simple slope
#'   assumption_plot = TRUE, # you can also request assumption plot
#'   plot_color = TRUE # you can also request the plot in color
#' )
#' }
#'
integrated_multilevel_model_summary <- function(data,
                                                model = NULL,
                                                response_variable = NULL,
                                                random_effect_factors = NULL,
                                                non_random_effect_factors = NULL,
                                                two_way_interaction_factor = NULL,
                                                three_way_interaction_factor = NULL,
                                                family = NULL,
                                                cateogrical_var = NULL,
                                                id = NULL,
                                                graph_label_name = NULL,
                                                estimation_method = "REML",
                                                opt_control = "bobyqa",
                                                na.action = stats::na.omit,
                                                model_summary = TRUE,
                                                interaction_plot = TRUE,
                                                y_lim = NULL,
                                                plot_color = FALSE,
                                                digits = 3,
                                                use_package = "lmerTest",
                                                simple_slope = FALSE,
                                                assumption_plot = FALSE,
                                                quite = FALSE,
                                                streamline = FALSE,
                                                return_result = FALSE) {

  ##################################### Set up #########################################
  # Temporary disable plots for glmer object
  data <- data_check(data) # check data and coerced into numeric

  if (simple_slope == TRUE) {
    if (use_package == "nlme") {
      warning("Switched use_package to lmerTest since you requested simple_slope")
      use_package <- "lmerTest"
    }
  }

  response_variable <- data %>%
    dplyr::select(!!enquo(response_variable)) %>%
    names()
  random_effect_factors <- data %>%
    dplyr::select(!!enquo(random_effect_factors)) %>%
    names()
  non_random_effect_factors <- data %>%
    dplyr::select(!!enquo(non_random_effect_factors)) %>%
    names()
  two_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(two_way_interaction_factor)) %>%
    names()
  three_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(three_way_interaction_factor)) %>%
    names()
  id <- data %>%
    dplyr::select(!!enquo(id)) %>%
    names()

  ##################################### Run Model #########################################
  if (is.null(family)) {
    model <- lme_model(
      model = model,
      data = data,
      response_variable = tidyselect::all_of(response_variable),
      random_effect_factors = tidyselect::all_of(random_effect_factors),
      non_random_effect_factors = tidyselect::all_of(non_random_effect_factors),
      two_way_interaction_factor = tidyselect::all_of(two_way_interaction_factor),
      three_way_interaction_factor = tidyselect::all_of(three_way_interaction_factor),
      id = tidyselect::all_of(id),
      opt_control = opt_control,
      na.action = na.action,
      estimation_method = estimation_method,
      use_package = use_package,
      quite = TRUE
    )
  } else {
    if (simple_slope == TRUE | interaction_plot == TRUE) {
      simple_slope <- FALSE
      interaction_plot <- FALSE
      warning("interaction_plot & simple_slope is not avaliable for glme model for now")
    }
    model <- glme_model(
      model = model,
      data = data,
      response_variable = tidyselect::all_of(response_variable),
      random_effect_factors = tidyselect::all_of(random_effect_factors),
      non_random_effect_factors = tidyselect::all_of(non_random_effect_factors),
      two_way_interaction_factor = tidyselect::all_of(two_way_interaction_factor),
      three_way_interaction_factor = tidyselect::all_of(three_way_interaction_factor),
      family = family,
      id = tidyselect::all_of(id),
      opt_control = opt_control,
      na.action = na.action,
      estimation_method = estimation_method,
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

  ############################### Generate Simple Slope Output ###############################
  if (simple_slope == TRUE) {
    simple_slope_list <- simple_slope(
      data = data,
      model = model,
      two_way_interaction_factor = two_way_interaction_factor,
      three_way_interaction_factor = three_way_interaction_factor
    )
  } else {
    simple_slope_list <- list(
      simple_slope_df = NULL,
      jn_plot = NULL
    )
  }

  ######################################### Output Result  #########################################
  if (model_summary == TRUE) {
    model_summary_list <- model_summary(
      model = model,
      streamline = streamline,
      digits = digits,
      return_result = TRUE,
      assumption_plot = assumption_plot,
      quite = quite
    )
  } else {
    model_summary_list <- NULL
  }


  if (simple_slope == TRUE & quite == FALSE) {
    super_print("underline|Slope Estimates at Each Level of Moderators")
    print_table(simple_slope_list$simple_slope_df)
    super_print("italic|Note: For continuous variable, low and high represent -1 and +1 SD from the mean, respectively.")
    print(simple_slope_list$jn_plot)
  }

  if (interaction_plot == TRUE) {
    try(print(interaction_plot_object))
  }
  # warning message
  plot_logical <- c(interaction_plot, simple_slope, assumption_plot)
  number_of_plot_requested <- length(plot_logical[plot_logical])
  if (number_of_plot_requested > 1) {
    warning("You requested > 2 plots. Since 1 plot can be displayed at a time, considering using Rmd for better viewing experience.")
  }

  # Return Result
  if (return_result == TRUE) {
    return_list <- list(
      model = model,
      summary = model_summary_list,
      interaction_plot = interaction_plot_object,
      simple_slope = simple_slope_list
    )
    return(return_list)
  }
}
