#' Integrated Function for Mixed Effect Model
#'
#' `r lifecycle::badge("experimental")` \cr
#' It will first compute the mixed effect model. It will use either the nlme::lme (Pinheiro, 2006) or the lmerTest::lmer (Kuznetsova, 2017) for linear mixed effect model. It will use lme4::glmer (Bates et al., 2014) for generalized linear mixed effect model. Then, it will graph the interaction using the two_way_interaction_plot or the three_way_interaction_plot function. If you requested simple slope summary, it will uses the interaction::sim_slopes (Long, 2019).
#'
#' @param data data frame
#' @param model lme4 model syntax. Support more complicated model. Note that model_summary will only return fixed effect estimates.
#' @param response_variable DV (i.e., outcome variable / response variable). Length of 1. Support `dplyr::select` syntax.
#' @param random_effect_factors random effect factors (level-1 variable for HLM people) Factors that need to estimate fixed effect and random effect (i.e., random slope / varying slope based on the id). Support `dplyr::select` syntax.
#' @param non_random_effect_factors non-random effect factors (level-2 variable for HLM people). Factors only need to estimate fixed effect. Support `dplyr::select` syntax.
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select` syntax.
#' @param id the nesting variable (e.g. group, time). Length of 1. Support `dplyr::select` syntax.
#' @param graph_label_name optional vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, ...). Function should be passed as a switch function (see ?two_way_interaction_plot for an example)
#' @param estimation_method character. `ML` or `REML` default is `REML`.
#' @param return_result If set to `TRUE` (default is `FALSE`), it will return the `model`, `model_summary`, and `plot` (if the interaction term is included)
#' @param print_result  Default is `model_summary` and `plot`. Options are `model_summary` or `plot`.
#' @param na.action default is `stats::na.exclude`. Required to be `stats::na.omit` if assumption_plot if `T`. It should produce similar result, but you should check using `na.exclude` to confirm.
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param opt_control default is `optim` for `lme` and `bobyqa` for lmerTest
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color If it is set to `TRUE` (default is `FALSE`), the interaction plot will plot with color.
#' @param use_package Default is `nlme`. Only available for linear mixed effect model. Options are `nlme` or `lmerTest`,`lme4`(`'lme4` return similar result as `lmerTest` except the return model)
#' @param quite If it is set to `TRUE` (default is `FALSE`), it will not print the fitting model statement
#' @param round number of digit.
#' @param simple_slope Compute the slope differing with ± 1 SD of the IVs. In the background, it calls interaction:sim_slopes()
#' @param assumption_plot Generate an panel of plots that check major assumptions. You can use this if the model summary show violation of assumption (those maybe unreliable due to the use of p-value which is sensitive to the sample size). In the background, it calls performance::check_model()
#' @param streamlined_output Only print model estimate and model performance. Default is `FALSE`
#'
#' @return
#' return a list of all requested items in the order of model, model_summary, plot
#' @export
#'
#' @examples
#' fit <- model_summary_with_plot(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = c(extrav),
#'   non_random_effect_factors = texp,
#'   two_way_interaction_factor = c(extrav, texp),
#'   graph_label_name = c("popular", "extraversion", "teacher experience"),
#'   id = class
#' )
#' \dontrun{
#' fit <- model_summary_with_plot(
#'   data = popular,
#'   response_variable = popular,
#'   random_effect_factors = c(extrav, sex),
#'   non_random_effect_factors = texp,
#'   three_way_interaction_factor = c(extrav, sex, texp), # three-way interaction
#'   graph_label_name = c("popular", "extraversion", "sex", "teacher experience"), 
#'   id = class,
#'   simple_slope = TRUE, #you can request simple slope
#'   assumption_plot = TRUE, # see can also request assumption plot
#'   plot_color = TRUE # you can also request the plot in color
#' )}
#' 
model_summary_with_plot <- function(data,
                                    model = NULL,
                                    response_variable = NULL,
                                    random_effect_factors = NULL,
                                    non_random_effect_factors = NULL,
                                    two_way_interaction_factor = NULL,
                                    three_way_interaction_factor = NULL,
                                    cateogrical_var = NULL,
                                    id = NULL,
                                    streamlined_output = F,
                                    graph_label_name = NULL,
                                    estimation_method = "REML",
                                    opt_control = "bobyqa",
                                    na.action = stats::na.omit,
                                    return_result = F,
                                    print_result = c("model_summary", "plot"),
                                    y_lim = NULL,
                                    plot_color = F,
                                    round = 3,
                                    use_package = "lmerTest",
                                    simple_slope = F,
                                    assumption_plot = F,
                                    quite = F) {

  ##################################### Set up #########################################
  # Temporary disable plots for glmer object
  data <- data_check(data) # check data and coerced into numeric

  if (simple_slope == T) {
    if (use_package == "nlme") {
      warning("Switched use_package to lmerTest since you requested simple_slope")
      use_package <- "lmerTest"
    }
  }

  ##################################### Run Model #########################################
  model <- lme_model(
    model = model,
    data = data,
    response_variable = enquo(response_variable),
    random_effect_factors = enquo(random_effect_factors),
    non_random_effect_factors = enquo(non_random_effect_factors),
    two_way_interaction_factor = enquo(two_way_interaction_factor),
    three_way_interaction_factor = enquo(three_way_interaction_factor),
    id = enquo(id),
    opt_control = opt_control,
    na.action = na.action,
    estimation_method = estimation_method,
    use_package = use_package,
    quite = quite,
    ... = "model_summary_with_plot"
  )

  ############################### Generate Interaction Plots ###############################
  two_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(two_way_interaction_factor)) %>%
    names()
  three_way_interaction_factor <- data %>%
    dplyr::select(!!enquo(three_way_interaction_factor)) %>%
    names()
  interaction_plot <- NULL
  if (length(two_way_interaction_factor) != 0 & (any(print_result %in% "plot") | return_result == T)) {
    interaction_plot <- two_way_interaction_plot(
      model = model,
      cateogrical_var = cateogrical_var,
      graph_label_name = graph_label_name,
      y_lim = y_lim,
      plot_color = plot_color
    )
  } else if (length(three_way_interaction_factor) != 0 & (any(print_result %in% "plot") | return_result == T)) {
    interaction_plot <- three_way_interaction_plot(
      model = model,
      cateogrical_var = cateogrical_var,
      graph_label_name = graph_label_name,
      y_lim = y_lim,
      plot_color = plot_color
    )
  } else {
    interaction_plot <- NULL
  }


  if (simple_slope == T) {
    if (length(two_way_interaction_factor) != 0) {
      if (requireNamespace("interactions", quietly = TRUE)) {
        simple_slope_model <- interactions::sim_slopes(
          data = data,
          model = model,
          pred = !!two_way_interaction_factor[1],
          modx = !!two_way_interaction_factor[2],
          jnplot = T,
        )
      } else {
        stop("please install.packages('interactions') to use simple_slope")
      }
    }
    if (length(three_way_interaction_factor) != 0) {
      if (all(unlist(lapply(c("cowplot", "interactions"), requireNamespace)))) {
        simple_slope_model <- interactions::sim_slopes(
          data = data,
          model = model,
          pred = !!three_way_interaction_factor[1],
          modx = !!three_way_interaction_factor[2],
          mod2 = !!three_way_interaction_factor[3],
          jnplot = T
        )
      } else {
        stop("please install.packages(c('cowplot','interactions')) use simple_slope with three-way interaction")
      }
    }
  }

  # Print result
  if (any(print_result %in% "model_summary") | return_result == T) {
    model_summary_df <- model_summary(
      model = model,
      streamlined_output = streamlined_output,
      round = round,
      return_result = T,
      assumption_plot = assumption_plot
    )
  }

  if (any(print_result %in% "plot")) {
    try(print(interaction_plot))
  }

  if (simple_slope == T) {
    print(simple_slope_model)
  }

  # Return Result
  if (return_result == T) {
    return_list <- list(model = model, summary = model_summary_df, plot = interaction_plot)
    return(return_list)
  }
}
