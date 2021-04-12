#' Integrated Function for Mixed Effect Model
#'
#' `r lifecycle::badge("stable")` \cr
#' It will first compute the mixed effect model. It will use either the nlme::lme (Pinheiro, 2006) or the lmerTest::lmer (Kuznetsova, 2017) for linear mixed effect model. It will use lme4::glmer (Bates et al., 2014) for generalized linear mixed effect model. Then, it will graph the interaction using the two_way_interaction_plot or the three_way_interaction_plot function. If you requested simple slope summary, it will uses the interaction::sim_slopes (Long, 2019). If you requested , it will uses the bruceR::HLM_summary (Bao, 2021)
#'
#' @param data data frame
#' @param model lme4 model syntax. Support more complicated model. Note that model_summary will only return fixed effect estimates.
#' @param response_variable DV (i.e., outcome variable / response variable). Length of 1. Support `dplyr::select` syntax.
#' @param random_effect_factors random effect factors (level-1 variable for HLM people) Factors that need to estimate fixed effect and random effect (i.e., random slope / varying slope based on the id). Support `dplyr::select` syntax.
#' @param non_random_effect_factors non-random effect factors (level-2 variable for HLM people). Factors only need to estimate fixed effect. Support `dplyr::select` syntax.
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select` syntax.
#' @param id the nesting variable (e.g. group, time). Length of 1. Support `dplyr::select` syntax.
#' @param family a GLM family. It will passed to the family argument in glmer. See `?glmer` for possible options. Early stage feature. Have not tested nor fully supported. `r lifecycle::badge("experimental")` \cr
#' @param graph_label_name optional vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, ...). Function should be passed as a switch function (see ?two_way_interaction_plot for an example)
#' @param estimation_method character. `ML` or `REML` default is `REML`.
#' @param return_result default is `F`. If set to `T`, it will return the `model`, `model_summary`, and `plot` (if the interaction term is included)
#' @param print_result  Default is `short_summary` and `plot`. Options are `model`,`plot`,`short_summary`,`long_summary`.`model` return the model object. `plot` return the interaction plot.`short_summary` return a short model summary. `long_summary` return the summary.
#' @param na.action default is `stats::na.exclude`. Required to be `stats::na.omit` if check_assumption if `T`.
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param opt_control default is `optim` for `lme` and `bobyqa` for lmerTest
#' @param model_performance  Default is `R2_full_model` and `R2_fixed_effect`. Options are `R2_full_model` for conditional R2 and `R2_fixed_effect` for marginal R2 (Nakagawa, 2013). `icc` for intraclass correlation coefficient. The function uses the performance package for R2 and ICC (Lüdecke et al., 2020).
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color default if `F`. Set to `T` if you want to plot in color
#' @param use_package Default is `nlme`. Only available for linear mixed effect model. Options are `nlme` or `lmerTest`,`lme4`(`'lme4` return similiar result as `lmerTest` except the return model)
#' @param quite default to `F`. If set to `T`, it will not print the fitting model statement
#' @param digit number of digit. Specify as a vector with `c(estimate_digit, p_value_digit, model_performance_digit)`
#' @param simple_slope default is `F`. If `T`, it will compute the slope differing with ± 1 SD of the IVs using interaction:sim_slopes function (Long, 2019). See `vignette('regression_model')` for more info
#' @param check_assumption default is `F`. If `T`, it will generate an panel of plots that check major assumptions. It uses the performance::check_model (Lüdecke, 2020). see ?performance::check_model to learn model
#'
#' @references
#' Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1–48. doi: 10.18637/jss.v067.i01.
#'
#' Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. (2017). lmerTest package: tests in linear mixed effects models. Journal of statistical software, 82(13), 1-26.
#'
#' Long J. A. (2019). interactions: Comprehensive, User-Friendly Toolkit for Probing Interactions. R package version 1.1.0, https://cran.r-project.org/package=interactions.
#'
#' Lüdecke, D., Makowski, D., Waggoner, P., Patil I (2020). performance: Assessment of Regression Models Performance. CRAN. doi: 10.5281/zenodo.3952174, R package, https://easystats.github.io/performance/.
#'
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
#'
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x
#'
#' Pinheiro, J., Bates, D., DebRoy, S., Sarkar, D., & Team, R. C. (2006). nlme: Linear and nonlinear mixed effects models. R package version, 3(4), 109.
#'
#' @return
#' return a list of all requested items in the order of model, short_summary, long_summary, plot
#' @export
#'
#' @examples
#'
#' fit1 <- model_summary_with_plot(
#'   response_variable = JS_Individual,
#'   random_effect_factors = Age_Individual,
#'   non_random_effect_factors = Hofstede_IC_Country,
#'   two_way_interaction_factor = c(Age_Individual, Hofstede_IC_Country),
#'   id = Country,
#'   graph_label_name = c("Job Satisfaction", "Age", "Hofstede_IC_Country"),
#'   plot_color = TRUE, # plot with color
#'   data = EWCS_2015_shorten,
#'   check_assumption = TRUE,
#'   simple_slope = TRUE,
#' )
model_summary_with_plot <- function(data,
                                    model = NULL,
                                    response_variable = NULL,
                                    random_effect_factors = NULL,
                                    non_random_effect_factors = NULL,
                                    two_way_interaction_factor = NULL,
                                    three_way_interaction_factor = NULL,
                                    cateogrical_var = NULL,
                                    id = NULL,
                                    family = NULL,
                                    graph_label_name = NULL,
                                    estimation_method = "REML",
                                    opt_control = "optim",
                                    na.action = stats::na.exclude,
                                    model_performance = c("R2_fixed_effect", "R2_full_model"),
                                    return_result = F,
                                    print_result = c("short_summary", "plot"),
                                    y_lim = NULL,
                                    plot_color = F,
                                    digit = c(3, 3, 3),
                                    use_package = "nlme",
                                    simple_slope = F,
                                    check_assumption = F,
                                    quite = F) {

  # Temporary disable plots for glmer object
  if (!is.null(family)) {
    warning("The interaction plots produced is not fully tested. Please use it at your own risk")
  }
  data <- data_check(data) # check data and coerced into numeric

  if (simple_slope == T) {
    if (use_package == "nlme") {
      warning("Switched use_package to lmerTest since you requested simple_slope")
      use_package <- "lmerTest"
    }
  }
  if (check_assumption == T) {
    na.action <- stats::na.omit
    warning("Switched to na.omit since you request check_assumption.")
  }

  if (is.null(family)) {
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
  } else {
    model <- glme_model(
      data = data,
      response_variable = response_variable,
      random_effect_factors = random_effect_factors,
      non_random_effect_factors = non_random_effect_factors,
      family = family,
      two_way_interaction_factor = two_way_interaction_factor,
      three_way_interaction_factor = three_way_interaction_factor,
      id = id,
      na.action = na.action,
      estimation_method = estimation_method,
      quite = quite
    )
  }
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

  if (any(print_result %in% "short_summary") | return_result == T) {
    model_summary_df <- model_summary(
      model = model,
      model_performance = model_performance,
      estimate_round = digit[1],
      p_value_round = digit[2],
      performance_round = digit[3]
    )
  } else {
    model_summary_df <- NULL
  }

  if (simple_slope == T) {
    if (length(two_way_interaction_factor) != 0) {
      simple_slope_model <- interactions::sim_slopes(
        data = data,
        model = model,
        pred = !!two_way_interaction_factor[1],
        modx = !!two_way_interaction_factor[2],
        jnplot = T,
      )
    }

    if (length(three_way_interaction_factor) != 0) {
      simple_slope_model <- interactions::sim_slopes(
        data = data,
        model = model,
        pred = !!three_way_interaction_factor[1],
        modx = !!three_way_interaction_factor[2],
        mod2 = !!three_way_interaction_factor[3],
        jnplot = T
      )
    }
  }

  # Check print result
  if (any(print_result %in% "model")) {
    print(model)
  }

  if (any(print_result %in% "short_summary")) {
    print(model_summary_df)
  }

  if (any(print_result %in% "long_summary")) {
    print(summary(model = model))
  }

  if (any(print_result %in% "plot")) {
    try(print(interaction_plot))
  }

  if (simple_slope == T) {
    print(simple_slope_model)
    warning("Overrided interaction plot. Set simple_slope to FALSE to see interaction plot. Knit a Rmd file to see all plots for better experience.")
  }

  if (check_assumption == T) {
    print(performance::check_model(model))
    if (simple_slope == T) {
      warning("Overrided simple slope plot. Set check_assumption to FALSE to see simple slope plot. Knit a Rmd file to see all plots for better experience.")
    }
    if (any(print_result %in% "plot")) {
      warning("Overrided interaction plot. Set check_assumption to FALSE to see interaction plot. Knit a Rmd file to see all plots for better experience.")
    }
  }
  if (return_result == T) {
    return_list <- list(model = model, summary = model_summary_df, plot = interaction_plot)
    return(return_list)
  }
}
