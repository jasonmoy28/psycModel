#' Integrated Function for Mixed Effect Model
#'
#' `r lifecycle::badge("experimental")` \cr
#' It will first compute the mixed effect model. It will use either the nlme::lme (Pinheiro, 2006) or the lmerTest::lmer (Kuznetsova, 2017) for linear mixed effect model. It will use lme4::glmer (Bates et al., 2014) for generalized linear mixed effect model. Then, it will graph the interaction using the two_way_interaction_plot or the three_way_interaction_plot function. If you requested simple slope summary, it will uses the interaction::sim_slopes (Long, 2019).
#'
#' @param data data frame
#' @param response_variable DV (i.e., outcome variable / response variable). Length of 1. Support `dplyr::select` syntax.
#' @param predictor_variable IV.  Support `dplyr::select` syntax.
#' @param three_way_interaction_factor three-way interaction factor. You need to pass exactly 3 factors. Specifying three-way interaction factors automatically included all two-way interactions, so please do not specify the two_way_interaction_factor argument. Support `dplyr::select` syntax.
#' @param graph_label_name optional vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, ...). Function should be passed as a switch function (see ?two_way_interaction_plot for an example)
#' @param return_result If set to `TRUE` (default is `FALSE`), it will return the `model`, `model_summary`, and `plot` (if the interaction term is included)
#' @param print_result  Default is `model_summary` and `plot`. Options are `model_summary` or `plot`.
#' @param cateogrical_var list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of `list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))`
#' @param y_lim the plot's upper and lower limit for the y-axis. Length of 2. Example: `c(lower_limit, upper_limit)`
#' @param plot_color If it is set to `TRUE` (default is `FALSE`), the interaction plot will plot with color.
#' @param quite suppress printing output
#' @param digits number of digit.
#' @param simple_slope Compute the slope differing with ± 1 SD of the IVs. In the background, it calls interaction:sim_slopes()
#' @param assumption_plot Generate an panel of plots that check major assumptions. You can use this if the model summary show violation of assumption (those maybe unreliable due to the use of p-value which is sensitive to the sample size). In the background, it calls performance::check_model()
#' @param streamlined_output Only print model estimate and model performance. Default is `FALSE`
#' @param model explicit model of object `lm`
#' @param two_way_interaction_factor two-way interaction factors. You need to pass 2+ factor. Support `dplyr::select` syntax.
#'
#' @return
#' return a list of all requested items in the order of model, model_summary, plot
#' @export
#'
#' @examples
#'
integrated_model_summary <- function(data,
                                     model = NULL,
                                     response_variable = NULL,
                                     predictor_variable = NULL,
                                     two_way_interaction_factor = NULL,
                                     three_way_interaction_factor = NULL,
                                     cateogrical_var = NULL,
                                     streamlined_output = F,
                                     graph_label_name = NULL,
                                     return_result = F,
                                     print_result = c("model_summary", "plot"),
                                     y_lim = NULL,
                                     plot_color = F,
                                     digits = 3,
                                     simple_slope = F,
                                     assumption_plot = F,
                                     quite = F) {
  
  ##################################### Set up #########################################
  # Temporary disable plots for glmer object
  data <- data_check(data) # check data and coerced into numeric
  
  ##################################### Run Model #########################################
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
  
  model <- lm_model(data = data,
                    response_variable = dplyr::all_of(response_variable),
                    predictor_variable =  dplyr::all_of(predictor_variable),
                    two_way_interaction_factor = dplyr::all_of(two_way_interaction_factor),
                    three_way_interaction_factor = dplyr::all_of(three_way_interaction_factor),
                    quite = T)
  
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
        simple_slope_output = simple_slope_model$slopes %>% 
          dplyr::mutate(Mod_1_Level = c('-1 SD','Mean','+1 SD')) %>% 
          select(-1) %>% 
          dplyr::rename(ci.lower = '2.5%') %>% 
          dplyr::rename(ci.upper = '97.5%') %>% 
          select('Mod_1_Level', 'Est.', 't val.', 'S.E.', 'p', 'ci.lower','ci.upper')
        colnames(simple_slope_output)[1] = c(paste(two_way_interaction_factor[2],'Level'))
        jnp_plot = simple_slope_model$jnplot
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
        simple_slope_output = rbind(simple_slope_model$slopes[[1]],simple_slope_model$slopes[[2]],simple_slope_model$slopes[[3]]) %>% 
          dplyr::mutate(Mod_2_Level = c(rep('-1 SD',3),rep('Mean',3),rep('+1 SD',3))) %>% 
          dplyr::mutate(Mod_1_Level = rep(c('-1 SD','Mean','+1 SD'),3)) %>% 
          dplyr:select(-1) %>% 
          dplyr::rename(ci.lower = '2.5%') %>% 
          dplyr::rename(ci.upper = '97.5%') %>% 
          dplyr::select('Mod_2_Level', 'Mod_1_Level', 'Est.', 't val.', 'S.E.', 'p', 'ci.lower','ci.upper') %>%
          dplyr::mutate(across('Mod_2_Level', ~ replace(., duplicated(.), '')))
        colnames(simple_slope_output)[c(1,2)] = c(paste(three_way_interaction_factor[2],'Level'),paste(three_way_interaction_factor[3],'Level'))
        jnp_plot = simple_slope_output$jnplot
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
      digits = digits,
      return_result = T,
      assumption_plot = assumption_plot
    )
  }
  
  if (any(print_result %in% "plot")) {
    try(print(interaction_plot))
  }
  
  if (simple_slope == T) {
    super_print('underline|Slope Estimates at Each Level of Moderators')
    print_table(simple_slope_output)
    print(jnp_plot)
  }
  
  # Return Result
  if (return_result == T) {
    return_list <- list(model = model, summary = model_summary_df, plot = interaction_plot)
    return(return_list)
  }
}
