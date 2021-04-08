#' Model Summary with Interaction Plot
#'
#' It will first compute the mixed effect model. It will use either the nlme::lme ((Pinheiro, 2006) or the lmerTest::lmer (Kuznetsova, 2017) for linear mixed effect model. It will use lme4::glmer (Bates et al., 2014) for generalized linear mixed effect model. Then, it will graph the interaction using the two_way_interaction_plot or the three_way interaction_plot (Moy, 2021). If you requested simple slope summary, it will uses the interaction::sim_slopes (Long, 2019). If you requested bruceR_summary, it will uses the bruceR::HLM_summary (Bao, 2021)
#'
#' @param data required dataframe
#' @param response_variable required character or vector of length 1
#' @param level_1_factors vector. Level-1 variables (e.g., individual-level)
#' @param level_2_factors optional vector. level-2 variables (e.g., group-level)
#' @param two_way_interaction_factor optional vector of length more than 2. Default to `null`.
#' @param three_way_interaction_factoroptional vector of length 3. Do not include two-way interaction factors if this is not null. Default to `null`.
#' @param id character or vector of length 1. The nesting variable (e.g. group).
#' @param family a GLM family. It will pased to the family argument in glmer. See `?glmer` for possible options. It will also call glmer.nb if family = 'negbin'
#' @param graph_label_name optional vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, [predict_var3]). Function should be passed as a switch function. See below for an example.
#' @param estimation_method character. `ML` or `REML` default to `REML`.
#' @param return_result optional vector. Choose from  `model`,`plot`,`short_summary`,`long_summary`,`bruceR_summary`.`model` return the model object. `plot` return the interaction plot.`short_summary` return a short model summary. `long_summary` return the summary. `bruceR_summary` uses the bruceR::HLM_summary (Bao, 2021) function. 
#' @param print_result  optional vector. Choose from `model`,`plot`,`short_summary`,`long_summary`.`model` return the model object. `plot` return the interaction plot.`short_summary` return a short model summary. `long_summary` return the summary.
#' @param na.action default to `na.exclude`.
#' @param cateogrical_var list. use the form list(var_name1 = c(upper_bound1, lower_bound1), [var_name2 = c(upper_bound2, lower_bound2]).
#' @param opt_control character. default to `optim` for `lme` and `bobyqa` for lmerTest
#' @param model_performance  vector. `R2_full_model` for conditional R2 and `R2_fixed_effect` for marginal R2 (Nakagawa, 2013). `icc` for intraclass correlation coefficient. The function calls the performance package for R2 and ICC (Lüdecke et al., 2020). 
#' @param y_lim vector of length 2. c(lower_limit, upper_limit)
#' @param plot_color logical. default as F. Set to T if you want to plot in color
#' @param estimate_round  numeric. number of digit to round to for the slope estimate.
#' @param p_value_round numeric. number of digit to round to for the p-value.
#' @param performance_round numeric. number of digit to round to for the performance statistics (e.g., R2)
#' @param use_package character. Only avaliable for linear mixed effect model. Options are "nlme" or "lmerTest". Default is "nlme".
#' @param quite default to F. If set to `T`, it will not print the fitting model statement
#'
#' @references
#' Bao, H.-W.-S. (2021). bruceR: Broadly useful convenient and efficient R functions. R package version 0.6.0. https://CRAN.R-project.org/package=bruceR
#'
#' Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1–48. doi: 10.18637/jss.v067.i01.
#'
#' Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. (2017). lmerTest package: tests in linear mixed effects models. Journal of statistical software, 82(13), 1-26.
#'
#' Long J. A. (2019). interactions: Comprehensive, User-Friendly Toolkit for Probing Interactions. R package version 1.1.0, https://cran.r-project.org/package=interactions.
#' 
#' Lüdecke, D., Makowski, D., Waggoner, P., Patil I (2020). performance: Assessment of Regression Models Performance. CRAN. doi: 10.5281/zenodo.3952174, R package, https://easystats.github.io/performance/.
#' 
#' Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modelling in R. R package version 0.1.0, https://github.com/jasonmoy28/psycModel.#'
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
#' model_summary_with_plot(response_variable = 'incidence',
#'                         level_1_factors = 'size',
#'                         level_2_factors = 'herd',
#'                         two_way_interaction_factor = c('size','herd'),
#'                         id = 'period',
#'                         data = lme4::cbpp,
#'                         use_package = 'lmerTest', #use lmerTest
#'                         model_performance = NULL)
#'
model_summary_with_plot = function(data, response_variable,
                                   level_1_factors,
                                   level_2_factors = NULL,
                                   two_way_interaction_factor = NULL,
                                   three_way_interaction_factor = NULL,
                                   cateogrical_var = NULL,
                                   id,
                                   family = NULL,
                                   graph_label_name = NULL,
                                   estimation_method = 'REML',
                                   opt_control = 'optim',
                                   na.action = na.exclude,
                                   model_performance = c('R2_fixed_effect','R2_full_model'),
                                   return_result = NULL,
                                   print_result = c('short_summary','plot'),
                                   y_lim = NULL,
                                   plot_color = F,
                                   estimate_round = 3,
                                   p_value_round = 3,
                                   performance_round = 3,
                                   use_package = 'nlme',
                                   quite = F) {

  # Temporary disbale plots for glmer object
  if (!is.null(family)) {
    warning('The interaction plots produced is not fully tested. Please use it at your own risk')
  }

  # All data must be dummy-code or factorized before passing into the function
  # Check datatype is correct
  datatype = as.vector(sapply(data, class))
  if(all(datatype == 'numeric'| datatype == 'factor' | datatype == 'integer')){
    data = data %>% dplyr::mutate_all(as.numeric)
  } else{
    return('Error: All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()')
  }

  if (!is.null(two_way_interaction_factor) & !is.null(three_way_interaction_factor)) {
    return('Error: Cannot passed both two_way_interaction_factor and three_way_interaction_factor. Passing three_way_interaction_factor automatically include all two-way interactions.')
  }

  if(any(print_result %in% 'long_summary') | any(return_result %in% 'long_summary') |
     any(print_result %in% 'simple_slope') | any(return_result %in% 'simple_slope')){
    if (use_package == 'nlme'){
      warning('use_package switch to lmerTest since you requested long_summary or simple_slope. Currently, we only support ModLmerTest (i.e, lmerTest) object for the two summary requests')
      use_package = 'lmerTest'
    }
  }
  if (is.null(family)) {
    model = lme_model(data = data,
                      response_variable = response_variable,
                      level_1_factors = level_1_factors,
                      level_2_factors = level_2_factors,
                      two_way_interaction_factor = two_way_interaction_factor,
                      three_way_interaction_factor = three_way_interaction_factor,
                      id = id,
                      opt_control = opt_control,
                      na.action = na.action,
                      estimation_method = estimation_method,
                      use_package = use_package,
                      quite = quite)
  } else{
    model = glme_model(data = data,
                      response_variable = response_variable,
                      level_1_factors = level_1_factors,
                      level_2_factors = level_2_factors,
                      family = family,
                      two_way_interaction_factor = two_way_interaction_factor,
                      three_way_interaction_factor = three_way_interaction_factor,
                      id = id,
                      na.action = na.action,
                      estimation_method = estimation_method,
                      quite = quite)
  }

  if (!is.null(two_way_interaction_factor) & (any(print_result %in% 'plot') | any(return_result %in% 'plot'))) {
    graphing_interaction_factor = two_way_interaction_factor[1:2]
    interaction_plot = two_way_interaction_plot(data = data,
                                                model = model,
                                                response_var = response_variable,
                                                predict_var_name = graphing_interaction_factor,
                                                cateogrical_var = cateogrical_var,
                                                graph_label_name = graph_label_name,
                                                y_lim = y_lim,
                                                plot_color = plot_color)

  } else if (!is.null(three_way_interaction_factor) & (any(print_result %in% 'plot') | any(return_result %in% 'plot'))) {
    interaction_plot = three_way_interaction_plot(data = data,
                                                  model = model,
                                                  response_var = response_variable,
                                                  predict_var_name = three_way_interaction_factor,
                                                  cateogrical_var = cateogrical_var,
                                                  graph_label_name = graph_label_name,
                                                  y_lim = y_lim,
                                                  plot_color = plot_color)
  } else{
    interaction_plot = NULL
  }

  if (any(print_result %in% 'short_summary') | any(return_result %in% 'short_summary')) {
    model_summary_df = model_summary(model = model,
                                     model_performance = model_performance,
                                     estimate_round = estimate_round,
                                     p_value_round = p_value_round,
                                     performance_round = performance_round)
  } else {
    model_summary_df = NULL
  }

  if(any(print_result %in% 'simple_slope')){
    if(!is.null(two_way_interaction_factor)){
      simple_slope = interactions::sim_slopes(model = model,
                                              pred = !!two_way_interaction_factor[1],
                                              modx = !!two_way_interaction_factor[2],
                                              jnplot = T)
    }

    if(!is.null(three_way_interaction_factor)){
      simple_slope = interactions::sim_slopes(model = model,
                                              pred = !!three_way_interaction_factor[1],
                                              modx = !!three_way_interaction_factor[2],
                                              mod2 = !!three_way_interaction_factor[3],
                                              jnplot = T)
    }
  }

  # Check print result
  if (any(print_result %in% 'short_summary')) {
    print(model_summary_df)
  }

  if(any(print_result %in% 'bruceR_summary')){
    check_package = requireNamespace('bruceR')
    if (check_package == F) {
      response = readline('Install bruceR package? It may take a long time to install. Enter Y/N ')
      if (stringr::str_to_upper(response) == 'Y') {
        utils::install.packages('bruceR')
        bruceR::HLM_summary(model = model,nsmall = estimate_round)
      } else{
        print('Installation Halted. Please do not pass "bruceR_summary" to the print_result argument')
      } 
    } else{
      bruceR::HLM_summary(model = model,nsmall = estimate_round)
    }
  }

  if(any(print_result %in% 'long_summary')){
    print(summary(model = model))
  }

  if(any(print_result %in% 'simple_slope')){
    print(simple_slope)
  }

  if(any(print_result %in% 'plot')){
    try(print(interaction_plot))
  }


  # Check return result
  if (length(return_result) != 0) {
    if(any(return_result %in% 'plot')) {
      return_plot = interaction_plot
    } else{return_plot = NULL}

    if (any(return_result %in% 'short_summary')) {
      return_short_summary = model_summary_df
    } else{return_short_summary = NULL}

    if(any(return_result %in% 'model')) {
      return_model = model
    } else{return_model = NULL}


    if(any(return_result %in% 'long_summary')) {
      return(summary(model = model))
    } else{return_long_summary = NULL}

    return_list = list(return_model,return_short_summary,return_long_summary,return_bruceR_summary,return_plot)
    return_list = return_list[!sapply(return_list,is.null)]
    return(return_list)
  }

}

