#' ANOVA Plot  
#'
#' `r lifecycle::badge("experimental")` \cr
#' Plot categorical variable with barplot. Continuous moderator are plotted at ± 1 SD from the mean. 
#' 
#' @param model fitted model (usually `lm` or `aov` object). Variables must be converted to correct data type before fitting the model. Specifically,  continuous variables must be converted to type `numeric` and categorical variables to type `factor`. 
#' @param predictor predictor variable. Must specified for non-interaction plot and must not specify for interaction plot. 
#' @param graph_label_name vector or function. Vector should be passed in the form of `c(response_var, predict_var1, predict_var2, ...)`. Function should be passed as a switch function that return the label based on the name passed (e.g., a switch function)
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
#' # Main effect plot with 1 categorical variable
#' fit_1 = lavaan::HolzingerSwineford1939 %>% 
#'   dplyr::mutate(school = as.factor(school)) %>% 
#'   lm(data = ., grade ~ school)
#' anova_plot(fit_1,predictor = school)
#'
#' # Interaction effect plot with 2 categorical variables 
#' fit_2 = lavaan::HolzingerSwineford1939 %>% 
#'   dplyr::mutate(dplyr::across(c(school,sex),as.factor)) %>% 
#'   lm(data = ., grade ~ school*sex)
#' anova_plot(fit_2)
#' 
#' # Interaction effect plot with 1 categorical variable and 1 continuous variable
#' fit_3 = lavaan::HolzingerSwineford1939 %>% 
#'   dplyr::mutate(school = as.factor(school)) %>% 
#'   dplyr::mutate(ageyr = as.numeric(ageyr)) %>% 
#'   lm(data = ., grade ~ ageyr*school)
#' anova_plot(fit_3)
#' 
#' 
anova_plot <- function(model,
                       predictor = NULL,
                       graph_label_name = NULL) {
  
  response_var_name = insight::find_response(model)
  response_var = dplyr::enquo(response_var_name)
  
  data = insight::get_data(x = model) %>% dplyr::mutate(dplyr::across(where(is.integer),as.numeric)) # temporary solution for treating integer as numeric
  
  interaction_term = get_interaction_term(model)
  
  ########################################### ANOVA plot without interaction ##################################################################
  if (is.null(interaction_term)) {
    try({if(!rlang::is_symbol(predictor)) {predictor <- dplyr::sym(predictor)}},silent = TRUE)
    predictor = dplyr::enquo(predictor)
    
    mean = data %>%
      dplyr::group_by(!!predictor) %>%
      dplyr::summarise(dplyr::across(!!response_var, ~ mean(., na.rm = TRUE))) %>%
      dplyr::rename(mean = !!response_var)
    
    se = data %>%
      dplyr::group_by(!!predictor) %>%
      dplyr::summarise(dplyr::across(
        !!response_var,
        ~ stats::sd(., na.rm = TRUE) / sqrt(dplyr::n())
      )) %>%
      dplyr::rename(se = !!response_var)
    
    plot_df = mean %>% dplyr::full_join(se) %>%
      dplyr::rename(predict_var1 = !!predictor)
    
    # label name
    predictor_name = data %>% dplyr::select(!!predictor) %>% colnames()
    label_name = label_name(
      graph_label_name = graph_label_name,
      response_var_name = response_var_name,
      predict_var1_name = predictor_name,
      predict_var2_name = NULL,
      predict_var3_name = NULL
    )
    
    main_plot = plot_df %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$predict_var1, y = mean)) +
      ggplot2::geom_bar(
        stat = 'identity',
        width = 0.5,
        color = 'black',
        fill = 'white',
        position = ggplot2::position_dodge(0.9)
      ) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                             position = ggplot2::position_dodge(0.9),
                             width = 0.1) +
      ggplot2::labs(y = label_name[1],
                    x = label_name[2])
    
    ########################################### ANOVA plot with interaction ##################################################################
  } else{
    if (is.null(predictor)) {
      # get predictor if predictor is not supplied
      predict_var1 = get_interaction_term(model)$predict_var1
      predict_var2 = get_interaction_term(model)$predict_var2
      predict_vars = c(predict_var1, predict_var2)
      if (length(get_interaction_term(model)) == 3) {
        predict_var3 = get_interaction_term(model)$predict_var3
        predict_vars = c(predict_vars, predict_var3)
      }
    } else{
      predict_var1 = predictor[1]
      predict_var2 = predictor[2]
      predict_vars = c(predict_var1, predict_var2)
      if (length(predictor) == 3) {
        predict_var2 = predictor[3]
        predict_vars = c(predict_vars, predict_var3)
      }
    }
    predict_vars = ggplot2::enquos(predict_vars)
    
    ##################################### Two-way Interaction #####################################
    if (length(get_interaction_term(model)) == 2) {
      data_type = data %>%
        dplyr::summarise(dplyr::across(!!!predict_vars, class)) %>%
        tidyr::pivot_longer(dplyr::everything(),names_to = 'name',values_to = 'value')
      
      numeric_var_count = data_type %>%
        dplyr::filter(.data$value == 'numeric') %>%
        nrow()
      # 1 continuous & 1 categorical variable 
      if (numeric_var_count == 1) { 
        num_var_name = data_type %>% dplyr::filter(.data$value == 'numeric') %>% dplyr::select('name') %>% dplyr::pull()
        cat_var_name = data_type %>% dplyr::filter(.data$value == 'factor') %>% dplyr::select('name') %>% dplyr::pull()
        
        mean_df = get_predict_df(data = data)$mean_df
        upper_df = get_predict_df(data = data)$upper_df
        lower_df = get_predict_df(data = data)$lower_df
        plot_df = data.frame()
        for (level in levels((data[[cat_var_name]]))) {
          upper_new_data = mean_df
          upper_new_data[num_var_name] = upper_df[num_var_name]
          upper_new_data[cat_var_name] = level
          
          lower_new_data = mean_df
          lower_new_data[cat_var_name] = level
          lower_new_data[num_var_name] = lower_df[num_var_name]
          if (inherits(model,'lm')) {
            upper_predicted = stats::predict(model, newdata = upper_new_data, se.fit = TRUE)
            lower_predicted = stats::predict(model, newdata = lower_new_data, se.fit = TRUE)
            
            plot_df = rbind(plot_df, data.frame(cat_var_name = level,
                                                num_var_name = 'high',
                                                mean = upper_predicted$fit,
                                                se = upper_predicted$se.fit))
            
            plot_df = rbind(plot_df, data.frame(cat_var_name = level,
                                                num_var_name = 'low',
                                                mean = lower_predicted$fit,
                                                se = lower_predicted$se.fit))
            
          } else if(inherits(model,'lmerMod')){
            upper_predicted = stats::predict(model, newdata = upper_new_data)
            lower_predicted = stats::predict(model, newdata = lower_new_data)
            
            plot_df = rbind(plot_df, data.frame(cat_var_name = level,
                                                num_var_name = 'high',
                                                mean = upper_predicted,
                                                se = NA_real_))
            
            plot_df = rbind(plot_df, data.frame(cat_var_name = level,
                                                num_var_name = 'low',
                                                mean = lower_predicted,
                                                se = NA_real_))
          }
        }
       
        names(plot_df)[1] = 'predict_var1'
        names(plot_df)[2] = 'predict_var2'
        
        label_name = label_name(
          graph_label_name = graph_label_name,
          response_var_name = response_var_name,
          predict_var1_name = cat_var_name,
          predict_var2_name = num_var_name,
          predict_var3_name = NULL
        )
      } else if(numeric_var_count == 0){
        mean = data %>%
          dplyr::group_by(dplyr::across(!!!predict_vars)) %>%
          dplyr::summarise(dplyr::across(!!response_var, ~ mean(., na.rm = TRUE))) %>%
          dplyr::rename(mean = !!response_var)
        
        se = data %>%
          dplyr::group_by(dplyr::across(!!!predict_vars)) %>%
          dplyr::summarise(dplyr::across(
            !!response_var,
            ~ stats::sd(., na.rm = TRUE) / sqrt(dplyr::n())
          )) %>%
          dplyr::rename(se = !!response_var)
        
        plot_df =  mean %>%
          dplyr::full_join(se) %>%
          dplyr::rename(predict_var1 = !!dplyr::enquo(predict_var1)) %>%
          dplyr::rename(predict_var2 = !!dplyr::enquo(predict_var2))
        
        label_name = label_name(
          graph_label_name = graph_label_name,
          response_var_name = response_var_name,
          predict_var1_name = predict_var1,
          predict_var2_name = predict_var2,
          predict_var3_name = NULL
        )
      }
      
      main_plot = plot_df %>%
        ggplot2::ggplot(data = .,
                        ggplot2::aes(
                          x = .data$predict_var1,
                          y = .data$mean,
                          fill = .data$predict_var2
                        )) +
        ggplot2::geom_bar(
          stat = 'identity',
          width = 0.5,
          color = 'black',
          position = ggplot2::position_dodge(0.9)) +
        ggplot2::labs(y = label_name[1],
                      x = label_name[2],
                      fill = label_name[3])
      
      if (all(!is.na(plot_df$se))) {
        main_plot = main_plot +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                                 position = ggplot2::position_dodge(0.9),
                                 width = 0.1)
        
      }
      ##################################### Three-way Interaction #####################################
    } else if (length(get_interaction_term(model)) == 3) {
      
      data_type = data %>%
        dplyr::summarise(dplyr::across(!!!predict_vars,class)) %>%
        tidyr::pivot_longer(dplyr::everything(),names_to = 'name',values_to = 'value')
      
      numeric_var_count = data_type %>%
        dplyr::filter(.data$value == 'numeric') %>%
        nrow()
      
      if (numeric_var_count == 2) {
        num_var_name = data_type %>% dplyr::filter(.data$value == 'numeric') %>% dplyr::select('name') %>% dplyr::pull()
        cat_var_name = data_type %>% dplyr::filter(.data$value == 'factor') %>% dplyr::select('name') %>% dplyr::pull()
        
        mean_df = get_predict_df(data = data)$mean_df
        upper_df = get_predict_df(data = data)$upper_df
        lower_df = get_predict_df(data = data)$lower_df
        plot_df = data.frame()
        for (level in levels((data[[cat_var_name]]))) {
          upper_upper_new_data = mean_df
          upper_upper_new_data[num_var_name[1]] = upper_df[num_var_name[1]]
          upper_upper_new_data[num_var_name[2]] = upper_df[num_var_name[2]]
          upper_upper_new_data[cat_var_name] = level
          
          upper_lower_new_data = mean_df
          upper_lower_new_data[num_var_name[1]] = upper_df[num_var_name[1]]
          upper_lower_new_data[num_var_name[2]] = lower_df[num_var_name[2]]
          upper_lower_new_data[cat_var_name] = level
          
          lower_upper_new_data = mean_df
          lower_upper_new_data[num_var_name[1]] = lower_df[num_var_name[1]]
          lower_upper_new_data[num_var_name[2]] = upper_df[num_var_name[2]]
          lower_upper_new_data[cat_var_name] = level
          
          lower_lower_new_data = mean_df
          lower_lower_new_data[num_var_name[1]] = lower_df[num_var_name[1]]
          lower_lower_new_data[num_var_name[2]] = lower_df[num_var_name[2]]
          lower_lower_new_data[cat_var_name] = level
          
          if (inherits(model,'lm')) {
            upper_upper_predicted = stats::predict(model, newdata = upper_upper_new_data, se.fit = TRUE)
            upper_lower_predicted = stats::predict(model, newdata = upper_lower_new_data, se.fit = TRUE)
            lower_upper_predicted = stats::predict(model, newdata = lower_upper_new_data, se.fit = TRUE)
            lower_lower_predicted = stats::predict(model, newdata = lower_lower_new_data, se.fit = TRUE)
            
            plot_df = rbind(plot_df,data.frame(cat_var_name = rep(level,4),
                                               numeric_var1 = c('high','high','low','low'),
                                               numeric_var2 = c('high','low','high','low'),
                                               mean = c(upper_upper_predicted$fit,upper_lower_predicted$fit,lower_upper_predicted$fit,lower_lower_predicted$fit),
                                               se = c(upper_upper_predicted$se.fit,upper_lower_predicted$se.fit,lower_upper_predicted$se.fit,lower_lower_predicted$se.fit)))
            
          } else if(inherits(model,'lmerMod')){
            upper_upper_predicted = stats::predict(model, newdata = upper_upper_new_data)
            upper_lower_predicted = stats::predict(model, newdata = upper_lower_new_data)
            lower_upper_predicted = stats::predict(model, newdata = lower_upper_new_data)
            lower_lower_predicted = stats::predict(model, newdata = lower_lower_new_data)
            
            plot_df = rbind(plot_df,data.frame(cat_var_name = rep(level,4),
                                               numeric_var1 = c('high','high','low','low'),
                                               numeric_var2 = c('high','low','high','low'),
                                               mean = c(upper_upper_predicted,upper_lower_predicted,lower_upper_predicted,lower_lower_predicted),
                                               se = rep(NA_real_,4)))
          }
        }
        names(plot_df)[1] = 'predict_var1'
        names(plot_df)[2] = 'predict_var2'
        names(plot_df)[3] = 'predict_var3'
        
        label_name = label_name(
          graph_label_name = graph_label_name,
          response_var_name = response_var_name,
          predict_var1_name = cat_var_name,
          predict_var2_name = num_var_name[1],
          predict_var3_name = num_var_name[2]
        )
        
      } else if(numeric_var_count == 1){
        num_var_name = data_type %>% dplyr::filter(.data$value == 'numeric') %>% dplyr::select('name') %>% dplyr::pull()
        cat_var_name = data_type %>% dplyr::filter(.data$value == 'factor') %>% dplyr::select('name') %>% dplyr::pull()
        
        mean_df = get_predict_df(data = data)$mean_df
        upper_df = get_predict_df(data = data)$upper_df
        lower_df = get_predict_df(data = data)$lower_df
        plot_df = data.frame()
        for (cat_var1_level in levels((data[[cat_var_name[1]]]))) {
          for (cat_var2_level in levels((data[[cat_var_name[2]]]))){
            upper_new_data = mean_df
            upper_new_data[num_var_name] = upper_df[num_var_name]
            upper_new_data[cat_var_name[1]] = cat_var1_level
            upper_new_data[cat_var_name[2]] = cat_var2_level
            
            lower_new_data = mean_df
            lower_new_data[num_var_name] = lower_df[num_var_name]
            lower_new_data[cat_var_name[1]] = cat_var1_level
            lower_new_data[cat_var_name[2]] = cat_var2_level
            if (inherits(model,'lm')) {
              upper_predicted = stats::predict(model, newdata = upper_new_data, se.fit = TRUE)
              lower_predicted = stats::predict(model, newdata = lower_new_data, se.fit = TRUE)
              
              plot_df = rbind(plot_df,data.frame(cat_var1_name = rep(cat_var1_level,2),
                                                 cat_var2_name = rep(cat_var2_level,2),
                                                 numeric_var1 = c('high','low'),
                                                 mean = c(upper_predicted$fit,lower_predicted$fit),
                                                 se = c(upper_predicted$se.fit,lower_predicted$se.fit)))
              
            } else if(inherits(model,'lmerMod')){
              upper_predicted = stats::predict(model, newdata = upper_new_data)
              lower_predicted = stats::predict(model, newdata = lower_new_data)
              
              plot_df = rbind(plot_df,data.frame(cat_var1_name = rep(cat_var1_level,2),
                                                 cat_var2_name = rep(cat_var2_level,2),
                                                 numeric_var1 = c('high','low'),
                                                 mean = c(upper_predicted,lower_predicted),
                                                 se = rep(NA_real_,2)))
            }
          }
        }
        names(plot_df)[1] = 'predict_var1'
        names(plot_df)[2] = 'predict_var2'
        names(plot_df)[3] = 'predict_var3'
        
        label_name = label_name(
          graph_label_name = graph_label_name,
          response_var_name = response_var_name,
          predict_var1_name = cat_var_name[1],
          predict_var2_name = cat_var_name[2],
          predict_var3_name = num_var_name
        )
        
      } else if(numeric_var_count == 0){
        mean = data %>%
          dplyr::group_by(dplyr::across(!!!predict_vars)) %>%
          dplyr::summarise(dplyr::across(!!response_var, ~ mean(., na.rm = TRUE))) %>%
          dplyr::rename(mean = !!response_var)
        
        se = data %>%
          dplyr::group_by(dplyr::across(!!!predict_vars)) %>%
          dplyr::summarise(dplyr::across(
            !!response_var,
            ~ stats::sd(., na.rm = TRUE) / sqrt(dplyr::n())
          )) %>%
          dplyr::rename(se = !!response_var)
        
        plot_df =  mean %>% dplyr::full_join(se) %>%
          dplyr::rename(predict_var1 = !!dplyr::enquo(predict_var1)) %>%
          dplyr::rename(predict_var2 = !!dplyr::enquo(predict_var2)) %>%
          dplyr::rename(predict_var3 = !!dplyr::enquo(predict_var3))
        
        label_name = label_name(
          graph_label_name = graph_label_name,
          response_var_name = response_var_name,
          predict_var1_name = predict_var1,
          predict_var2_name = predict_var2,
          predict_var3_name = predict_var3
        )
      }
      
      main_plot = plot_df %>%
        ggplot2::ggplot(data = .,
                        ggplot2::aes(
                          x = .data$predict_var1,
                          y = .data$mean,
                          fill = .data$predict_var2
                        )) +
        ggplot2::geom_bar(
          stat = 'identity',
          width = 0.5,
          color = 'black',
          position = ggplot2::position_dodge(0.9)
        ) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se),
                               position = ggplot2::position_dodge(0.9),
                               width = 0.1) +
        ggplot2::labs(y = label_name[1],
                      x = label_name[2],
                      fill = label_name[3]) +
        ggplot2::facet_wrap( ~ .data$predict_var3)
    }
  }
  
  ##################################### Touching up #####################################
  final_plot = main_plot +
    ggplot2::scale_fill_brewer() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black")
    )
  
  return(final_plot)
}
