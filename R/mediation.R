#' Mediation analysis 
#' A Monte Carlo simulation method to assess mediation based on Selig & Preacher (2008).
#'  
#' @param model_med a fitted model object for mediator.
#' @param model_y a fitted model object for outcome
#' @param model_med2 a fitted model object for the second mediator for serial mediation
#' @param x a character string indicating the name of the independent variable used in the models.
#' @param med a character string indicating the name of the mediator used in the models.
#' @param med2 a character string indicating the name of the second mediator used in the models (for serial mediations)
#' @param mod a character string indicating the name of the moderator used in the models.
#' @param mod_stage a character string specifying the stage at which the moderating effect occurs. For instance, in a first-stage moderated mediation, where the moderator influences the effect of X on the mediator (Med), set this to "model_med". In a second-stage moderated mediation, where the moderator affects the relationship between the mediator (Med) and the outcome variable (Y), set this to "model_y".#' 
#' @param mod_level The default is -1 SD and +1 SD for a continuous variable, and it is the two levels for a binary variable. 
#' @param conf 	level of the returned two-sided confidence intervals. Default is to return the 2.5 and 97.5 percentiles of the simulated quantities (i.e., 95%).
#' @param rep number of Monte Carlo draws
#' @param digits number of digits to round to
#' @param verbose deafult is `TRUE`.
#'
#' @return
#' Nothing to return. Print the indirect effect. 
#' @export
#' @references Selig, J. P., & Preacher, K. J. (2008, June). Monte Carlo method for assessing mediation: An interactive tool for creating confidence intervals for indirect effects. http://quantpsy.org/.
#' @examples
#' new_dat = iris %>% 
#'   dplyr::rename(x = Petal.Length) %>% 
#'   dplyr::rename(m = Sepal.Length) %>% 
#'   dplyr::rename(moderator = Sepal.Width) %>% 
#'   dplyr::rename(y = Petal.Width)
#' 
#' model_1 = lm(data = new_dat, m ~ x)
#' model_2 = lm(data = new_dat, y ~ x*moderator + m)
#' 
#' 
#' mediation(model_med = model_1,
#'           model_y = model_2,
#'           rep = 20000,
#'           x = 'x', 
#'           med = 'm',
#'           mod = 'moderator',
#'           mod_stage = 'model_y',
#'           digits = 3)
#'           
mediation <- function(model_med, 
                      model_y,
                      model_med2 = NULL, 
                      x, 
                      med, 
                      med2 = NULL,
                      mod = NULL,
                      mod_stage = NULL,
                      mod_level = NULL,
                      conf = 95, 
                      rep = 20000,
                      verbose = TRUE,
                      digits = 3) {
  if (is.null(model_med2)) {
    if (is.null(mod)) {
      model_med_num = nrow(insight::get_parameters(model_med))
      model_y_num = nrow(insight::get_parameters(model_y))
      matrix_length = model_med_num + model_y_num
      acov = matrix(rep(0,matrix_length*matrix_length),nrow = matrix_length)
      acov[1:model_med_num,1:model_med_num] <- as.matrix(stats::vcov(model_med))
      acov[(model_med_num+1):matrix_length,(model_med_num+1):matrix_length] <- as.matrix(stats::vcov(model_y))
      
      coef_model_med = insight::get_parameters(model_med) %>% dplyr::mutate('Parameter' = stringr::str_c("model_med_",.data$Parameter))
      coef_model_y = insight::get_parameters(model_y) %>% dplyr::mutate('Parameter' = stringr::str_c("model_y_",.data$Parameter))
      pest = rbind(coef_model_med,coef_model_y) %>% dplyr::pull('Estimate')
      names(pest) = rbind(coef_model_med,coef_model_y) %>% dplyr::pull('Parameter')
      a_path = which(names(pest) == paste0('model_med_',x))
      b_path = which(names(pest) == paste0('model_y_',med))
      
      mcmc <- MASS::mvrnorm(rep,pest,acov,empirical=FALSE)
      indirect <- mcmc[,a_path]*mcmc[,b_path]
      low=(1-conf/100)/2
      upp=((1-conf/100)/2)+(conf/100)
      LL=stats::quantile(indirect,low)
      UL=stats::quantile(indirect,upp)
      LL4=round(LL,digits=digits)
      UL4=round(UL,digits=digits)
      mediation_summary = tibble::tibble(indirect_effect = mean(indirect), CI = glue::glue('[{LL4},{UL4}]'))
    } else{
      if (is.null(mod_stage)) {stop('Please enter mod_stage')}
      model_med_num = nrow(insight::get_parameters(model_med))
      model_y_num = nrow(insight::get_parameters(model_y))
      matrix_length = model_med_num + model_y_num
      acov = matrix(rep(0,matrix_length*matrix_length),nrow = matrix_length)
      acov[1:model_med_num,1:model_med_num] <- as.matrix(stats::vcov(model_med))
      acov[(model_med_num+1):matrix_length,(model_med_num+1):matrix_length] <- as.matrix(stats::vcov(model_y))
      
      coef_model_med = insight::get_parameters(model_med) %>% dplyr::mutate('Parameter' = stringr::str_c("model_med_",.data$Parameter))
      coef_model_y = insight::get_parameters(model_y) %>% dplyr::mutate('Parameter' = stringr::str_c("model_y_",.data$Parameter))
      pest = rbind(coef_model_med,coef_model_y) %>% dplyr::pull('Estimate')
      names(pest) = rbind(coef_model_med,coef_model_y) %>% dplyr::pull('Parameter')
      a_path = which(names(pest) == paste0('model_med_',x))
      b_path = which(names(pest) == paste0('model_y_',med))
      
      mod_stage = paste0(mod_stage,'_')
      mod_path = which(names(pest) == paste0(mod_stage,mod))
      interaction_path = suppressWarnings(which(stringr::str_detect(names(pest),stringr::regex(glue::glue("{mod_stage}.*:.*")))))
      if (is.null(mod_level)) {
        if (mod_stage == 'model_y_') {
          mod_data = insight::get_data(model_y)
        } else if(mod_stage == 'model_med_'){
          mod_data = insight::get_data(model_med)
        }
        
        mod_level = interaction_value(mod_data,!!enquo(mod))
      }
      mcmc <- MASS::mvrnorm(rep,pest,acov,empirical=FALSE)
      if (mod_stage == 'model_y_') {
        indirect_low <- mcmc[,a_path]*(mcmc[,b_path] + mcmc[,interaction_path]*mod_level[1])
        indirect_high <- mcmc[,a_path]*(mcmc[,b_path] + mcmc[,interaction_path]*mod_level[2])
        index_mod_med =  mcmc[,a_path]*mcmc[,interaction_path]
      } else if (mod_stage == 'model_med_') {
        indirect_low <- (mcmc[,a_path] + mcmc[,interaction_path]*mod_level[1])*mcmc[,b_path]
        indirect_high <- (mcmc[,a_path] + mcmc[,interaction_path]*mod_level[2])*mcmc[,b_path]
        index_mod_med =  mcmc[,b_path]*mcmc[,interaction_path]
      }
      low=(1-conf/100)/2
      upp=((1-conf/100)/2)+(conf/100)
      LL_low = round(stats::quantile(indirect_low,low),digits)
      UL_low = round(stats::quantile(indirect_low,upp),digits)
      LL_high = round(stats::quantile(indirect_high,low),digits)
      UL_high = round(stats::quantile(indirect_high,upp),digits)
      index_mod_med_low = round(stats::quantile(index_mod_med,low),digits)
      index_mod_med_high = round(stats::quantile(index_mod_med,upp),digits)
      mediation_summary = data.frame(indirect_effect_low = round(mean(indirect_low),digits),
                                     indirect_effect_high = round(mean(indirect_high),digits),
                                     CI_low = glue::glue('[{LL_low},{UL_low}]'),
                                     CI_high = glue::glue('[{LL_high},{UL_high}]'),
                                     index_mod_med = round(mean(index_mod_med),digits),
                                     CI_index_mod_med = glue::glue('[{index_mod_med_low},{index_mod_med_high}]'))
    }
  } else { # serial mediation
    if(is.null(mod)) {
      model_med_num = nrow(insight::get_parameters(model_med))
      model_med2_num = nrow(insight::get_parameters(model_med2))
      model_y_num = nrow(insight::get_parameters(model_y))
      matrix_length = model_med_num + model_med2_num + model_y_num
      acov = matrix(rep(0,matrix_length*matrix_length),nrow = matrix_length)
      acov[1:model_med_num,1:model_med_num] <- as.matrix(stats::vcov(model_med))
      acov[(model_med_num+1):(model_med_num + model_med2_num),(model_med_num+1):(model_med_num + model_med2_num)] <- as.matrix(stats::vcov(model_med2))
      acov[(model_med_num + model_med2_num+1):matrix_length,(model_med_num + model_med2_num+1):matrix_length] <- as.matrix(stats::vcov(model_y))
      
      coef_model_med = insight::get_parameters(model_med) %>% dplyr::mutate('Parameter' = stringr::str_c("model_med_",.data$Parameter))
      coef_model_med2 = insight::get_parameters(model_med2) %>% dplyr::mutate('Parameter' = stringr::str_c("model_med2_",.data$Parameter))
      coef_model_y = insight::get_parameters(model_y) %>% dplyr::mutate('Parameter' = stringr::str_c("model_y_",.data$Parameter))
      pest = rbind(coef_model_med,coef_model_med2,coef_model_y) %>% dplyr::pull('Estimate')
      names(pest) = rbind(coef_model_med,coef_model_med2,coef_model_y) %>% dplyr::pull('Parameter')
      a_path = which(names(pest) == paste0('model_med_',x))
      b_path = which(names(pest) == paste0('model_med2_',med))
      c_path = which(names(pest) == paste0('model_y_',med2))
      
      mcmc <- MASS::mvrnorm(rep,pest,acov,empirical=FALSE)
      indirect <- mcmc[,a_path]*mcmc[,b_path]*mcmc[,c_path]
      low=(1-conf/100)/2
      upp=((1-conf/100)/2)+(conf/100)
      LL=stats::quantile(indirect,low)
      UL=stats::quantile(indirect,upp)
      LL4=round(LL,digits=digits)
      UL4=round(UL,digits=digits)
      mediation_summary = tibble::tibble(indirect_effect = mean(indirect), CI = glue::glue('[{LL4},{UL4}]'))
      
    } else{
      if (is.null(mod_stage)) {stop('Please enter mod_stage')}
      
      model_med_num = nrow(insight::get_parameters(model_med))
      model_med2_num = nrow(insight::get_parameters(model_med2))
      model_y_num = nrow(insight::get_parameters(model_y))
      matrix_length = model_med_num + model_med2_num + model_y_num
      acov = matrix(rep(0,matrix_length*matrix_length),nrow = matrix_length)
      acov[1:model_med_num,1:model_med_num] <- as.matrix(stats::vcov(model_med))
      acov[(model_med_num+1):(model_med_num + model_med2_num),(model_med_num+1):(model_med_num + model_med2_num)] <- as.matrix(stats::vcov(model_med2))
      acov[(model_med_num + model_med2_num+1):matrix_length,(model_med_num + model_med2_num+1):matrix_length] <- as.matrix(stats::vcov(model_y))
      
      coef_model_med = insight::get_parameters(model_med) %>% dplyr::mutate('Parameter' = stringr::str_c("model_med_",.data$Parameter))
      coef_model_med2 = insight::get_parameters(model_med2) %>% dplyr::mutate('Parameter' = stringr::str_c("model_med2_",.data$Parameter))
      coef_model_y = insight::get_parameters(model_y) %>% dplyr::mutate('Parameter' = stringr::str_c("model_y_",.data$Parameter))
      pest = rbind(coef_model_med,coef_model_med2,coef_model_y) %>% dplyr::pull('Estimate')
      names(pest) = rbind(coef_model_med,coef_model_med2,coef_model_y) %>% dplyr::pull('Parameter')
      a_path = which(names(pest) == paste0('model_med_',x))
      b_path = which(names(pest) == paste0('model_med2_',med))
      c_path = which(names(pest) == paste0('model_y_',med2))
      
      mod_stage = paste0(mod_stage,'_')
      mod_path = which(names(pest) == paste0(mod_stage,mod))
      interaction_path = suppressWarnings(which(stringr::str_detect(names(pest),(glue::glue("{mod_stage}.*:.*")))))
      if (is.null(mod_level)) {
        if (mod_stage == 'model_y_') {
          mod_data = insight::get_data(model_y)
        } else if(mod_stage == 'model_med_'){
          mod_data = insight::get_data(model_med)
        } else if(mod_stage == 'model_med2_'){
          mod_data = insight::get_data(model_med2)
        }
        mod_level = interaction_value(mod_data,!!enquo(mod))
      }
      
      mcmc <- MASS::mvrnorm(rep,pest,acov,empirical=FALSE)
      if (mod_stage == 'model_y_') {
        indirect_low <- mcmc[,a_path]*mcmc[,b_path]*(mcmc[,c_path] + mcmc[,interaction_path]*mod_level[1])
        indirect_high <- mcmc[,a_path]*mcmc[,b_path]*(mcmc[,c_path] + mcmc[,interaction_path]*mod_level[2])
        index_mod_med = mcmc[,a_path]*mcmc[,b_path]*mcmc[,interaction_path]
      } else if (mod_stage == 'model_med2_') {
        indirect_low <- mcmc[,a_path]*mcmc[,c_path]*(mcmc[,b_path] + mcmc[,interaction_path]*mod_level[1])
        indirect_high <- mcmc[,a_path]*mcmc[,c_path]*(mcmc[,b_path] + mcmc[,interaction_path]*mod_level[2])
        index_mod_med = mcmc[,a_path]*mcmc[,c_path]*mcmc[,interaction_path]
      } else if (mod_stage == 'model_med_') {
        indirect_low <- mcmc[,b_path]*mcmc[,c_path]*(mcmc[,a_path] + mcmc[,interaction_path]*mod_level[1])
        indirect_low <- mcmc[,b_path]*mcmc[,c_path]*(mcmc[,a_path] + mcmc[,interaction_path]*mod_level[2])
        index_mod_med = mcmc[,b_path]*mcmc[,c_path]*mcmc[,interaction_path]
      } 
      
      low=(1-conf/100)/2
      upp=((1-conf/100)/2)+(conf/100)
      LL_low = round(stats::quantile(indirect_low,low),digits)
      UL_low = round(stats::quantile(indirect_low,upp),digits)
      LL_high = round(stats::quantile(indirect_high,low),digits)
      UL_high = round(stats::quantile(indirect_high,upp),digits)
      index_mod_med_low = round(stats::quantile(index_mod_med,low),digits)
      index_mod_med_high = round(stats::quantile(index_mod_med,upp),digits)
      mediation_summary = data.frame(indirect_effect_low = round(mean(indirect_low),digits),
                                     indirect_effect_high = round(mean(indirect_high),digits),
                                     CI_low = glue::glue('[{LL_low},{UL_low}]'),
                                     CI_high = glue::glue('[{LL_high},{UL_high}]'),
                                     index_mod_med = round(mean(index_mod_med),digits),
                                     CI_index_mod_med = glue::glue('[{index_mod_med_low},{index_mod_med_high}]'))
      
    }
  }
  if (verbose) {
    psycModel::super_print('underline|Model Summary')
    psycModel::super_print('Model Type = Mediation Analysis')
    psycModel::super_print('Monte Carlo simulation = {rep}')
    psycModel::super_print('CI = {conf}%')
    if (!is.null(mod)) {
      psycModel::super_print('Condition Indirect Effect = Low is {round(mod_level[1],digits)} and High is {round(mod_level[2],digits)}')
    }
    psycModel::print_table(mediation_summary,digits = digits)
  }
}

#process(data = new_dat,y = 'y', x = 'x',m = c('m2','m1'),model = 6)

