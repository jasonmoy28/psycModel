mediation <- function(model_med, 
                      model_y,
                      model_med2 = NULL, 
                      x, 
                      med, 
                      mod = NULL,
                      mod_stage = NULL,
                      mod_level = NULL,
                      conf = 95, 
                      rep = 20000,
                      digits = 3) {
  
  if (is.null(model_med2)) {
    if (is.null(mod)) {
      model_med_num = nrow(insight::get_parameters(model_med))
      model_y_num = nrow(insight::get_parameters(model_y))
      matrix_length = model_med_num + model_y_num
      acov = matrix(rep(0,matrix_length*matrix_length),nrow = matrix_length)
      acov[1:model_med_num,1:model_med_num] <- as.matrix(vcov(model_med))
      acov[(model_med_num+1):matrix_length,(model_med_num+1):matrix_length] <- as.matrix(vcov(model_y))
      
      coef_model_med = insight::get_parameters(model_med) %>% mutate(Parameter = stringr::str_c("model_med_",Parameter))
      coef_model_y = insight::get_parameters(model_y) %>% mutate(Parameter = stringr::str_c("model_y_",Parameter))
      pest = rbind(coef_model_med,coef_model_y) %>% pull(Estimate)
      names(pest) = rbind(coef_model_med,coef_model_y) %>% pull(Parameter)
      a_path = which(names(pest) == paste0('model_med_',x))
      b_path = which(names(pest) == paste0('model_y_',med))
      
      mcmc <- MASS::mvrnorm(rep,pest,acov,empirical=FALSE)
      ab <- mcmc[,a_path]*mcmc[,b_path]
      low=(1-conf/100)/2
      upp=((1-conf/100)/2)+(conf/100)
      LL=quantile(ab,low)
      UL=quantile(ab,upp)
      LL4=round(LL,digits=digits)
      UL4=round(UL,digits=digits)
      mediation_summary = tibble::tibble(indirect_effect = mean(ab), CI = glue::glue('[{LL4},{UL4}]'))
      psycModel::print_table(mediation_summary)
    } else{
      if (is.null(mod_stage)) {stop('Please enter mod_stage')}
      model_med_num = nrow(insight::get_parameters(model_med))
      model_y_num = nrow(insight::get_parameters(model_y))
      matrix_length = model_med_num + model_y_num
      acov = matrix(rep(0,matrix_length*matrix_length),nrow = matrix_length)
      acov[1:model_med_num,1:model_med_num] <- as.matrix(vcov(model_med))
      acov[(model_med_num+1):matrix_length,(model_med_num+1):matrix_length] <- as.matrix(vcov(model_y))
      
      coef_model_med = insight::get_parameters(model_med) %>% mutate(Parameter = stringr::str_c("model_med_",Parameter))
      coef_model_y = insight::get_parameters(model_y) %>% mutate(Parameter = stringr::str_c("model_y_",Parameter))
      pest = rbind(coef_model_med,coef_model_y) %>% pull(Estimate)
      names(pest) = rbind(coef_model_med,coef_model_y) %>% pull(Parameter)
      a_path = which(names(pest) == paste0('model_med_',x))
      b_path = which(names(pest) == paste0('model_y_',med))
      
      mod_stage = paste0(mod_stage,'_')
      mod_path = which(names(pest) == paste0(mod_stage,mod))
      interaction_path = suppressWarnings(which(stringr::str_detect(names(pest),regex(glue::glue("{mod_stage}.*:.*")))))
      print(interaction_path)
      if (is.null(mod_level)) {
        if (mod_stage == 'model_y_') {
          mod_data = insight::get_data(model_y)
        } else if(mod_stage == 'model_med_'){
          mod_data = insight::get_data(model_med)
        }
        mod_level = c(mean(mod_data[,mod],na.rm = T) - sd(mod_data[,mod],na.rm = T),
                      mean(mod_data[,mod],na.rm = T) + sd(mod_data[,mod],na.rm = T))
      }
      print(mod_level)
      mcmc <- MASS::mvrnorm(rep,pest,acov,empirical=FALSE)
      if (mod_stage == 'model_y_') {
        indirect_low <- mcmc[,a_path]*(mcmc[,b_path] + mcmc[,interaction_path]*mod_level[1])
        indirect_high <- mcmc[,a_path]*(mcmc[,b_path] + mcmc[,interaction_path]*mod_level[2])
      } else if (mod_stage == 'model_med_') {
        indirect_low <- (mcmc[,a_path] + mcmc[,interaction_path]*mod_level[1])*mcmc[,b_path]
        indirect_high <- (mcmc[,a_path] + mcmc[,interaction_path]*mod_level[2])*mcmc[,b_path]
      }
      low=(1-conf/100)/2
      upp=((1-conf/100)/2)+(conf/100)
      LL_low = round(quantile(indirect_low,low),digits)
      UL_low = round(quantile(indirect_low,upp),digits)
      LL_high = round(quantile(indirect_high,low),digits)
      UL_high = round(quantile(indirect_high,upp),digits)
      mediation_summary = data.frame(indirect_effect_low = round(mean(indirect_low),digits),
                                     indirect_effect_high = round(mean(indirect_high),digits),
                                     CI_low = glue::glue('[{LL_low},{UL_low}]'),
                                     CI_high = glue::glue('[{LL_high},{UL_high}]'))
      psycModel::print_table(mediation_summary,digits = digits)
    }
  }
  
}

set.seed(1)
model_1 = lm(data = iris, Sepal.Length ~ Petal.Length*Sepal.Width)
model_2 = lm(data = iris, Petal.Width ~ Petal.Length + Sepal.Length)
mediation(model_med = model_1,
          model_y = model_2,
          rep = 10000,
          x = 'Petal.Length', 
          med = 'Sepal.Length',
          mod = 'Sepal.Width',
          mod_stage = 'model_med',
          digits = 5)

process(data = iris,y = 'Petal.Width', x = 'Petal.Length',m = 'Sepal.Length',w = 'Sepal.Width',model = 7)

