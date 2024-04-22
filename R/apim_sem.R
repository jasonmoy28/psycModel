#' APIM Actor-Partner Interdependence Model (SEM)
#' 
#' `r lifecycle::badge("stable")` \cr
#' 
#' Actor-partner interdependence model using SEM approach (with lavaan). Indistinguishable dyads only. Results should be the same as those from Kenny (2015a, 2015b). 
#' 
#' @param data data frame object 
#' @param predictor_a predictor variable name for actor
#' @param predictor_p predictor variable name for partner
#' @param outcome_a dependent variable name for actor
#' @param outcome_p dependent variable name for partner
#' @param med_a mediation variable name for actor
#' @param med_p mediation variable name for partner
#' @param mod_a moderation variable name for actor
#' @param mod_p moderation variable name for partner
#' @param bootstrap number of bootstrapping (e.g., 5000). Default is not using bootstrap
#' @param standardized standardized coefficient
#' @param return_result return `lavaan::parameterestimates()`. Default is `FALSE`
#' @param quite suppress printing output. Default is `FALSE`
#' @param mod_type options are "simple" (main effect), "med" (mediation), and "mod" (moderation)
#'
#' @return
#' data.frame from `lavaan::parameterestimates()`
#' @export
#' @references 
#' Kenny, D. A. (2015, October). An interactive tool for the estimation and testing mediation in the Actor-Partner Interdependence Model using structural equation modeling. Computer software. Available from https://davidakenny.shinyapps.io/APIMeM/.
#' Kenny, D. A. (2015, October). An interactive tool for the estimation and testing moderation in the Actor-Partner Interdependence Model using structural equation modeling. Computer software. Available from https://davidakenny.shinyapps.io/APIMoM/.
#' Stas, L, Kenny, D. A., Mayer, A., & Loeys, T. (2018). Giving Dyadic Data Analysis Away: A User-Friendly App for Actor-Partner Interdependence Models. Personal Relationships, 25 (1), 103-119. https://doi.org/10.1111/pere.12230.

#' @examples
#' APIM_sem(data = acitelli,
#'         predictor_a = 'Tension_A',
#'         predictor_p = 'Tension_P',
#'         outcome_a = 'Satisfaction_A',
#'         outcome_p = 'Satisfaction_P',
#'         mod_type = 'simple')
#'         
APIM_sem <- function(data,
                     mod_type,
                     predictor_a,
                     predictor_p,
                     outcome_a,
                     outcome_p,
                     med_a = NULL,
                     med_p = NULL,
                     mod_a = NULL,
                     mod_p = NULL,
                     bootstrap = NULL,
                     standardized = FALSE,
                     return_result = FALSE,
                     quite = FALSE) {
  # ------------------------------------------------------ Mediation APIM ------------------------------------------------------ #
  if (!is.null(bootstrap)) {
    se = 'boot'
  } else{
    se = 'standard'
  }
  if (mod_type == 'med') {
    mediation_effect_mod = 
      '
      m_a  ~ aa*x_a
      m_p  ~ aa*x_p
      m_a  ~ pa*x_p
      m_p  ~ pa*x_a
      
      y_a  ~ ab*m_a
      y_p  ~ ab*m_p
      y_a  ~ pb*m_p
      y_p  ~ pb*m_a
      
      y_a  ~ ac*x_a
      y_p  ~ ac*x_p
      y_a  ~ pc*x_p
      y_p  ~ pc*x_a
      
      x_a ~ m1*1
      x_p ~ m1*1
      y_a ~ m2*1
      y_p ~ m2*1
      m_a ~ m3*1
      m_p ~ m3*1
      
      x_a ~~ v1*x_a
      x_p ~~ v1*x_p
      y_a ~~ v2*y_a
      y_p ~~ v2*y_p
      m_a ~~ v3*m_a
      m_p ~~ v3*m_p
      
      x_a ~~ x_p
      y_a ~~ y_p
      m_a ~~ m_p

      ka := pa/aa
      kb := pb/ab
      AA_ie := aa*ab
      AP_ie := aa*pb
      PA_ie := pa*ab
      PP_ie := pa*pb
      total_ie_a := aa*ab + pa*pb
      total_ie_p := aa*pb + pa*ab
      total_a := aa*ab + pa*pb + ac
      total_p := aa*pb + pa*ab + pc
'
    data = data %>% 
      dplyr::rename('x_a' := !!predictor_a) %>% 
      dplyr::rename('x_p' := !!predictor_p) %>%
      dplyr::rename('y_a' := !!outcome_a) %>% 
      dplyr::rename('y_p' := !!outcome_p) %>% 
      dplyr::rename('m_a' := !!med_a) %>% 
      dplyr::rename('m_p' := !!med_p) 
    
    
    mediation_effect_sem <- lavaan::sem(mediation_effect_mod,fixed.x=FALSE, data = data,missing="fiml",bootstrap = bootstrap,se = se)
    if (quite == FALSE) {
      print(lavaan::summary(mediation_effect_sem,fit.measures = T,standardized = standardized)) 
    }
    if (return_result == T) {
      return(lavaan::parameterestimates(mediation_effect_sem)) 
    }
    
    
    # ------------------------------------------------------ Moderation APIM ------------------------------------------------------ #
  } else if (mod_type == 'mod'){
    moderation_effect_mod = 
      '
     y_a ~ ab*m_a
     y_p ~ ab*m_p
     y_a ~ pb*m_p
     y_p ~ pb*m_a
     y_a ~ aa*x_a
     y_p ~ aa*x_p
     y_a ~ pa*x_p
     y_p ~ pa*x_a     
     y_a ~ iAA*xama
     y_p ~ iPP*xama    
     y_a ~ iAP*xamp
     y_p ~ iPA*xamp     
     y_a ~ iPA*xpma
     y_p ~ iAP*xpma     
     y_a ~ iPP*xpmp
     y_p ~ iAA*xpmp     
     
     x_a ~~ x_p
     y_a ~~ y_p
     m_a ~~ m_p
     xama ~~ xpmp
     xamp ~~ xpma
     
     x_a ~ m1*1
     x_p ~ m1*1
     
     y_a ~ m2*1
     y_p ~ m2*1
     
     m_a ~ m3*1
     m_p ~ m3*1
     
     xama ~ m4*1
     xpmp ~ m4*1
     
     xamp ~ m5*1
     xpma ~ m5*1
     
     x_a ~~ v1*x_a
     x_p ~~ v1*x_p
     
     y_a ~~ v2*y_a
     y_p ~~ v2*y_p
     
     m_a ~~ v3*m_a
     m_p ~~ v3*m_p
     
     xama ~~ v4*xama
     xpmp ~~ v4*xpmp
     
     xamp ~~ v5*xamp
     xpma ~~ v5*xpma
     
     x_a ~~ c1*xama
     x_p ~~ c1*xpmp
     
     x_p ~~ c2*xama
     x_a ~~ c2*xpmp
     
     x_a ~~ c3*xamp
     x_p ~~ c3*xpma
     
     x_p ~~ c4*xamp
     x_a ~~ c4*xpma
     
     m_a ~~ c5*xama
     m_p ~~ c5*xpmp
     
     m_p ~~ c6*xama
     m_a ~~ c6*xpmp
     
     m_a ~~ c7*xamp
     m_p ~~ c7*xpma
     
     m_p ~~ c8*xamp
     m_a ~~ c8*xpma
     
     xama ~~ c9*xamp
     xpma ~~ c9*xpmp
     
     xama ~~ c10*xpma
     xamp ~~ c10*xpmp
     
     m_a ~~ c11*x_a
     m_p ~~ c11*x_p
     
     m_p ~~ c12*x_a
     m_a ~~ c12*x_p
     
     kx:= pa/aa
     km:= pb/ab
   '
    predictor_a = dplyr::enquo(predictor_a)
    predictor_p = dplyr::enquo(predictor_p)
    outcome_a = dplyr::enquo(outcome_a)
    outcome_p = dplyr::enquo(outcome_p)
    mod_a = dplyr::enquo(mod_a)
    mod_p = dplyr::enquo(mod_p)
    
    predictor_a_c = data %>% dplyr::select(dplyr::all_of(!!predictor_a)) %>% names(.) %>% as.character()
    predictor_p_c = data %>% dplyr::select(dplyr::all_of(!!predictor_p)) %>% names(.) %>% as.character()
    outcome_a_c = data %>% dplyr::select(dplyr::all_of(!!outcome_a)) %>% names(.) %>% as.character()
    outcome_p_c = data %>% dplyr::select(dplyr::all_of(!!outcome_p)) %>% names(.) %>% as.character()
    mod_a_c = data %>% dplyr::select(dplyr::all_of(!!mod_a)) %>% names(.) %>% as.character()
    mod_p_c = data %>% dplyr::select(dplyr::all_of(!!mod_p)) %>% names(.) %>% as.character()
    
    data = data %>% 
      dplyr::rename('x_a' := !!predictor_a) %>% 
      dplyr::rename('x_p' := !!predictor_p) %>%
      dplyr::rename('y_a' := !!outcome_a) %>% 
      dplyr::rename('y_p' := !!outcome_p) %>% 
      dplyr::rename('m_a' := !!mod_a) %>% 
      dplyr::rename('m_p' := !!mod_p) %>% 
      dplyr::mutate(dplyr::across(c('x_a','x_p','m_a','m_p'), function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
      dplyr::rename_with(.fn = ~ stringr::str_replace(.,'_c',''),dplyr::ends_with('_c')) %>%
      dplyr::mutate(xama = .data$x_a*.data$m_a) %>%
      dplyr::mutate(xpmp = .data$x_p*.data$m_p) %>%
      dplyr::mutate(xamp = .data$x_a*.data$m_p) %>%
      dplyr::mutate(xpma = .data$x_p*.data$m_a)
    
    moderation_effect_sem <- lavaan::sem(moderation_effect_mod,fixed.x=FALSE, data = data,missing="fiml",bootstrap = bootstrap,se = se)
    if (quite == FALSE) {
      print(lavaan::summary(moderation_effect_sem,fit.measures = T,standardized = standardized)) 
    }
    if (return_result == T) {
      return(lavaan::parameterestimates(moderation_effect_sem)) 
    }
    
    
    
    # ------------------------------------------------------ Simple APIM ------------------------------------------------------ #
    
  } else if(mod_type == 'simple'){
    simple_effect_mod = 
      '
        y_a ~ a*x_a 
        y_a ~ p*x_p
        y_p ~ a*x_p
        y_p ~ p*x_a
        
        
        x_a ~ mx*1
        x_p ~ mx*1
        
        y_a ~ my*1
        y_p ~ my*1
        
        x_a ~~ vx*x_a
        x_p ~~ vx*x_p
        
        y_a ~~ ve*y_a
        y_p ~~ ve*y_p
        
        x_a ~~ cx*x_p
        y_a ~~ cy*y_p
        
        k := p/a
        sum := (p + a)/2
        cont := a - p
'
    
    predictor_a = dplyr::enquo(predictor_a)
    predictor_p = dplyr::enquo(predictor_p)
    outcome_a = dplyr::enquo(outcome_a)
    outcome_p = dplyr::enquo(outcome_p)
    
    data = data %>% 
      dplyr::rename('x_a' := !!predictor_a) %>% 
      dplyr::rename('x_p' := !!predictor_p) %>%
      dplyr::rename('y_a' := !!outcome_a) %>% 
      dplyr::rename('y_p' := !!outcome_p) 
    
    simple_effect_sem = lavaan::sem(simple_effect_mod, fixed.x = FALSE, data=data, missing = 'fiml',se = se,bootstrap= bootstrap)
    if (quite == FALSE) {
      print(lavaan::summary(simple_effect_sem,fit.measures = T,standardized = standardized)) 
    }
    if (return_result == T) {
      return(lavaan::parameterestimates(simple_effect_sem)) 
    }
  }
}

