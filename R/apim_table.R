#' APIM Actor-Partner Interdependence Model Table with Multiple Moderators
#'
#' Actor-partner interdependence model that test multiple moderators simultaneously.
#' 
#' @param data data frame object 
#' @param predictor_a predictor variable name for actor
#' @param predictor_p predictor variable name for partner
#' @param outcome_a dependent variable name for actor
#' @param outcome_p dependent variable name for partner
#' @param mod_a moderation variable name for actor. Support `dplyr::select()` syntax.
#' @param mod_p moderation variable name for partner. Support `dplyr::select()` syntax.
#' @param mod_type only 'mod' is supported for now
#' @param return_result return `lavaan::parameterestimates()`. Default is `FALSE`
#'
#' @return
#' data.frame of the APIM table
#' 
#' @export
#' @examples
#' APIM_table(data = acitelli,
#'            predictor_a = 'Tension_A',
#'            predictor_p = 'Tension_P',
#'            outcome_a = 'Satisfaction_A',
#'            outcome_p = 'Satisfaction_P',
#'            mod_a = c('SelfPos_A','OtherPos_A','SimHob_A'),
#'            mod_p = c('SelfPos_P','OtherPos_P','SimHob_P'))
#' 

APIM_table = function(
    data,
    predictor_a,
    predictor_p,
    outcome_a,
    outcome_p,
    mod_a = NULL,
    mod_p = NULL,
    mod_type = 'mod',
    return_result = FALSE
){
  if (mod_type == 'mod') {
    mod_a = dplyr::enquo(mod_a)
    mod_p = dplyr::enquo(mod_p)
    
    mod_a_ch = data %>% dplyr::select(dplyr::all_of(!!mod_a)) %>% names(.)
    mod_p_ch = data %>% dplyr::select(dplyr::all_of(!!mod_p)) %>% names(.)
    
    APIM_return = tibble::tibble()
    for (i in 1:length(mod_a_ch)) {
      APIM_result = APIM_sem(
        data = data,
        predictor_a = predictor_a,
        predictor_p = predictor_p,
        outcome_a = outcome_a,
        outcome_p = outcome_p,
        mod_a = mod_a_ch[i],
        mod_p = mod_p_ch[i],
        mod_type = mod_type,
        quite = T,
        return_result = T
      )
      APIM_result_processed = tibble::tibble(APIM_result) %>% 
        dplyr::filter(.data$label %in% c('aa','pa','ab','pb','iAA','iPP','iPA','iAP')) %>% 
        dplyr::mutate(dplyr::across(c('est','pvalue'),~ round(x = .,digits = 4))) %>% 
        dplyr::distinct(.data$label,.data$est,.data$pvalue) %>% 
        dplyr::rename(Coefficient = 'est') %>% 
        dplyr::rename(p = 'pvalue')
      

      APIM_result_final = 
        APIM_result_processed %>% 
        dplyr::select(-c('Coefficient','p')) %>% 
        dplyr::bind_cols(coefficent_to_p(APIM_result_processed,show_p = F)) %>% 
        tidyr::pivot_wider(names_from = .data$label,values_from = .data$Coefficient) %>% 
        dplyr::select(dplyr::all_of(c('iAA','iPP','iAP','iPA','aa','ab','pa','pb'))) %>% 
        tibble::add_column(Outcome = outcome_a,.before = 1) %>% 
        tibble::add_column(Predictor = predictor_a,.before = 1) %>% 
        tibble::add_column(Moderator = mod_a_ch[i],.before = 1) 
      
      APIM_return = dplyr::bind_rows(APIM_return,APIM_result_final)
    }
    
    if (return_result == T) {
      return(APIM_return)
    }
    
    print_table(APIM_return)
    super_print('
    Lab
    iAA: Y_a ~ X_a * M_a and Y_p ~ X_p * M_p 
    iPP: Y_a ~ X_p * M_p and Y_p ~ X_a * M_a                                               
    iAP: Y_a ~ X_a * M_p and Y_p ~ X_p * M_a
    iPA: Y_a ~ X_p * M_a and Y_p ~ X_a * M_p
     aa: Y_a ~ X_a and Y_p ~ X_p 
     ab: Y_a ~ M_a and Y_p ~ M_p 
     pa: Y_a ~ X_p and Y_p ~ X_a
     pb: Y_a ~ M_p and Y_p ~ M_a ')
    
  }
  
  
}
