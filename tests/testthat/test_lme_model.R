
lme_model(response_variable = 'JS_Individual',
          level_1_factors = Gender_Individual:Age_Individual, 
          level_2_factors = contains('Country'),
          two_way_interaction_factor = c('Age_Individual','Hofstede_IC_Country'),
          id = 'Country',
          data = EWCS_2015_shorten,
          quite = T)

expect_error(lme_model(response_variable = 'JS_Individual',
          level_1_factors = Gender_Individual:Age_Individual, 
          level_2_factors = contains('Country'),
          three_way_interaction_factor = c('Age_Individual','Hofstede_IC_Country'),
          id = 'Country',
          data = EWCS_2015_shorten,
          quite = T))

expect_error(lme_model(response_variable = c(JS_Individual,Education_Individual),
                       level_1_factors = Age_Individual:Gender_Individual, 
                       level_2_factors = Hofstede_IC_Country,
                       three_way_interaction_factor = c('Gender_Individual','Hofstede_IC_Country','Age_Individual'),
                       id = 'Country',
                       data = EWCS_2015_shorten,
                       use_package = 'lme4',
                       quite = T))

expect_error(lme_model(response_variable = JS_Individual,
                       level_1_factors = Age_Individual:Gender_Individual, 
                       level_2_factors = Hofstede_IC_Country,
                       three_way_interaction_factor = c('Gender_Individual','Hofstede_IC_Country','Age_Individual'),
                       id = c('Country',Education_Individual),
                       data = EWCS_2015_shorten,
                       use_package = 'lme4',
                       quite = T))
