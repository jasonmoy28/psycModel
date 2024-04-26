
testthat::test_that('lm_model_table',{test = data.frame(y1 = rnorm(1000,2,3),
                                                          y2 = rnorm(1000,10,2),
                                                          y3 = rnorm(1000,1,4),
                                                          x1 = rnorm(1000,100,10),
                                                          x2 = rnorm(1000,10,1),
                                                          x3 = rnorm(1000,6,2),
                                                          m1 = rnorm(1000,3,1),
                                                          m2 = rnorm(1000,2,0.5),
                                                          m3 = rnorm(1000,9,0.1),
                                                          c1 = rnorm(1000,5,0.4),
                                                          c2 = rnorm(1000,2,0.2),
                                                          c3 = rnorm(1000,7,0.9)
)

testthat::expect_no_error(lm_model_table(data = test, 
                                         response_variable = y1,
                                         predictor_variable =x1,
                                         two_way_interaction_variable = c(m1,m2,m3),
                                         control_variable = c(c1,c2,c3),
                                         return_result = T,
                                         verbose = F,
                                         show_p = T))

testthat::expect_no_error(lm_model_table(data = test, 
                                         response_variable = y1,
                                         predictor_variable = c(x1,x2,x3),
                                         control_variable = c(c1,c2,c3),
                                         other_parameters = c('x3*m3'),
                                         return_result = T,
                                         verbose = F,
                                         show_p = T))

testthat::expect_no_error(lm_model_table(data = test, 
                                         response_variable = c(y1,y2,y3),
                                         predictor_variable = x1,
                                         control_variable = c(c1,c2,c3),
                                         other_parameters = c('x2*m1','x1*m1'),
                                         return_result = T,
                                         verbose = F,
                                         show_p = T))





  
})


