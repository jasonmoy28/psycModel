model1 = model_summary_with_plot(response_variable = 'incidence',
                                 level_1_factors = 'size',
                                 level_2_factors = 'herd',
                                 id = 'period',
                                 data = lme4::cbpp,
                                 use_package = 'lmerTest', #use lmerTest
                                 model_performance = NULL,
                                 return_result = T)

testthat::expect(is.null(model1$plot),failure_message = 'plot is not NULL when no interaction is included')

testthat::expect_warning(
  model_summary_with_plot(response_variable = 'JS_Individual',
                          level_1_factors = 'SWB_Individual',
                          id = 'Country',
                          data = EWCS_2015_shorten,
                          use_package = 'lmerTest', #use lmerTest
                          model_performance = NULL,
                          return_result = F),
  regexp = 'optimizer')

