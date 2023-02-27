testthat::test_that("model_table: linear regression", {
  model_summary = model_table(data = iris,response_variable = 'Petal.Width',
              predictor_variable = c('Petal.Length','Sepal.Length'),
              return_result = T,
              quite = T)
  
  testthat::expect_equal(format_round(model_summary$Coefficient,3),c('0.416','0.753'))
})

testthat::test_that("model_table: linear regression full model", {
  model_summary = model_table(data = iris,response_variable = 'Petal.Width',
                              predictor_variable = c('Petal.Length','Sepal.Length'),
                              return_result = T,
                              full_model = T,
                              quite = T)
  
  testthat::expect_equal(model_summary$Petal.Length,c('-0.363 ***',' 0.416 ***'))
  testthat::expect_equal(model_summary$Sepal.Length,c('-3.200 ***',' 0.753 ***'))
})
