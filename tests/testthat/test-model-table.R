testthat::test_that("model_table: linear regression", {
  model_summary = lm_model_table(data = iris,
                                 response_variable = Petal.Width,
                                 predictor_variable = ends_with('Length'),
                                 return_result = TRUE,
                                 verbose = FALSE) %>%
    .[1:2,] %>% 
    dplyr::mutate(Petal.Length =  stringr::str_trim(stringr::str_replace(string = Petal.Length, pattern = '\\*\\*\\*',replacement = ""))) %>%
    dplyr::mutate(Sepal.Length =  stringr::str_trim(stringr::str_replace(string = Sepal.Length, pattern = '\\*\\*\\*',replacement = ""))) %>% 
    array()
  
  lm_1_check = lm(data = iris,Petal.Width ~ Sepal.Length)$coefficients %>% format_round(3) %>% stringr::str_trim()
  lm_2_check = lm(data = iris,Petal.Width ~ Petal.Length)$coefficients %>% format_round(3) %>% stringr::str_trim()

  testthat::expect_equal(lm_1_check,model_summary[[2]])
  testthat::expect_equal(lm_2_check,model_summary[[3]])
})
