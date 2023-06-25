testthat::test_that("anova plot: no interaction", {
  fit = iris %>% lm(data = ., Sepal.Length ~ Species)
  plot = suppressMessages(anova_plot(fit,predictor = 'Species'))
  testthat::expect_true(!is.null(plot))
})

testthat::test_that("anova plot: two-way interaction with 1 categorical predictor and 1 continuous predictor", {
  fit = lavaan::HolzingerSwineford1939 %>% 
    dplyr::mutate(dplyr::across(c(sex),as.factor)) %>% 
    dplyr::mutate(dplyr::across(c(ageyr),as.numeric)) %>% 
    lm(data = ., grade ~ sex*ageyr)
  plot = anova_plot(fit)
  testthat::expect_true(!is.null(plot))
})

testthat::test_that("anova plot: two-way interaction with 2 categorical predictors", {
  fit = lavaan::HolzingerSwineford1939 %>% 
    dplyr::mutate(dplyr::across(c(sex,school),as.factor)) %>% 
    lm(data = ., grade ~ sex*school)
  plot =  suppressMessages(anova_plot(fit))
  testthat::expect_true(inherits(plot,'ggplot'))
})
  
testthat::test_that("anova plot: three-way interaction with 1 categorical predictor and 2 continuous predictors", {
  fit = lavaan::HolzingerSwineford1939 %>% 
    dplyr::mutate(dplyr::across(c(sex),as.factor)) %>% 
    dplyr::mutate(dplyr::across(c(ageyr,x1),as.numeric)) %>%  
    lm(data = ., grade ~ sex*x1*ageyr)
  plot = anova_plot(fit)
  testthat::expect_true(inherits(plot,'ggplot'))
})

testthat::test_that("anova plot: three-way interaction with 2 categorical moderator and 1 continuous moderator", {
  fit = lavaan::HolzingerSwineford1939 %>% 
    dplyr::mutate(dplyr::across(c(sex,school),as.factor)) %>% 
    dplyr::mutate(dplyr::across(c(ageyr),as.numeric)) %>% 
    lm(data = ., grade ~ school*sex*ageyr)
  plot = anova_plot(fit)
  testthat::expect_true(inherits(plot,'ggplot'))
})

testthat::test_that("anova plot: three-way interaction with 3 categorical moderator", {
  fit = lavaan::HolzingerSwineford1939 %>% 
    dplyr::mutate(dplyr::across(c(sex,ageyr,school),as.factor)) %>% 
    lm(data = ., grade ~ ageyr*sex*school)
  plot =  suppressMessages(anova_plot(fit))
  testthat::expect_true(inherits(plot,'ggplot'))
})
