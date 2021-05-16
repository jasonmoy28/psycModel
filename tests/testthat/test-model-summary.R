testthat::test_that(desc = "glme model summary test", {
  testthat::skip_on_cran()
  model <- expect_warning(glme_model(
    response_variable = incidence,
    random_effect_factors = period,
    family = "poisson", # or you can enter as poisson(link = 'log')
    id = herd,
    data = lme4::cbpp,
    quite = T
  ))
  summary <-model_summary(model,
    return_result = T,
    quite = T,
    assumption_plot = T
  )
})

testthat::test_that(desc = "lme model summary test", {
  model <- expect_warning(lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, sex, texp),
    id = class,
    use_package = "nlme",
    quite = T
  ), regexp = "optim")
  summary <- expect_error(expect_warning(model_summary(model,
    return_result = T,
    assumption_plot = T,
    quite = T
  ),
  regexp = "assumption_plot does not support this model type"
  ))
})
