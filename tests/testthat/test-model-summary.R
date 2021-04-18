testthat::test_that(desc = 'model summary test',{
  testthat::skip_on_cran()
  model = expect_warning(glme_model(
    response_variable = incidence,
    random_effect_factors = period,
    family = "poisson", # or you can enter as poisson(link = 'log')
    id = herd,
    data = lme4::cbpp,
    quite = T))
summary = model_summary(model,return_result = T,
                        quite = T,
                        assumption_plot = T)
expect_false(is.null(summary$model_summary))
expect_false(is.null(summary$model_performance_df))
expect_false(is.null(summary$assumption_plot))
})
