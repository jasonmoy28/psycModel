testthat::test_that("model_summary: lm model", {
  model <- lm_model(
    data = iris[1:4],
    response_variable = "Sepal.Length",
    predictor_variable = c(Sepal.Width, Petal.Width),
    two_way_interaction_factor = c(Sepal.Width, Petal.Width),
    quite = T
  )
  summary <- model_summary(model,
    return_result = T,
    assumption_plot = T,
    quite = T
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

testthat::test_that("model_summary: glm model", {
  expect_warning(model <- glm_model(
    response_variable = incidence,
    predictor_variable = period,
    family = "poisson",
    data = lme4::cbpp,
    quite = TRUE,
  ))
  summary <- model_summary(model,
    return_result = T,
    assumption_plot = T,
    quite = T
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

testthat::test_that(desc = "model_summary: nlme model", {
  model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, sex, texp),
    id = class,
    opt_control = "optim",
    use_package = "nlme",
    quite = T
  )
  summary <- model_summary(model,
    return_result = T,
    assumption_plot = T,
    quite = T
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

testthat::test_that(desc = "model_summary: lmerTest model", {
  model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = c(extrav),
    non_random_effect_factors = c(texp),
    id = class,
    use_package = "lmerTest",
    quite = T
  )
  summary <- model_summary(model,
    return_result = T,
    assumption_plot = T,
    quite = T
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

testthat::test_that(desc = "model_summary: lme4 model", {
  model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = c(extrav),
    non_random_effect_factors = c(texp),
    id = class,
    use_package = "lme4",
    quite = T
  )
  summary <- model_summary(model,
    return_result = T,
    assumption_plot = T,
    quite = T
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})

testthat::test_that(desc = "model_summary: glme model", {
  testthat::skip_on_cran()
  model <- expect_warning(glme_model(
    response_variable = incidence,
    random_effect_factors = period,
    family = "poisson", # or you can enter as poisson(link = 'log')
    id = herd,
    data = lme4::cbpp,
    quite = T
  ))
  summary <- model_summary(model,
    return_result = T,
    assumption_plot = T,
    quite = T
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
  expect_false(is.null(summary$assumption_plot))
})
