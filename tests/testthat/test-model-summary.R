testthat::test_that("model_summary: lm model", {
  model <- lm_model(
    data = iris[1:4],
    response_variable = "Sepal.Length",
    predictor_variable = c(Sepal.Width, Petal.Width),
    two_way_interaction_factor = c(Sepal.Width, Petal.Width),
    quite = TRUE
  )
  summary <- model_summary(model,
    return_result = TRUE,
    assumption_plot = FALSE,
    quite = TRUE
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
})

testthat::test_that("model_summary: anova model", {
  model = stats::aov(Petal.Length ~ Species,data = iris)
  summary <- model_summary(model,
                           return_result = TRUE,
                           assumption_plot = FALSE,
                           quite = TRUE
  )
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
})

testthat::test_that(desc = "model_summary: lmerTest model", {
  model = lmerTest::lmer(popular ~ extrav + texp + (1|class),data = popular)
  summary <- suppressWarnings(model_summary(model,
    return_result = TRUE,
    assumption_plot = FALSE,
    quite = TRUE
  ))
  expect_false(is.null(summary$model_summary))
  expect_false(is.null(summary$model_performance_df))
})
