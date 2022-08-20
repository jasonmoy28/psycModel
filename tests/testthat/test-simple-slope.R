testthat::test_that(desc = "simple_slope: two-way-interaction", {
  model <- lm_model(
    data = iris[1:4],
    response_variable = "Sepal.Length",
    predictor_variable = c(Sepal.Width, Petal.Width),
    two_way_interaction_factor = c(Sepal.Width, Petal.Width),
    quite = TRUE
  )

  summary <- simple_slope(
    model = model)

  expect_equal(c(summary$simple_slope_df[1])[[1]], c("Low", "Mean", "High"))
})

testthat::test_that(desc = "simple_slope: three-way-interaction", {
  model <- lm_model(
    data = iris[1:4],
    response_variable = "Sepal.Length",
    predictor_variable = c(Sepal.Width, Petal.Width),
    three_way_interaction_factor = c(Sepal.Width, Petal.Width, Petal.Length),
    quite = TRUE
  )

  summary <- simple_slope(
    model = model,
  )

  expect_equal(c(summary$simple_slope_df[1])[[1]], c("Low", "", "", "Mean", "", "", "High", "", ""))
  expect_equal(c(summary$simple_slope_df[2])[[1]], c("Low", "Mean", "High", "Low", "Mean", "High", "Low", "Mean", "High"))
})
