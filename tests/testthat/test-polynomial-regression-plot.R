testthat::test_that(desc = "polynomial regression plot: main effect", {
  fit = lm(data = iris, Sepal.Length ~ poly(Petal.Length,2))
  plot = polynomial_regression_plot(model = fit,predictor = 'Petal.Length')
  testthat::expect_true(inherits(plot,'ggplot'))
})

testthat::test_that(desc = "polynomial regression plot: interaction", {
  fit = lm(data = iris, Sepal.Length ~ poly(Petal.Length,2)*Petal.Width)
  plot = polynomial_regression_plot(model = fit,predictor = 'Petal.Length')
  testthat::expect_true(inherits(plot,'ggplot'))
})
