testthat::test_that("anova plot: two-way interaction", {
  fit = iris %>% lm(data = ., Sepal.Length ~ Species)
  plot = anova_plot(fit,predictor = 'Species')
  testthat::expect_true(!is.null(plot))
})

testthat::test_that("anova plot: two-way interaction", {
  fit = iris %>% lm(data = ., Sepal.Length ~ Species*Petal.Width)
  plot = anova_plot(fit)
  testthat::expect_true(!is.null(plot))
})

testthat::test_that("anova plot: three-way interaction", {
  fit = iris %>% lm(data = ., Sepal.Length ~ Species*Petal.Width*Petal.Length)
  plot = anova_plot(fit)
  testthat::expect_true(!is.null(plot))
})
s

