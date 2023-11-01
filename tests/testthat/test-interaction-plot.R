testthat::test_that("interaction_plot: lm model", {
  model <- lm_model(
    data = iris[1:4],
    response_variable = 'Sepal.Length',
    predictor_variable = c('Sepal.Width', 'Petal.Width'),
    two_way_interaction_factor = c('Sepal.Width', 'Petal.Width'),
    quite = TRUE
  )
  plot <- interaction_plot(model)
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: lme4 model", {
  suppressMessages(model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, sex, texp),
    two_way_interaction_factor = c(sex, extrav),
    id = class,
    use_package = "lme4",
    quite = TRUE
  ))
  plot <- interaction_plot(model)
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: lmerTest model", {
  suppressMessages(model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, sex, texp),
    two_way_interaction_factor = c(sex, extrav),
    id = class,
    use_package = "lmerTest",
    quite = TRUE
  ))
  plot <- interaction_plot(model)
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: nlme model", {
  model <- lme_model(
    data = psycModel::popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, sex, texp),
    two_way_interaction_factor = c(sex, extrav),
    id = class,
    use_package = "nlme",
    opt_control = "optim",
    quite = TRUE
  )
  suppressWarnings(plot <- interaction_plot(model))
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: lm model", {
  model <- lm_model(
    data = iris[1:4],
    response_variable = Sepal.Length,
    predictor_variable = c(Sepal.Width, Petal.Width, Petal.Length),
    three_way_interaction_factor = c(Sepal.Width, Petal.Width, Petal.Length),
    quite = TRUE
  )
  plot <- interaction_plot(model)
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: lme4 model", {
  suppressMessages(model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, texp),
    three_way_interaction_factor = c(sex, extrav, texp),
    id = class,
    use_package = "lme4",
    quite = TRUE
  ))
  plot <- interaction_plot(model)
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: lmerTest model", {
  suppressMessages(model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, texp),
    three_way_interaction_factor = c(sex, extrav, texp),
    id = class,
    use_package = "lmerTest",
    quite = TRUE
  ))
  plot <- interaction_plot(model)
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: nlme model", {
  model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, texp),
    three_way_interaction_factor = c(sex, extrav, texp),
    id = class,
    use_package = "nlme",
    opt_control = "optim",
    quite = TRUE
  )
  suppressWarnings(plot <- interaction_plot(model))
  testthat::expect_false(is.null(plot))
})

testthat::test_that("interaction_plot: nlme model", {
  suppressMessages(model <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, texp),
    two_way_interaction_factor = c(sex, extrav, texp),
    id = class,
    quite = TRUE
  ))
  plot <- testthat::expect_warning(interaction_plot(model))
  testthat::expect_false(is.null(plot))
})

