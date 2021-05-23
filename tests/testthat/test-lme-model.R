
testthat::test_that(desc = "lme_model: nlme", {
  fit <- lme_model(
    data = popular,
    response_variable = popular,
    random_effect_factors = sex,
    non_random_effect_factors = c(extrav, sex, texp),
    id = class,
    use_package = "nlme",
    opt_control = "optim",
    quite = T
  )
  expect_equal(class(fit), "lme")
})


testthat::test_that(desc = "lme_model: lme4", {
  fit <- lme_model(
    data = popular,
    response_variable = popular,
    non_random_effect_factors = c(extrav, sex),
    id = class,
    use_package = "lme4",
    quite = T
  )
  expect_equal(class(fit)[1], "lmerMod")
})

testthat::test_that(desc = "lme_model: lmerTest", {
  fit <- lme_model(
    data = popular,
    response_variable = popular,
    non_random_effect_factors = c(extrav, sex),
    id = class,
    use_package = "lmerTest",
    quite = T
  )
  expect_equal(class(fit)[1], "lmerModLmerTest")
})

testthat::test_that(desc = "lme_model: lmerTest (specify model)", {
  fit <- lme_model(
    data = popular,
    model = "popular ~ extrav + sex + (1 | class)",
    use_package = "lmerTest"
  )
  expect_equal(class(fit)[1], "lmerModLmerTest")
})
