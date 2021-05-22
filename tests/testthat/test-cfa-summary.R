test_that('cfa_summary: single factor',{
  expect_warning(summary <- cfa_summary(
    data = lavaan::HolzingerSwineford1939,
    x1:x3,
    return_result = TRUE,
    quite = TRUE,
    plot = FALSE
  ),regexp = 'coerced')
  expect_equal(class(summary)[1],'lavaan')
})

test_that('cfa_summary: mutiple factor',{
  expect_warning(summary <- cfa_summary(
    data = lavaan::HolzingerSwineford1939,
    x1:x3,
    x4:x6,
    x7:x9,
    return_result = TRUE,
    plot = FALSE,
    quite = TRUE
  ))
  expect_equal(class(summary)[1],'lavaan')
})

test_that('cfa_summary: mutiple factor with group',{
  expect_warning(summary <- cfa_summary(
    data = lavaan::HolzingerSwineford1939,
    x1:x3,
    x4:x6,
    x7:x9,
    group = school,
    return_result = TRUE,
    plot = FALSE,
    quite = TRUE
  ))
  expect_equal(class(summary)[1],'lavaan')
})
