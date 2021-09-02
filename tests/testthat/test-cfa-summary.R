test_that("cfa_summary: single factor", {
  summary <- cfa_summary(
    data = lavaan::HolzingerSwineford1939,
    x1:x3,
    return_result = TRUE,
    quite = TRUE,
    plot = FALSE
  )
  expect_equal(class(summary)[1], "lavaan")
})

test_that("cfa_summary: mutiple factor", {
  summary <- cfa_summary(
    data = lavaan::HolzingerSwineford1939,
    x1:x3,
    x4:x6,
    x7:x9,
    return_result = TRUE,
    plot = FALSE,
    quite = TRUE
  )
  expect_equal(class(summary)[1], "lavaan")
})

test_that("cfa_summary: mutiple factor with group", {
 summary <- cfa_summary(
    data = lavaan::HolzingerSwineford1939,
    x1:x3,
    x4:x6,
    x7:x9,
    group = school,
    return_result = TRUE,
    plot = FALSE,
    quite = TRUE
  )
  expect_equal(class(summary)[1], "lavaan")
})
