testthat::test_that("cronbach alpha: two-way interaction", {
  return_df = suppressMessages(cronbach_alpha(
    data = lavaan::HolzingerSwineford1939,
    var_name = c('Visual','Textual','Speed'),
    c(x1,x2,x3), # one way to pass the items of a factor is by wrapping it with c()
    x4:x6, # another way to pass the items is use tidyselect syntax 
    x7:x9,
    quite = TRUE,
    return_result = TRUE))
  testthat::expect_true(inherits(return_df,'data.frame'))
})

testthat::test_that("cronbach alpha: two-way interaction", {
  return_df = suppressMessages(cronbach_alpha(
    data = lavaan::HolzingerSwineford1939,
    group = school,
    var_name = c('Visual','Textual','Speed'),
    c(x1,x2,x3), # one way to pass the items of a factor is by wrapping it with c()
    x4:x6, # another way to pass the items is use tidyselect syntax 
    x7:x9,
    quite = TRUE,
    return_result = TRUE))
  testthat::expect_true(inherits(return_df,'data.frame'))
})
