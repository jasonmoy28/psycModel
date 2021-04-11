########################################## Correct usage ##############################################################################
model_summary_with_plot(
  response_variable = "JS_Individual",
  level_1_factors = Age_Individual,
  level_2_factors = tidyselect::contains("Country"),
  id = "Country",
  data = EWCS_2015_shorten,
  print_result = NULL,
  return_result = T
)

model_summary_with_plot(
  response_variable = "JS_Individual",
  level_1_factors = Age_Individual,
  level_2_factors = tidyselect::contains("Country"),
  id = "Country",
  data = EWCS_2015_shorten,
  print_result = NULL,
  return_result = T
)

model_summary_with_plot(
  response_variable = "JS_Individual",
  level_1_factors = Gender_Individual:Age_Individual,
  level_2_factors = Hofstede_IC_Country,
  id = "Country",
  data = EWCS_2015_shorten,
  print_result = NULL,
  return_result = T
)

model_summary_with_plot(
  response_variable = "JS_Individual",
  level_1_factors = c(Gender_Individual, Age_Individual),
  level_2_factors = tidyselect::contains("Country"),
  two_way_interaction_factor = contains("Individual"),
  id = "Country",
  data = EWCS_2015_shorten,
  print_result = NULL,
  return_result = T
)


########################################## Incorrect usage ##############################################################################
testthat::expect_warning(
  model_summary_with_plot(
    response_variable = "JS_Individual",
    level_1_factors = "SWB_Individual",
    id = "Country",
    data = EWCS_2015_shorten,
    use_package = "lmerTest", # use lmerTest
    model_performance = NULL,
    return_result = F
  ),
  regexp = "optimizer"
)

testthat::expect_error(model_summary_with_plot(
  response_variable = "JS_Individual",
  level_2_factors = tidyselect::contains("Country"),
  id = "Country",
  data = EWCS_2015_shorten
))
