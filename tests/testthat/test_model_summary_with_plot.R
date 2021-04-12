########################################## Correct usage ##############################################################################
model_summary_with_plot(
  response_variable = "JS_Individual",
  random_effect_factors = Age_Individual,
  non_random_effect_factors = tidyselect::contains("Country"),
  id = "Country",
  data = EWCS_2015_shorten,
  print_result = NULL,
  return_result = T
)

model_summary_with_plot(
  response_variable = "JS_Individual",
  random_effect_factors = Age_Individual,
  non_random_effect_factors = tidyselect::contains("Country"),
  id = "Country",
  data = EWCS_2015_shorten,
  print_result = NULL,
  return_result = T
)

model_summary_with_plot(
  response_variable = "JS_Individual",
  random_effect_factors = Gender_Individual:Age_Individual,
  non_random_effect_factors = Hofstede_IC_Country,
  id = "Country",
  data = EWCS_2015_shorten,
  print_result = NULL,
  return_result = T
)

model_summary_with_plot(
  response_variable = "JS_Individual",
  random_effect_factors = c(Gender_Individual, Age_Individual),
  non_random_effect_factors = tidyselect::contains("Country"),
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
    random_effect_factors = "SWB_Individual",
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
  non_random_effect_factors = tidyselect::contains("Country"),
  id = "Country",
  data = EWCS_2015_shorten
))
