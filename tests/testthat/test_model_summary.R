fit <- lme_model(
  response_variable = "Reaction",
  random_effect_factors = "Days",
  id = "Subject",
  data = lme4::sleepstudy,
  use_package = "lme4"
)

model_summary(fit)
