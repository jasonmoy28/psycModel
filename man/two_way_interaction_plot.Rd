% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/two_way_interaction_plot.R
\name{two_way_interaction_plot}
\alias{two_way_interaction_plot}
\title{Two-way Interaction Plot}
\usage{
two_way_interaction_plot(
  model,
  graph_label_name = NULL,
  cateogrical_var = NULL,
  y_lim = NULL,
  plot_color = FALSE
)
}
\arguments{
\item{model}{object from nlme, lme4, or lmerTest (linear mixed effect model)}

\item{graph_label_name}{vector of length 3 or function. Vector should be passed in the form of \code{c(response_var, predict_var1, predict_var2)}. Function should be passed as a switch function that return the label based on the name passed (e.g., a switch function)}

\item{cateogrical_var}{list. Specify the upper bound and lower bound directly instead of using ± 1 SD from the mean. Passed in the form of \code{list(var_name1 = c(upper_bound1, lower_bound1),var_name2 = c(upper_bound2, lower_bound2))}}

\item{y_lim}{vector of length 2. c(lower_limit, upper_limit). Default is +0.5 of the max -0.5 of the min Y value}

\item{plot_color}{logical. default as F. Set to T if you want to plot in color}
}
\value{
ggplot object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} \cr
The function creates a two-way interaction plot. It will creates a plot with ± 1 SD from the mean of the independent variable. See supported model below
}
\examples{
# run the fit model first
fit <- lme_model(
  response_variable = JS_Individual,
  level_1_factors = Age_Individual,
  level_2_factors = contains("Country"),
  two_way_interaction_factor = c("Age_Individual", "Hofstede_IC_Country"),
  id = "Country",
  data = EWCS_2015_shorten
)

two_way_interaction_plot(
  model = fit,
  # this will change the plot name
  graph_label_name = c(
    "Job Satisfaction",
    "Age",
    "Individualism/Collectivism"
  )
)

# Below is an equivalent way of changing graph_label_name by using the switch function
# You should store this in a separate script, and load this function using source().
# It is primarily use if you are running many models
graph_label_name <- function(var_name) {
  var_name_processed <- switch(var_name,
    "JS_Individual" = "Job Satisfaction",
    "Age_Individual" = "Age",
    "Hofstede_IC_Country" = "Individualism/Collectivism"
  )
  if (is.null(var_name_processed)) {
    var_name_processed <- var_name
  }
  return(var_name_processed)
}

two_way_interaction_plot(
  model = fit,
  # this will change the plot name
  graph_label_name = graph_label_name
)
}
\references{
Moy, J. H. (2021). psycModel: Integrated Toolkit for Psychological Analysis and Modeling in R. R package. https://github.com/jasonmoy28/psycModel
}