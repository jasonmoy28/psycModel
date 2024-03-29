% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interaction_plot.R
\name{interaction_plot}
\alias{interaction_plot}
\title{Interaction plot}
\usage{
interaction_plot(
  model,
  interaction_term = NULL,
  response_var_name = NULL,
  predict_var1_name = NULL,
  predict_var2_name = NULL,
  predict_var3_name = NULL,
  predict_var1_level = NULL,
  predict_var2_level = NULL,
  predict_var3_level = NULL,
  predict_var1_level_name = NULL,
  predict_var2_level_name = NULL,
  predict_var3_level_name = NULL,
  y_lim = NULL,
  plot_color = FALSE,
  return_plot_data = FALSE,
  return_plot = FALSE,
  verbose = TRUE,
  print_plot = TRUE,
  data = NULL
)
}
\arguments{
\item{model}{a regression model object from \link[effects]{effect}.}

\item{interaction_term}{default is the first highest order interaction term in the model. The term should be given explicitly if you want to plot other interaction terms.}

\item{response_var_name}{The name of the response variable can be changed using this setting.}

\item{predict_var1_name}{The name of the first predictor can be changed using this setting.}

\item{predict_var2_name}{The name of the second predictor can be changed using this setting.}

\item{predict_var3_name}{The name of the third predictor can be changed using this setting.}

\item{predict_var1_level}{The default is -1 SD and +1 SD for a continuous variable, and it is the two levels for a binary variable. These can be changed using this setting.}

\item{predict_var2_level}{The default is -1 SD and +1 SD for a continuous variable, and it is the two levels for a binary variable. These can be changed using this setting.}

\item{predict_var3_level}{The default is -1 SD and +1 SD for a continuous variable, and it is the two levels for a binary variable. These can be changed using this setting.}

\item{predict_var1_level_name}{The labels of the level can be change using this value (e.g., \code{c('-1 SD','+1 SD')}). The order should be from the left to right on the x-axis.}

\item{predict_var2_level_name}{The labels of the level can be change using this value (e.g., \code{c('-1 SD','+1 SD')}). The order should be from the top to down on the legend.}

\item{predict_var3_level_name}{The labels of the level can be change using this value (e.g., \code{c('-1 SD','+1 SD')}). The order should be from the left to right on the facets.}

\item{y_lim}{the plot's upper and lower limit for the y-axis. Length of 2. Example: \code{c(lower_limit, upper_limit)}}

\item{plot_color}{default if \code{FALSE}. Set to \code{TRUE} if you want to plot in color}

\item{return_plot_data}{default is \code{FALSE}. Set to \code{TRUE} to return the plot data.}

\item{return_plot}{default is \code{FALSE}. Set to \code{TRUE} to return the plot.}

\item{verbose}{deafult is \code{TRUE}.}

\item{print_plot}{default is \code{TRUE}. Set to \code{TRUE} to print the plot.}

\item{data}{Optional data.frame. Only used when it is not possible to extract data from the model object.}
}
\value{
a \code{ggplot} object
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} \cr
The function creates interaction plot. By default, it will create an interaction plot using -1 SD and +1 SD of continuous variables, or the two levels of binary variables.
}
\examples{

model_1 <- lm(Sepal.Length ~ Petal.Width * Sepal.Width,
              data = iris)
interaction_plot(model_1)

model_2 <- lm(Sepal.Length ~ Petal.Width * Sepal.Width * Petal.Length,
              data = iris
)

interaction_plot(model_2, # it will automatically select the first three-way interaction term

                 # change the name of the variables of the plot
                 response_var_name = 'SEPAL LENGTH',
                 predict_var1_name = 'PETAL WIDTH',
                 predict_var2_name = 'SEPAL WIDTH',
                 predict_var3_name = 'PETAL LENGTH',
                           
                 # change the number of levels of the variables (e.g., adding the mean)
                 predict_var1_level = c(0.43, 1.19,1.96), 
                 predict_var2_level = c(2.62, 3.05,3.49), 
                 predict_var3_level = c(1.99,3.758,5.52),
                 predict_var1_level_name = c('-1 SD','Mean','+1 SD'),
                 predict_var2_level_name = c('-1 SD','Mean','+1 SD'),
                 predict_var3_level_name = c('-1 SD','Mean','+1 SD')) 
}
