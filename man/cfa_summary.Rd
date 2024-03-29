% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfa_summary.R
\name{cfa_summary}
\alias{cfa_summary}
\title{Confirmatory Factor Analysis}
\usage{
cfa_summary(
  data,
  ...,
  model = NULL,
  group = NULL,
  ordered = FALSE,
  digits = 3,
  estimator = "ML",
  model_covariance = TRUE,
  model_variance = TRUE,
  plot = TRUE,
  group_partial = NULL,
  streamline = FALSE,
  quite = FALSE,
  return_result = FALSE
)
}
\arguments{
\item{data}{data frame}

\item{...}{CFA items. Multi-factor CFA items should be separated by comma (as different argument). See below for examples. Support \code{dplyr::select()} syntax.}

\item{model}{explicit \code{lavaan} model. Must be specify with \code{model = lavaan_model_syntax}. \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}}

\item{group}{optional character. used for multi-level CFA. the nested variable for multilevel dataset (e.g., Country). Support \code{dplyr::select()} syntax.}

\item{ordered}{Default is \code{FALSE}. If it is set to \code{TRUE}, \code{lavaan} will treat it as a ordinal variable and use \code{DWLS} instead of \code{ML}}

\item{digits}{number of digits to round to}

\item{estimator}{estimator for lavaan. Default is \code{ML}}

\item{model_covariance}{print model covariance. Default is \code{TRUE}}

\item{model_variance}{print model variance. Default is \code{TRUE}}

\item{plot}{print a path diagram. Default is \code{TRUE}}

\item{group_partial}{Items for partial equivalence. The form should be c('DV =~ item1', 'DV =~ item2').}

\item{streamline}{print streamlined output}

\item{quite}{suppress printing output}

\item{return_result}{If it is set to \code{TRUE}, it will return the \code{lavaan} model}
}
\value{
a \code{lavaan} object if return_result is \code{TRUE}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} \cr
The function fits a CFA model using the \code{lavaan::cfa()}. Users can fit single and multiple factors CFA, and it also supports multilevel CFA (by specifying the group).
Users can fit the model by passing the items using \code{dplyr::select()} syntax or an explicit lavaan model for more versatile usage.
All arguments (except the CFA items) must be explicitly named (e.g., model = your-model; see example for inappropriate behavior).
}
\details{
First, just like researchers have argued against p value of 0.05 is not a good cut-of, researchers have also argue against that fit indicies (more importantly, the cut-off criteria) are not completely representative of the goodness of fit.
Nonetheless, you are required to report them if you are publishing an article anyway. I will summarize the general recommended cut-off criteria for CFA model below.
Researchers consider models with CFI (Bentler, 1990) that is > 0.95 to be excellent fit (Hu & Bentler, 1999), and > 0.9 to be acceptable fit. Researchers considered a model is excellent fit if CFI > 0.95 (Hu & Bentler, 1999), RMSEA < 0.06 (Hu & Bentler, 1999), TLI > 0.95, SRMR < 0.08.
The model is considered an acceptable fit if CFI > 0.9 and RMSEA < 0.08. I need some time to find all the relevant references, but this should be the general consensus.
}
\examples{
# REMEMBER, YOU MUST NAMED ALL ARGUMENT EXCEPT THE CFA ITEMS ARGUMENT
# Fitting a multilevel single factor CFA model
fit <- cfa_summary(
  data = lavaan::HolzingerSwineford1939,
  x1:x3,
  x4:x6,
  x7:x9,
  group = "sex",
  model_variance = FALSE, # do not print the model_variance
  model_covariance = FALSE # do not print the model_covariance
)


# Fitting a CFA model by passing explicit lavaan model (equivalent to the above model)
# Note in the below function how I added `model = ` in front of the lavaan model.
# Similarly, the same rule apply for all arguments (e.g., `ordered = FALSE` instead of just `FALSE`)
\donttest{
fit <- cfa_summary(
  model = "visual  =~ x1 + x2 + x3",
  data = lavaan::HolzingerSwineford1939,
  quite = TRUE # silence all output
)
}
\dontrun{
# This will fail because I did not add `model = ` in front of the lavaan model.
# Therefore,you must add the tag in front of all arguments
# For example, `return_result = 'model'` instaed of `model`
cfa_summary("visual  =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed   =~ x7 + x8 + x9 ",
  data = lavaan::HolzingerSwineford1939
)
}
}
\references{
Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling, 6, 1–55. https://doi.org/10.1080/10705519909540118
}
