% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efa_summary.R
\name{efa_summary}
\alias{efa_summary}
\title{Exploratory Factor Analysis}
\usage{
efa_summary(
  data,
  cols,
  rotation = "varimax",
  optimal_factor_method = FALSE,
  efa_plot = TRUE,
  digits = 3,
  n_factor = NULL,
  post_hoc_cfa = FALSE,
  quite = FALSE,
  streamline = FALSE,
  return_result = FALSE
)
}
\arguments{
\item{data}{\code{data.frame}}

\item{cols}{columns. Support \code{dplyr::select()} syntax.}

\item{rotation}{the rotation to use in estimation. Default is 'oblimin'. Options are 'none', 'varimax', 'quartimax', 'promax', 'oblimin', or 'simplimax'}

\item{optimal_factor_method}{Show a summary of the number of factors by optimization method (e.g., BIC, VSS complexity, Velicer's MAP)}

\item{efa_plot}{show explained variance by number of factor plot. default is \code{TRUE}.}

\item{digits}{number of digits to round to}

\item{n_factor}{number of factors for EFA. It will bypass the initial optimization algorithm, and fit the EFA model using this specified number of factor}

\item{post_hoc_cfa}{a CFA model based on the extracted factor}

\item{quite}{suppress printing output}

\item{streamline}{print streamlined output}

\item{return_result}{If it is set to \code{TRUE} (default is \code{FALSE}), it will return a \code{fa} object from \code{psych}}
}
\value{
a \code{fa} object from \code{psych}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} \cr
The function is used to fit a exploratory factor analysis model. It will first find the optimal number of factors using parameters::n_factors. Once the optimal number of factor is determined, the function will fit the model using
\code{psych::fa()}. Optionally, you can request a post-hoc CFA model based on the EFA model which gives you more fit indexes (e.g., CFI, RMSEA, TLI)
}
\examples{
efa_summary(lavaan::HolzingerSwineford1939, starts_with("x"), post_hoc_cfa = TRUE)
}
