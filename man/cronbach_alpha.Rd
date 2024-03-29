% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cronbach_alpha.R
\name{cronbach_alpha}
\alias{cronbach_alpha}
\title{Cronbach alpha}
\usage{
cronbach_alpha(
  ...,
  data,
  var_name,
  group = NULL,
  quite = FALSE,
  return_result = FALSE
)
}
\arguments{
\item{...}{Items. Group each latent factors using c() with when computing Cronbach alpha for 2+ factors (see example below)}

\item{data}{\code{data.frame}. Must specify}

\item{var_name}{character or a vector of characters. The order of \code{var_name} must be same as the order of the \code{...}}

\item{group}{optional character. Specify this argument for computing Cronbach alpha for group separetely}

\item{quite}{suppress printing output}

\item{return_result}{If it is set to \code{TRUE}, it will return a \code{dataframe} object}
}
\value{
a \code{data.frame} object if return_result is \code{TRUE}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} \cr
Computing the Cronbach alphas for multiple factors.
}
\examples{
cronbach_alpha(
  data = lavaan::HolzingerSwineford1939,
  var_name = c('Visual','Textual','Speed'),
  c(x1,x2,x3), # one way to pass the items of a factor is by wrapping it with c()
  x4:x6, # another way to pass the items is use tidyselect syntax 
  x7:x9)
}
