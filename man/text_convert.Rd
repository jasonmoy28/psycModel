% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-text-convert.R
\name{text_convert}
\alias{text_convert}
\title{text_convert for super_print (internal use only)}
\usage{
text_convert(text, type)
}
\arguments{
\item{text}{character. inputted text}

\item{type}{either "greek" (convert greek character) or "command" (convert color text and font style command). Command options are bold, italic, underline, red, green, yellow, blue, silver.}
}
\value{
converted unicode-based character for greek letter, color, font style
}
\description{
Command options are bold, italic, underline, red, green, yellow, blue, silver.
Greek letter options are alpha, beta, gamma, delta, epsilon, eta, theta, lamda, pi, rho, sigma, chi, omega. If capital letter greek letter, use all capital letter command (e.g., ALPHA)
}
\keyword{internal}
