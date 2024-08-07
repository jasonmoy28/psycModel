% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apim_sem.R
\name{APIM_sem}
\alias{APIM_sem}
\title{APIM Actor-Partner Interdependence Model (SEM)}
\usage{
APIM_sem(
  data,
  mod_type,
  predictor_a,
  predictor_p,
  outcome_a,
  outcome_p,
  med_a = NULL,
  med_p = NULL,
  mod_a = NULL,
  mod_p = NULL,
  bootstrap = NULL,
  standardized = FALSE,
  return_result = FALSE,
  quite = FALSE
)
}
\arguments{
\item{data}{data frame object}

\item{mod_type}{options are "simple" (main effect), "med" (mediation), and "mod" (moderation)}

\item{predictor_a}{predictor variable name for actor}

\item{predictor_p}{predictor variable name for partner}

\item{outcome_a}{dependent variable name for actor}

\item{outcome_p}{dependent variable name for partner}

\item{med_a}{mediation variable name for actor}

\item{med_p}{mediation variable name for partner}

\item{mod_a}{moderation variable name for actor}

\item{mod_p}{moderation variable name for partner}

\item{bootstrap}{number of bootstrapping (e.g., 5000). Default is not using bootstrap}

\item{standardized}{standardized coefficient}

\item{return_result}{return \code{lavaan::parameterestimates()}. Default is \code{FALSE}}

\item{quite}{suppress printing output. Default is \code{FALSE}}
}
\value{
data.frame from \code{lavaan::parameterestimates()}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} \cr
}
\details{
Actor-partner interdependence model using SEM approach (with lavaan). Indistinguishable dyads only. Results should be the same as those from Kenny (2015a, 2015b).
}
\examples{
APIM_sem(data = acitelli,
        predictor_a = 'Tension_A',
        predictor_p = 'Tension_P',
        outcome_a = 'Satisfaction_A',
        outcome_p = 'Satisfaction_P',
        mod_type = 'simple')
        
}
\references{
Kenny, D. A. (2015, October). An interactive tool for the estimation and testing mediation in the Actor-Partner Interdependence Model using structural equation modeling. Computer software. Available from https://davidakenny.shinyapps.io/APIMeM/.
Kenny, D. A. (2015, October). An interactive tool for the estimation and testing moderation in the Actor-Partner Interdependence Model using structural equation modeling. Computer software. Available from https://davidakenny.shinyapps.io/APIMoM/.
Stas, L, Kenny, D. A., Mayer, A., & Loeys, T. (2018). Giving Dyadic Data Analysis Away: A User-Friendly App for Actor-Partner Interdependence Models. Personal Relationships, 25 (1), 103-119. https://doi.org/10.1111/pere.12230.
}
