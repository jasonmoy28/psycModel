#' Popular dataset
#'
#' Classic data-set from Chapter 2 of Joop Hox’s Multilevel Analysis (2010). The popular dataset included student from different class (i.e., class is the nesting variable). The outcome variable is a self-rated popularity scale. Individual-level (i.e., level 1) predictors are sex, extroversion. Class level (i.e., level 2) predictor is teacher experience.
#'
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{pupil}{Subject ID}
#'   \item{popular}{Self-rated popularity scale ranging from 1 to 10}
#'   \item{class}{the class that students belong to (nesting variable)}
#'   \item{extrav}{extraversion scale (individual-level)}
#'   \item{sex}{gender of the student (individual-level)}
#'   \item{texp}{teacher experience (class-level)}
#' }
#' @source \url{http://joophox.net/mlbook2/DataExchange.zip}
"popular"
