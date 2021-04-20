#' Slope Estimate at Varying Level of Moderators
#'
#' The function uses the `interaction::sim_slopes()` to calculate the slope estimate at varying level of moderators (+/- 1 SD and mean). 
#' Additionally, it will produce a Johnson-Newman plot that shows when the slope estimate is not significant
#'
#' @param data data frame
#' @param model model object from `lm`, `lme`,`lmer`
#' @param two_way_interaction_factor vector of character of the two_way_interaction_factor
#' @param three_way_interaction_factor vector of character of the three_way_interaction_factor
#'
#' @return a list with the slope estimate data frame and a Johnson-Newman plot. 
#' @export
#'
#' @examples
#' fit <- lm_model(
#'   data = iris,
#'   response_variable = Sepal.Length,
#'   predictor_variable = tidyselect::everything(),
#'   three_way_interaction_factor = c(Sepal.Width, Petal.Width, Petal.Length)
#' )
#'
#' simple_slope_fit <- simple_slope(
#'   data = iris,
#'   model = fit,
#'   three_way_interaction_factor = c("Sepal.Width", "Petal.Width", "Petal.Length")
#' )
simple_slope <- function(data,
                         model,
                         two_way_interaction_factor = NULL,
                         three_way_interaction_factor = NULL) {
  ##################################### Set up #####################################################
  if (!requireNamespace("interactions", quietly = TRUE)) {
    stop("Please install.packages(c('interactions','sandwich')) use simple_slope with three-way interaction")
  }

  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Please install.packages('sandwich') use simple_slope with three-way interaction")
  }
  ##################################### two way interaction ####################################################
  if (length(two_way_interaction_factor) == 2) {
    simple_slope_model <- interactions::sim_slopes(
      data = data,
      model = model,
      pred = !!two_way_interaction_factor[1],
      modx = !!two_way_interaction_factor[2],
      jnplot = TRUE,
    )
    simple_slope_output <-
      rbind(simple_slope_model$slopes) %>%
      dplyr::mutate(dplyr::across(1, function(x) {
        dplyr::case_when(
          x > mean(x) ~ "High",
          x == mean(x) ~ "Mean",
          x < mean(x) ~ "Low "
        )
      })) %>%
      dplyr::rename(ci.lower = "2.5%") %>%
      dplyr::rename(ci.upper = "97.5%")

    colnames(simple_slope_output)[1] <- c(paste(two_way_interaction_factor[2], "Level"))
    jn_plot <- simple_slope_model$jnplot
    ##################################### three way interaction #####################################################
  } else if (length(three_way_interaction_factor) == 3) {
    if (!requireNamespace("cowplot", quietly = TRUE)) {
      stop("Please install.packages('cowplot') use simple_slope with three-way interaction")
    }

    simple_slope_model <- interactions::sim_slopes(
      data = data,
      model = model,
      pred = !!three_way_interaction_factor[1],
      modx = !!three_way_interaction_factor[2],
      mod2 = !!three_way_interaction_factor[3],
      jnplot = TRUE
    )

    if (length(simple_slope_model$slopes) == 3) { # if mod 2 is continuous
      simple_slope_output <-
        rbind(simple_slope_model$slopes[[1]], simple_slope_model$slopes[[2]], simple_slope_model$slopes[[3]]) %>%
        dplyr::mutate(dplyr::across(1, function(x) {
          dplyr::case_when(
            x > mean(x) ~ "High",
            x == mean(x) ~ "Mean",
            x < mean(x) ~ "Low "
          )
        }))
      simple_slope_output <- simple_slope_output %>%
        dplyr::mutate(Mod_1_Level = rep(c("Low ", "Mean", "High"), each = nrow(simple_slope_output) / 3)) %>%
        dplyr::select("Mod_1_Level", tidyselect::everything())
    } else if (length(simple_slope_model$slopes) == 2) { # if mod 2 is binary
      simple_slope_output <-
        rbind(simple_slope_model$slopes[[1]], simple_slope_model$slopes[[2]]) %>%
        dplyr::mutate(dplyr::across(1, function(x) {
          dplyr::case_when(
            x > mean(x) ~ "High",
            x == mean(x) ~ "Mean",
            x < mean(x) ~ "Low "
          )
        }))
      simple_slope_output <- simple_slope_output %>%
        dplyr::mutate(Mod_1_Level = rep(c("Low ", "High"), each = nrow(simple_slope_output) / 2)) %>%
        dplyr::select("Mod_1_Level", tidyselect::everything())
    }

    simple_slope_output <- simple_slope_output %>%
      dplyr::rename(ci.lower = "2.5%") %>%
      dplyr::rename(ci.upper = "97.5%") %>%
      dplyr::mutate(dplyr::across("Mod_1_Level", ~ replace(., duplicated(.), "")))

    colnames(simple_slope_output)[c(1, 2)] <- c(paste(three_way_interaction_factor[3], "Level"), paste(three_way_interaction_factor[2], "Level"))
    jn_plot <- simple_slope_model$jnplot
  } else {
    stop("Length of the interaction factor is not correct (must be 2 for two-way interaction and 3 for three-way interaction")
  }

  simple_slope_list <- list(
    simple_slope_df = simple_slope_output,
    jn_plot = jn_plot
  )

  return(simple_slope_list)
}
