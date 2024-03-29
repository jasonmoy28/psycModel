#' Slope Estimate at Varying Level of Moderators
#'
#' `r lifecycle::badge("stable")` \cr
#' The function uses the `interaction::sim_slopes()` to calculate the slope estimate at varying level of moderators (+/- 1 SD and mean).
#' Additionally, it will produce a Johnson-Newman plot that shows when the slope estimate is not significant
#'
#' @param data `data.frame`
#' @param model model object from `lm`, `lme`,`lmer`
#'
#' @return a list with the slope estimate data frame and a Johnson-Newman plot.
#' @export
#'
#' @examples
#' fit <- lm_model(
#'   data = iris,
#'   response_variable = Sepal.Length,
#'   predictor_variable = dplyr::everything(),
#'   three_way_interaction_factor = c(Sepal.Width, Petal.Width, Petal.Length)
#' )
#'
#' simple_slope_fit <- simple_slope(
#'   model = fit,
#' )
simple_slope <- function(model,
                         data = NULL) {
  ##################################### Custom function #####################################################
  getfun <- function(x) {
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }
  ##################################### Set up #####################################################
  if (!requireNamespace("interactions", quietly = TRUE)) {
    stop(
      "Please install.packages(c('interactions','sandwich')) use simple_slope with three-way interaction"
    )
  }
  
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop(
      "Please install.packages('sandwich') use simple_slope with three-way interaction"
    )
  }
  if (is.null(data)) {
    data = insight::get_data(model)
  }
  
  interaction_factor = unlist(get_interaction_term(model), use.names = FALSE)
  
  if (inherits(model,'lmerMod')) {
    model <- do.call(getfun("lmerTest::lmer"), list(formula = model, data = data))
  }
  

  ##################################### two way interaction ####################################################
  
  
  if (length(interaction_factor) == 2) {
    simple_slope_model <- interactions::sim_slopes(
      data = data,
      model = model,
      pred = !!interaction_factor[1],
      modx = !!interaction_factor[2],
      jnplot = TRUE,
    )
    simple_slope_output <-
      rbind(simple_slope_model$slopes) %>%
      dplyr::mutate(dplyr::across(1, function(x) {
        dplyr::case_when(round(x,digits = 6) == round(mean(x),digits = 6) ~ "Mean",
                         x > mean(x) ~ "High",
                         x < mean(x) ~ "Low")
      })) %>% 
      dplyr::rename(ci.lower = "2.5%") %>%
      dplyr::rename(ci.upper = "97.5%")
    colnames(simple_slope_output)[1] <-
      c(paste(interaction_factor[2], "Level"))
    jn_plot <- simple_slope_model$jnplot
    ##################################### three way interaction #####################################################
  } else if (length(interaction_factor) == 3) {
    if (!requireNamespace("cowplot", quietly = TRUE)) {
      stop(
        "Please install.packages('cowplot') use simple_slope with three-way interaction"
      )
    }
    
    simple_slope_model <- interactions::sim_slopes(
      data = data,
      model = model,
      pred = !!interaction_factor[1],
      modx = !!interaction_factor[2],
      mod2 = !!interaction_factor[3],
      jnplot = TRUE
    )
    
    if (length(simple_slope_model$slopes) == 3) {
      # if mod 2 is continuous
      simple_slope_output <-
        rbind(
          simple_slope_model$slopes[[1]],
          simple_slope_model$slopes[[2]],
          simple_slope_model$slopes[[3]]
        ) %>%
        dplyr::mutate(dplyr::across(1, function(x) {
          dplyr::case_when(round(x,digits = 6) == round(mean(x),digits = 6) ~ "Mean",
                           x > mean(x) ~ "High",
                           x < mean(x) ~ "Low")
        }))
      simple_slope_output <- simple_slope_output %>%
        dplyr::mutate(Mod_1_Level = rep(c("Low", "Mean", "High"), each = nrow(simple_slope_output) / 3)) %>%
        dplyr::select("Mod_1_Level", dplyr::everything())
    } else if (length(simple_slope_model$slopes) == 2) {
      # if mod 2 is binary
      simple_slope_output <-
        rbind(simple_slope_model$slopes[[1]],
              simple_slope_model$slopes[[2]]) %>%
        dplyr::mutate(dplyr::across(1, function(x) {
          dplyr::case_when(round(x,digits = 6) == round(mean(x),digits = 6) ~ "Mean",
                           x > mean(x) ~ "High",
                           x < mean(x) ~ "Low")
        }))
      
      simple_slope_output <- simple_slope_output %>%
        dplyr::mutate(Mod_1_Level = rep(c("Low", "High"), each = nrow(simple_slope_output) / 2)) %>%
        dplyr::select("Mod_1_Level", dplyr::everything())
    }
    
    simple_slope_output <- simple_slope_output %>%
      dplyr::rename(ci.lower = "2.5%") %>%
      dplyr::rename(ci.upper = "97.5%") %>%
      dplyr::mutate(dplyr::across("Mod_1_Level", ~ replace(., duplicated(.), "")))
    
    colnames(simple_slope_output)[c(1, 2)] <-
      c(paste(interaction_factor[3], "Level"),
        paste(interaction_factor[2], "Level"))
    jn_plot <- simple_slope_model$jnplot
  } else {
    stop(
      "Length of the interaction factor is not correct (must be 2 for two-way interaction and 3 for three-way interaction"
    )
  }
  
  simple_slope_list <- list(simple_slope_df = simple_slope_output,
                            jn_plot = jn_plot)
  
  return(simple_slope_list)
}
