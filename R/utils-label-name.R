#' get label name
#'
#' @param graph_label_name label name
#' @param response_var_name outcome variable  name
#' @param predict_var1_name predictor 1 name
#' @param predict_var2_name predictor 2 name
#' @param predict_var3_name predictor 3 name
#'
#' @return vector of var name

label_name = function(graph_label_name,
                      response_var_name,
                      predict_var1_name,
                      predict_var2_name,
                      predict_var3_name) {
  
  #
  if (!is.null(graph_label_name)) {
    # If a vector of string is passed as argument, slice the vector
    if (inherits(graph_label_name,"character")) {
      response_var_plot_label <- graph_label_name[1]
      predict_var1_plot_label <- graph_label_name[2]
      if (!is.null(predict_var2_name)) {predict_var2_plot_label <- graph_label_name[3]}
      if (!is.null(predict_var3_name)) {predict_var3_plot_label <- graph_label_name[4]}
      # if a function of switch_case is passed as an argument, use the function
    } else if (inherits(graph_label_name,"function")) {
      response_var_plot_label <- graph_label_name(response_var_name)
      predict_var1_plot_label <- graph_label_name(predict_var1_name)
      if (!is.null(predict_var2_name)) {predict_var2_plot_label <- graph_label_name(predict_var2_name)}
      if (!is.null(predict_var3_name)) {predict_var3_plot_label <- graph_label_name(predict_var3_name)}
      
      # All other case use the original label
    } else {
      response_var_plot_label <- response_var_name
      predict_var1_plot_label <- predict_var1_name
      if (!is.null(predict_var1_plot_label)) {predict_var2_plot_label <- predict_var2_name}
      if (!is.null(predict_var3_name)) {predict_var3_plot_label <- predict_var3_name}
      
    }
    # All other case use the original label
  } else {
    response_var_plot_label <- response_var_name
    predict_var1_plot_label <- predict_var1_name
    if (!is.null(predict_var2_name)) {predict_var2_plot_label <- predict_var2_name}
    if (!is.null(predict_var3_name)) {predict_var3_plot_label <- predict_var3_name}
  }
  
  labels_name = c(response_var_plot_label,predict_var1_plot_label)
  if (!is.null(predict_var2_name)) {labels_name = c(labels_name,predict_var2_plot_label)}
  if (!is.null(predict_var3_name)) {labels_name = c(labels_name,predict_var3_plot_label)}
  
  return(labels_name)
  
}
