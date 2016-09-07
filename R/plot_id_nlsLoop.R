# function

#' Creates a graph of one of the levels of id_col with raw data and corresponding predictions
#'
#' @param raw_data The data frame containing the data used in the nlsLoop argument
#' @param param_data The nlsLoop object
#' @param id The level of id_col from which the plot is desired
#' @author Daniel Padfield
#' @return a plot using ggplot2 of the raw points and predictions of a single level of \code{id_col}
#'
#' @export plot_id_nlsLoop

# plot single curve
plot_id_nlsLoop <- function(raw_data, param_data, id){
  id_col <- as.character(param_data$info$id_col)
  x <- as.character(param_data$info$params_ind)
  y <- as.character(param_data$info$param_dep)
  raw_dat <- raw_data[raw_data[,id_col] == id,]
  predict_data <- param_data$predictions
  predict_data <- predict_data[predict_data[,id_col] == id,]
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes_string(x = x, y = y), predict_data, col = 'red', linetype = 2) +
    ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), shape = 21, fill = 'white', size = 2.75, raw_dat) +
    ggplot2::ylab(y) +
    ggplot2::xlab(x) +
    ggplot2::theme_bw(base_family = 'Helvetica', base_size = 14) +
    ggplot2::ggtitle(id)
  return(plot)

}
