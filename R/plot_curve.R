# function

#' Creates a graph of one of the levels of id_col with the corresponding predictions fitted
#'
#'
#' @author Daniel Padfield
#' @return a plot using ggplot2 of the raw points and predictions of a single level of \code{id_col}
#' @note variables \code{id_col}, \code{id},\code{x},\code{y}, and \code{predict.y} need to be in \code{''} or \code{""}
#'
#' @export plot_curve

# plot single curve
plot_curve <- function(raw_data, param_data, id){
  # load in package
  library(ggplot2)
  id_col <- as.character(param_data$info$id_col)
  x <- as.character(param_data$info$params_ind)
  y <- as.character(param_data$info$param_dep)
  raw_dat <- raw_data[raw_data[,id_col] == id,]
  predict_data <- param_data$predictions
  predict_data <- predict_data[predict_data[,id_col] == id,]
  plot <- ggplot() +
    geom_line(aes_string(x = x, y = y), predict_data, col = 'red', linetype = 2) +
    geom_point(aes_string(x = x, y = y), shape = 21, fill = 'white', size = 2.75, raw_dat) +
    ylab(y) +
    xlab(x) +
    theme_bw(base_family = 'Helvetica', base_size = 14)
  return(plot)

}
