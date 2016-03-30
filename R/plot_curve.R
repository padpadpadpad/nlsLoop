# function

#' Creates a graph of one of the levels of id_col with the corresponding predictions fitted
#'
#' @param data the raw data frame
#' @param predictions the predictions data frame
#' @param id_col column name of looping factor in raw data frame
#' @param id the level of id_col that you want to plot
#' @param x the value of the independent variable
#' @param y the value of the dependent variable in the raw data frame
#' @param new.y the value of the dependent variable in the predictions data frame
#' @author Daniel Padfield
#' @return a plot using ggplot2 of the raw points and predictions of a single level of \code{id_col}
#' @note variables \code{id_col}, \code{id},\code{x},\code{y}, and \code{predict.y} need to be in \code{''} or \code{""}
#'
#' @export plot_curve

# plot single curve
plot_curve <- function(data, predictions, id_col, id, x, y, predict.y){
  # load in package
  library(ggplot2)
  ggplot() +
    geom_line(aes_string(x = x, y = predict.y), predictions[predictions[, id_col] == id,], col = 'red', linetype = 2) +
    geom_point(aes_string(x = x, y = y), shape = 21, fill = 'white', size = 2.75, data[data[, id_col] == id,]) +
    ylab(y) +
    theme_bw(base_family = 'Helvetica', base_size = 14)

}
