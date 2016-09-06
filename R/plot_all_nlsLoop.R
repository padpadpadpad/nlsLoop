#' plot_all_nlsLoop
#'
#' plot a pdf of all curves fitted with nlsLoop
#'
#' Creates a pdf where each new page is a graph of one level of id_col with raw data and corresponding predictions
#' @param file_name The desired path for the pdf to be saved to
#' @param raw_data The data frame containing the data used in the nlsLoop argument
#' @param param_data The nlsLoop object
#' @author Daniel Padfield
#' @return a plot using ggplot2 of the raw points and predictions of a single level of \code{id_col}
#'
#' @export plot_all_nlsLoop

plot_all_nlsLoop <- function(file_name, raw_data, param_data){
  # load in package
  id_col <- as.character(param_data$info$id_col)
  x <- as.character(param_data$info$params_ind)
  y <- as.character(param_data$info$param_dep)
  raw_data <- raw_data[,c(x,y, id_col)]
  raw_data <- raw_data[stats::complete.cases(raw_data),]

  predict_data <- param_data$predictions

  id <- unique(raw_data[,id_col])

  grDevices::pdf(file_name)
  for(i in 1:length(id)){
    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes_string(x = x, y = y), predict_data[predict_data[,id_col] == id[i],], col = 'red', linetype = 2) +
      ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), shape = 21, fill = 'white', size = 2.75, raw_data[raw_data[,id_col] == id[i],]) +
      ggplot2::ylab(y) +
      ggplot2::xlab(x) +
      ggplot2::theme_bw(base_family = 'Helvetica', base_size = 14) +
      ggplot2::ggtitle(id[i])

    print(plot)
  }

  grDevices::dev.off()

}
