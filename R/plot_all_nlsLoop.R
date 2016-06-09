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
  suppressPackageStartupMessages(library(ggplot2))
  id_col <- as.character(param_data$info$id_col)
  x <- as.character(param_data$info$params_ind)
  y <- as.character(param_data$info$param_dep)

  predict_data <- param_data$predictions

  id <- unique(raw_data[,id_col])

  pdf(file_name)
  for(i in 1:length(id)){
    plot <- ggplot() +
      geom_line(aes_string(x = x, y = y), predict_data[predict_data[,id_col] == id[i],], col = 'red', linetype = 2) +
      geom_point(aes_string(x = x, y = y), shape = 21, fill = 'white', size = 2.75, raw_data[raw_data[,id_col] == id[i],]) +
      ylab(y) +
      xlab(x) +
      theme_bw(base_family = 'Helvetica', base_size = 14) +
      ggtitle(id[i])

    print(plot)
  }

  dev.off()

}
