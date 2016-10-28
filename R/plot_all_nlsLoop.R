#' plot_all_nlsLoop
#'
#' plot a pdf of all curves fitted with nlsLoop
#'
#' Creates a pdf where each new page is a graph of one level of \code{id_col} with raw data and corresponding predictions.
#' @param file_name the desired path for the pdf to be saved to.
#' @param raw_data the data frame containing the data used in the nlsLoop argument
#' @param param_data the nlsLoop object
#' @param id_col an optional argument if a different column is desired from which each new plot panel is chosen
#' @param col_point optional argument to control colour of points
#' @param col_line optional argument to control colour of predictions
#' @param group optional argument to ensure lines are grouped properly
#' @author Daniel Padfield
#' @return a plot using ggplot2 of the raw points and predictions of a single level of \code{id_col}
#' @examples
#' data(Chlorella_TRC)
#' Chlorella_TRC_test <- Chlorella_TRC[Chlorella_TRC$curve_id %in% c(1:10),]
#'
#' # run nlsLoop()
#'
#' fits <- nlsLoop(ln.rate ~ schoolfield.high(ln.c, Ea, Eh, Th, temp = K, Tc = 20),
#'                 data = Chlorella_TRC_test,
#'                 tries = 500,
#'                 id_col = 'curve_id',
#'                 param_bds = c(-10, 10, 0.1, 2, 0.5, 5, 285, 330),
#'                 lower = c(ln.c=-10, Ea=0, Eh=0, Th=0))
#'
#' \dontrun{
#'
#' plot_all_nlsLoop('WhereYouWillSaveTheFile.pdf', raw_data = Chlorella_TRC_test, param_data = fits)
#' }
#' @export plot_all_nlsLoop

plot_all_nlsLoop <- function(file_name, raw_data, param_data, id_col = NULL, col_point = NULL, col_line = NULL, group = NULL, ...){

  # if statements if things are null
  if(is.null(id_col)){id_col <- as.character(param_data$info$id_col)}
  if(is.null(col_line)){param_data$predictions$col_line <- 'red'}
  if(is.null(col_point)){raw_data$col_point <- 'black'}
  if(is.null(group)){group <- col_line}


  x <- as.character(param_data$info$params_ind)
  y <- as.character(param_data$info$param_dep)
  raw_data <- raw_data[,c(x, y, id_col, col_point)]
  raw_data <- raw_data[stats::complete.cases(raw_data),]

  predict_data <- param_data$predictions

  id <- unique(raw_data[,id_col])

  ggplot2::update_geom_defaults("line", list(colour = 'red', linetype = 2))

  grDevices::pdf(file_name, ...)
  for(i in 1:length(id)){
    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes_string(x = x, y = y,
                                             col = col_line, group = group),
                         predict_data[predict_data[,id_col] == id[i],], linetype = 2) +
      ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col_point),
                          shape = 21, fill = 'white', size = 2.75,
                          raw_data[raw_data[,id_col] == id[i],]) +
      ggplot2::ylab(y) +
      ggplot2::xlab(x) +
      ggplot2::theme_bw(base_family = 'Helvetica', base_size = 14) +
      ggplot2::ggtitle(id[i])
    print(plot)
  }

  grDevices::dev.off()

  if("ggplot2" %in% (.packages())){
    suppressMessages(suppressWarnings(detach("package:ggplot2", unload=TRUE)))
    suppressMessages(suppressWarnings(library(ggplot2)))
  }

}
