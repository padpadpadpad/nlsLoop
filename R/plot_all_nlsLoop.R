#' Plot all the predicted fits of an nlsLoop object
#'
#' creates a pdf of all models fitted with nlsLoop.
#'
#' Creates a pdf where each new page is a graph of one level of \code{id_col} with raw data and corresponding predictions.
#' @param file_name the desired path for the pdf to be saved to.
#' @param data the data frame containing the data used in the nlsLoop argument
#' @param param_data the nlsLoop object
#' @param id_col an optional argument if a different column is desired from which each new plot panel is chosen
#' @param col_point optional argument to control colour of points
#' @param col_line optional argument to control colour of predictions
#' @param group optional argument to ensure lines are grouped properly
#' @param ... extra arguments to feed into \code{function(pdf)}
#' @author Daniel Padfield
#' @return a plot using ggplot2 of the points and predictions of a single level of \code{id_col}
#' @examples
#' # load in data
#'
#' data("Chlorella_TRC")
#' Chlorella_TRC_test <- Chlorella_TRC[Chlorella_TRC$curve_id %in% c(1:10),]
#'
#' # run nlsLoop()
#'
#'# define the Sharpe-Schoolfield equation
#' schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
#'  Tc <- 273.15 + Tc
#'  k <- 8.62e-5
#'  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
#'  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
#'  return(boltzmann.term + inactivation.term)
#'}
#'
#' fits <- nlsLoop(ln.rate ~ schoolfield_high(lnc, E, Eh, Th, temp = K, Tc = 20),
#'                 data = Chlorella_TRC_test,
#'                 tries = 500,
#'                 id_col = 'curve_id',
#'                 param_bds = c(-10, 10, 0.1, 2, 0.5, 5, 285, 330),
#'                 lower = c(lnc=-10, E=0, Eh=0, Th=0))
#' \dontrun{
#'
#' plot_all_nlsLoop('WhereYouWillSaveTheFile.pdf', data = Chlorella_TRC_test, param_data = fits)
#' }
#' @export plot_all_nlsLoop

plot_all_nlsLoop <- function(file_name, data, param_data, id_col = NULL, col_point = NULL, col_line = NULL, group = NULL, ...){

  # if statements if things are null
  if(is.null(id_col)){id_col <- as.character(unique(param_data$info$id_col))}
  if(is.null(group) & is.null(col_line)){group <- 1}
  if(is.null(group)){group <- col_line}


  x <- as.character(unique(param_data$info$params_ind))
  y <- as.character(unique(param_data$info$param_dep))

  if(!is.null(col_point)){data <- data[, c(x, y, id_col, col_point)]}
  if(is.null(col_point)){data <- data[, c(x, y, id_col)]}
  data <- data[stats::complete.cases(data),]

  predict_data <- param_data$predictions

  id <- unique(data[,id_col])
  grDevices::pdf(file_name, ...)
  pb <- progress::progress_bar$new(total = length(id), clear  = FALSE)
  for(i in 1:length(id)){
    pb$tick()
    temp_raw <- data[data[,id_col] == id[i],]
    temp_pred <-  predict_data[predict_data[,id_col] == id[i],]
    plot <- ggplot2::ggplot() +
      ggplot2::ylab(y) +
      ggplot2::xlab(x) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::ggtitle(id[i])
    if(!is.null(col_line) & nrow(temp_pred) > 0){
      plot <- plot + ggplot2::geom_line(ggplot2::aes_string(x = x, y = y,
                                             col = col_line, group = group),
                         temp_pred, linetype = 2)
    }
    if(!is.null(col_point) & nrow(temp_raw) > 0){
      plot <- plot + ggplot2::geom_point(ggplot2::aes_string(x = x, y = y, col = col_point),
                                         size = 2,
                                         temp_raw)
    }
    if(is.null(col_line) & nrow(temp_pred) > 0){
      plot <- plot + ggplot2::geom_line(ggplot2::aes_string(x = x, y = y, group = group), col = 'red',
                         temp_pred, linetype = 2)
    }
    if(is.null(col_point) & nrow(temp_raw) > 0){
      plot <- plot + ggplot2::geom_point(ggplot2::aes_string(x = x, y = y), size = 2,
                                        temp_raw)
    }

    print(plot)
  }

  grDevices::dev.off()

}
