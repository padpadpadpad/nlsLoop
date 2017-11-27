# function

#' Creates a graph of one of the levels of id_col with the data points and corresponding predictions
#'
#' @param data the data frame containing the data used in the nlsLoop call.
#' @param param_data the nlsLoop object.
#' @param id the level of \code{id_col} from which the plot is desired.
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
#'
#' plot_id_nlsLoop(data = Chlorella_TRC_test, param_data = fits, id = '1')
#'
#' @export plot_id_nlsLoop

# plot single curve
plot_id_nlsLoop <- function(data, param_data, id){
  id_col <- unique(param_data$info$id_col)
  x <- param_data$info$params_ind
  y <- param_data$info$param_dep
  raw_dat <- data[data[,id_col] == id,]
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
