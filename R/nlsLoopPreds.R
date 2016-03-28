# function

#' Creates a predictions dataframe from a nlsLoop parameter results dataframe.
#'
#' Uses the parameters from the results from nlsLoop and the original dataframe to create a predictions dataframe allowing for smooth fitting to the raw data.
#'
#' @param data raw data frame
#' @param param_data data frame containing the parameters from nlsLoop() or similar format
#' @param id_col column name of looping factor in raw data frame
#' @param FUN Function name
#' @param param_x independent variable in the nlsLoop model
#' @param x name of the independent variable in the raw data frame
#' @param gap the gap desired between prediction data points
#' @param ... extra arguments to pass to FUN
#'
#' @return returns a dataframe of predictive values, \code{y} for each value of \code{id_col} and \code{x}
#' @author Daniel Padfield
#' @seealso \code{\link{nlsLoop}} for details on shotgunning fits using non-linear least squares regression
#' @note uses the minimum and maximum values of \code{x} for each value of \code{id_col} to predict extrapolation of non-linear fits to values not given by the data
#' @examples
#'
#' library(TeamPhytoplankton)
#'
#' data(PI_data)
#'
#' fit <- nlsLoop(PI_data,
#'          GPP ~ Eilers_PI(Pmax, Iopt, a, I = light),
#'          tries = 10,
#'          id_col = 'temp',
#'          r2 = 'Y',
#'          supp.errors = 'Y',
#'          param_bds = c(0,20000, 0,1000, 0, 500))
#'
#' predictions <- predict.nlsLoop(PI_data, fit, id_col = 'temp', FUN = 'Eilers_PI', param_x = 'I', x = 'light', gap = 1)
#' @export predict.nlsLoop

predict.nlsLoop <- function(data, param_data, id_col, FUN, param_x, x, gap, ...){

  # create params_est
  all_params <- names(formals(FUN))
  params_est <- all_params[all_params %in% names(param_data)]

  # set id
  id <- unique(param_data[, id_col])

  # create empty predictions dataframe
  predictions.temp <- NULL

  for(i in 1:length(id)){

    tmp_results <- param_data[param_data[, id_col] == id[i],]
    tmp_parent <- data[data[, id_col] == id[i],]

    # loop append values for parameter values
    param_temps <- c()
    for(j in 1:length(params_est)){
      param_temps2 <- tmp_results[names(tmp_results) == params_est[j]]
      param_temps <- c(param_temps, param_temps2)
    }

    tmp_data <- data.frame(expand.grid(x = seq(min(tmp_parent[,x]), max(tmp_parent[,x]), gap), id = id[i]))

    colnames(tmp_data) <- c(param_x, id_col)

   x_temps <- c()
    for(k in 1:length(param_x)){
      x_temps2 <- tmp_data[names(tmp_data) == param_x]
      x_temps <- c(x_temps, x_temps2)
    }

    tmp_data$y <- do.call(FUN, c(param_temps, x_temps))
    predictions.temp <- rbind(predictions.temp, tmp_data)


  }

  colnames(predictions.temp)[colnames(predictions.temp) == param_x] <- x
  return(predictions.temp)

}
