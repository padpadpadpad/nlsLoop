#' Calculate the confidence intervals from an nlsLoop object
#'
#' Determines the confidence intervals of a set of non-linear regressions fitted with nlsLoop
#'
#' @param data the data frame containing the data used in the nlsLoop argument
#' @param param_data the nlsLoop object
#' @return returns a dataframe in stacked format with columns \code{id_col}, \code{param}, \code{CI_lwr}, \code{CI_higher}, \code{mean} that give the confidence intervals and mean estimate of each model fit.
#' @author Daniel Padfield
#' @details calculates the confidence intervals of each fit using \code{\link[nlstools]{confint2}} and the "asymptotic" approach. This method cannot be changed to "profile" as it the model is fitted with \code{\link[minpack.lm]{nlsLM}}
#' @seealso \code{\link[nlsLoop]{nlsLoop}} for details of fitting the initial nlsLoop object.
#'
#' \code{\link[nlstools]{confint2}} for details of calculating CI from non-linear regressions
#'
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
#'                 tries = 100,
#'                 id_col = 'curve_id',
#'                 param_bds = c(-10, 10, 0.1, 2, 0.5, 5, 285, 330),
#'                 lower = c(lnc=-10, E=0, Eh=0, Th=0))
#'
#' CI <- confint_nlsLoop(Chlorella_TRC_test, fits)
#'
#' @export

# function to get confidence intervals
confint_nlsLoop <- function(data, param_data){

  # set up internal arguments
  id_col <- unique(param_data$info$id_col)
  ids <- unique(data[, id_col])

  calculate_confint <- function(x, .param_data = param_data, .data = data){

    formula = .param_data$formula
    id_col <- unique(.param_data$info$id_col)

    dat <- .data[.data[,id_col] == x,]
    params2 <- .param_data$params[.param_data$params[, id_col] == x,]
    fit = NULL
    try(fit <- minpack.lm::nlsLM(formula,
                                 data = dat,
                                 start = params2[colnames(params2) %in% all.vars(formula[[3]])]), silent = TRUE)
    if(!is.null(fit)){
      confint <- nlstools::confint2(fit, ...)
      confint <- data.frame(confint)
      confint$param <- row.names(confint)
      colnames(confint)[1:2] <- c('CI_lwr', 'CI_upr')
      confint$mean <- unlist(stats::coef(fit))
      confint[,id_col] <- x
      rownames(confint) <- NULL
      confint <- confint[,c(5, 4, 3, 1, 2)]
      return(confint)
    }
  }

  temp <- plyr::ldply(ids, calculate_confint)
  return(temp)
}
