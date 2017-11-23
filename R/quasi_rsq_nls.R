#' R squared statistic for non linear models
#'
#' Calculates a quasi-rsquared value for non linear least squares regression
#' models. The formula for calculating this is given in the paper in the
#' references (Spiess 2010).
#'
#' @param mdl model object from which R^2 is desired. Used to calculate the
#' residual sum of squares.
#' @param y a vector of the response variable from the model.
#' @param param the number of parameters of the non-linear model
#' @return value of r squared for specific non-linear model object
#' @details cautionary notes of whether its useful are provided in \href{http://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-model}{this} StackOverflow discussion reference. A personal highlight of which is 'If someone asks for rope to hang themselves, its fine to give it to them (while performing due diligence by asking "are you sure you want to do this?")'
#'
#' Calculated as :
#'
#' 1 - (n-1)/(n-param) * (1 - R^2)
#'
#' where R^2 = 1 - RSS/TSS
#'
#' RSS is the residual sum of squares given by deviance(mdl)
#'
#' TSS is given by how much variance there is in the dependent variable:
#'
#' (n - 1)*var(y)
#' @author Daniel Padfield
#' @references Spiess, A.-N. & Neumeyer, N. (2010) An evaluation of R2 as an
#' inadequate measure for nonlinear models in pharmacological and biochemical
#' research: a Monte Carlo approach. BMC pharmacology, 10, 6.
#'
#' \href{http://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-model}{A StackOverflow discussion}
#' @examples
#' data(Chlorella_TRC)
#'
#' # define the Sharpe-Schoolfield equation
#' schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
#'  Tc <- 273.15 + Tc
#'  k <- 8.62e-5
#'  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
#'  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
#'  return(boltzmann.term + inactivation.term)
#'}
#'
#' fit <- minpack.lm::nlsLM(ln.rate ~ schoolfield_high(lnc, E, Eh, Th, temp = K, Tc = 20),
#'                 data = Chlorella_TRC[Chlorella_TRC$curve_id == 1,],
#'                 start = c(lnc = -1.2, E = 0.95, Eh = 5, Th = 315))
#'
#' quasi_rsq_nls(fit, Chlorella_TRC[Chlorella_TRC$curve_id == 1,]$ln.rate, 4)
#'
#' 0.4608054
#' @export
quasi_rsq_nls <- function(mdl, y, param){
  adj <- (sum(!is.na(y)) - 1)/(sum(!is.na(y)) - param)
  sum.sq <- (sum(!is.na(y)) - 1)*stats::var(y, na.rm = TRUE)
  rsq <- 1 - (stats::deviance(mdl)/sum.sq)
  rsq.adj <- 1 - adj*(1 - rsq)
  return(rsq.adj)

}
