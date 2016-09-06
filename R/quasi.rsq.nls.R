############################## quasi r-squared nls ###############################



#' R squared statistic for non linear models
#'
#' Calculates a quasi-rsquared value for non linear least squares regression
#' models. The formula for calculating this is given in the paper in the
#' references (Spiess 2010).
#'
#' Cautionary notes of whether its useful are provided in the stack overflow
#' reference. A personal highlight of which is 'If someone asks for rope to
#' hang themselves, its fine to give it to them (while performing due diligence
#' by asking "are you sure you want to do this?")'
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
#'
#' @param mdl model object from which R^2 is desired. Used to calculate the
#' residual sum of squares.
#' @param y the dependent variable in the model.
#' @param param the number of parameters of the non-linear model
#' @return value of r squared for specific non-linear model object
#' @author Daniel Padfield
#' @references Spiess, A.-N. & Neumeyer, N. (2010) An evaluation of R2 as an
#' inadequate measure for nonlinear models in pharmacological and biochemical
#' research: a Monte Carlo approach. BMC pharmacology, 10, 6.
#'
#' http://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-model
#' @examples
#'
#'
#' @export
quasi.rsq.nls <- function(mdl, y, param){
  adj <- (sum(!is.na(y)) - 1)/(sum(!is.na(y)) - param)
  sum.sq <- (sum(!is.na(y)) - 1)*stats::var(y, na.rm = TRUE)
  rsq <- 1 - (stats::deviance(mdl)/sum.sq)
  rsq.adj <- 1 - adj*(1 - rsq)
  return(rsq.adj)

}
