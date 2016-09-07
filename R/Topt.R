#' Calculates the optimum temperature
#'
#' Calculates the optimum temperature from the parameters of the modified
#' Sharpe-Schoolfield equation. Used in tandem with schoolfield.high
#'
#' @param Eh The value of the deactivation energy
#' @param Th The value of Th being the temperature at which the substrate is
#' half high temperature suppressed (in K)
#' @param Ea The activation energy
#' @param K Whether or not you want the results in K or in degrees centigrade.
#' Defaults to K. 'Y' gives K and 'N' gives degrees celsius
#' @return value of optimum temperature
#' @author Daniel Padfield
#' @examples
#' Topt(4, 315, 0.6)
#' @export
Topt <- function(Eh, Th, Ea, K = c('Y', 'N')){
  if(missing(K)) return((Eh*Th)/(Eh + (8.62e-05 *Th*log((Eh/Ea) - 1))))
  if(K == 'Y') return((Eh*Th)/(Eh + (8.62e-05 *Th*log((Eh/Ea) - 1))))
  if(K =='N') return(((Eh*Th)/(Eh + (8.62e-05 *Th*log((Eh/Ea) - 1)))) - 273.15)
}

