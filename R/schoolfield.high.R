#' Performs the modified Sharpe-Schoolfield equation for high temperature
#' inactivation
#'
#' Models the thermal performance curve for metabolism using a modified thermal
#' performance curve.
#'
#' @param ln.c The specific rate of metabolism (log scale)
#' @param Ea activation energy (eV)
#' @param Eh high temperature de-activation energy (eV)
#' @param Th temperature at which enzyme is 1/2 active and 1/2 High-T suppressed
#' @param temp temperature in K
#' @param Tc standardisation temperature (in degrees centigrade, arbitrary)
#' @author Daniel Padfield
#' @references Schoolfield, R. M., Sharpe, P. J. & Magnuson, C. E. Non-linear regression of biological temperature-dependent rate models based on absolute reaction-rate theory. J. Theor. Biol. 88, 719-731 (1981)
#' @export

schoolfield.high <- function(ln.c,Ea,Eh,Th,temp, Tc) {

Tc <- 273.15 + Tc
k <- 8.62e-5
boltzmann.term <- ln.c + log(exp(Ea/k*(1/Tc - 1/temp)))
inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))

return(boltzmann.term + inactivation.term)

}

