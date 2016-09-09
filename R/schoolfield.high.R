#' Performs the modified Sharpe-Schoolfield equation for high temperature
#' inactivation
#'
#' Models the thermal performance curve for metabolism using a modified thermal
#' performance curve.
#'
#' @param ln.c the specific rate of metabolism (log scale).
#' @param Ea activation energy (eV)
#' @param Eh high temperature de-activation energy (eV)
#' @param Th temperature at which enzyme is 1/2 active and 1/2 suppressed due to high temperatures
#' @param temp temperature in K
#' @param Tc standardisation temperature (in degrees centigrade, arbitrary)
#' @author Daniel Padfield
#' @references Schoolfield, R. M., Sharpe, P. J. & Magnuson, C. E. Non-linear regression of biological temperature-dependent rate models based on absolute reaction-rate theory. J. Theor. Biol. 88, 719-731 (1981)
#' @seealso a Shiny app showing how the parameters of the schoolfield high and full schoolfield model incorporating low temperature inactivation control the shape of the curve is available online \href{https://padpadpadpad.shinyapps.io/Sharpe-Schoolfield/}{here}
#' @examples
#' temp <- seq(273.15, 333.15, 1)
#'
#' plot(schoolfield.high(ln.c = -1, Ea = 0.6, Eh = 2, Th = 320, temp = temp, Tc = 20))
#'
#' @export

schoolfield.high <- function(ln.c,Ea,Eh,Th,temp, Tc) {

Tc <- 273.15 + Tc
k <- 8.62e-5
boltzmann.term <- ln.c + log(exp(Ea/k*(1/Tc - 1/temp)))
inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))

return(boltzmann.term + inactivation.term)

}

