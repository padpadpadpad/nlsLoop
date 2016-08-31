##################################################
#Schoolfield approximation - high temperature inactivation only


#' Performs the modified Sharpe-Schoolfield equation for high temperature
#' inactivation
#' 
#' Models the thermal performance curve for metabolism using a modified thermal
#' performance curve.
#' 
#' 
#' @param ln.c The specific rate of metabolism (log scale)
#' @param Ea Activation energy
#' @param Eh High temperare De-Activation energy
#' @param Th T at which enzyme is 1/2 active and 1/2 High-T suppressed
#' @param temp temperature in K
#' @param Tc standardisation temperature (in ºC, arbitrary)
#' @author Daniel Padfield
#' @seealso
#' 
#' \code{\link{schoolfield}} for applying the high and low temperature
#' inactivation modification of the Sharpe-Schoolfield model.
#' 
#' \code{\link{Topt}} for calculating the optimum temperature of the
#' Sharpe-Schoolfield model
#' 
#' \code{\link{SH.Tmin}} and \code{\link{SH.Tmax}} for empirically calculating
#' the thermal minima and maxima.
#' @examples
#' 
#' 
#' 
#' @export schoolfield.high
schoolfield.high <- function(ln.c,Ea,Eh,Th,temp, Tc) {

#constants
Tc <- 273.15 + Tc #standardization temperature (in K, arbitrary)
k <- 8.62e-5 #Boltzmann's constant; units imply Ea, El, and Eh are in eV

#ln.c normalisation constant (log scale)
#Ea Apparent Activation energy 
#Eh High temperare De-Activation energy
#Th T at which enzyme is 1/2 active and 1/2 High-T suppressed

#expression for flux is the product of two terms below
boltzmann.term <- ln.c + log(exp(Ea/k*(1/Tc - 1/temp)))  #Boltzmann term
inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp)))) 

#predicted flux (log scale)
return(boltzmann.term + inactivation.term)

}

