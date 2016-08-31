######## Function for deriving Topt #########################



#' Calculates the optimum temperature
#' 
#' Calculates the optimum temperature from the parameters of the modified
#' Sharpe-Schoolfield equation. Used in tandem with schoolfield.high
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param Eh The value of the deactivation energy
#' @param Th The value of Th being the temperature at which the substrate is
#' half high temperature suppressed (in K)
#' @param Ea The activation energy
#' @param K Whether or not you want the results in K or in degrees centigrade.
#' Defaults to K. 'Y' gives K and 'N' gives degrees celsius
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author Daniel Padfield
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 
#' ## example
#' 
#' Topt(4, 315, 0.6)
#' 312.6682
#' 
#' Topt(4, 315, 0.6, 'N')
#' 39.51823
#' 
#' 312.6682 - 273.15 = 39.5182
#' 
#' 
#' ###############################################################################
#' ## The function is currently defined as
#' function (Eh, Th, Ea, K = c("Y", "N")) 
#' {
#'     if (missing(K)) 
#'         return(Eh * (1/((Eh/Th) + (8.62e-05 * log(Eh - Ea/Ea)))))
#'     if (K == "Y") 
#'         return(Eh * (1/((Eh/Th) + (8.62e-05 * log(Eh - Ea/Ea)))))
#'     if (K == "N") 
#'         return((Eh * (1/((Eh/Th) + (8.62e-05 * log(Eh - Ea/Ea))))) - 
#'             273.15)
#'   }
#' 
#' @export Topt
Topt <- function(Eh, Th, Ea, K = c('Y', 'N')){
  if(missing(K)) return((Eh*Th)/(Eh + (8.62e-05 *Th*log((Eh/Ea) - 1))))
  if(K == 'Y') return((Eh*Th)/(Eh + (8.62e-05 *Th*log((Eh/Ea) - 1))))
  if(K =='N') return(((Eh*Th)/(Eh + (8.62e-05 *Th*log((Eh/Ea) - 1)))) - 273.15)
}

################ example

Topt(4, 315, 0.6)
