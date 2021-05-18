#' @title calcCwp
#'
#' @description Calculates the water plane area coefficient (\code{Cwp})
#' (dimensionless) using one of two methods (Kristensen or Schneekluth).
#'
#' @param Cbw Waterline block coefficient (vector of numericals, 
#' dimensionless) (see \code{\link{calcCbw}})
#' @param CwpEquationType Indicates equation type:
#' \itemize{\item"kristensen"\item"schneekluth"}
#' This argument is not vectorized, as it takes only a single string
#'
#' @details
#'Kristensen:
#'\deqn{Cwp = 0.55+0.45Cbw}
#'
#'Schneekluth:
#'\deqn{Cwp = \frac{1+2Cbw}{3}}{Cwp = (1+2Cbw)/3}
#'
#' @return \code{Cwp} (vector of numericals, dimensionless)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2012. "Prediction of Resistance and Propulsion
#'Power of Ships. Clean Shipping Currents, 1 (6).
#'
#'Schneekluth, H. and Bertram, V. 1998. "Ship Design for Efficiency and Economy."
#'2nd ed. Oxford, Boston: Butterworth-Heinemann.
#'
#' @seealso \code{\link{calcCbw}}
#'
#' @examples
#' calcCwp(c(0.81,0.65),"kristensen")
#'
#' @export

calcCwp <- function(Cbw, CwpEquationType){

  if(grepl("kristensen", tolower(CwpEquationType))){
    Cwp<-0.55+0.45*Cbw
  }else{
    Cwp<-(1+2*Cbw)/3
  }


  return(Cwp)

}
