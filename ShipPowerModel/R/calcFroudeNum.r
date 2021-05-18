#' @title calcFroudeNum
#'
#' @description Calculate the Froude number (\code{froudeNum}) (dimensionless).
#'
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see 
#'   \code{\link{calcSpeedUnitConversion}})
#' @param lwl Waterline length (vector of numericals, m) (see 
#'   \code{\link{calclwl}})
#'
#' @details
#' The Froude number is a dimensionless coefficient that relates a vessel's speed
#' and length to a relative resistance. The equation for the Froude number is:
#' \deqn{Fn=\frac{V}{\sqrt{g*Lwl}}}{Fn=V/sqrt(g*lwl)}
#' Where V is the ship's speed, g is the gravitational constant (9.81 m/s^2),
#' and \code{lwl} is the waterline length (m) of the vessel.
#'
#' @return \code{froudeNum} (vector of numericals, dimensionless)
#'
#' @references
#' \href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#' Solutions. 2011. "Basic Principles of Propulsion."}
#'
#' @seealso \itemize{
#' \item \code{\link{calcSpeedUnitConversion}}
#' \item \code{\link{calclwl}}
#' }
#'
#' @examples
#' calcFroudeNum(seq(1,5,1),214)
#'
#' @export

calcFroudeNum <- function(shipSpeed, lwl){

  froudeNum<-(shipSpeed)/((9.81*lwl)^0.5)

  return(froudeNum)
}
