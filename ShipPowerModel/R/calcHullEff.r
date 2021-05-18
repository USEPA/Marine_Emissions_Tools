#' @title calcHullEff
#'
#' @description Calculate the hull efficiency (\code{hullEff}) (dimensionless)
#' for a specified vessel.
#'
#' @param thrustFactor Thrust deduction coefficient (vector of numericals, dimensionless)
#' (see \code{\link{calcHMThrustFactor}} and \code{\link{calcKristThrustFactor}})
#' @param wakeFraction Wake fraction coefficient (vector of numericals, dimensionless)
#' (see \code{\link{calcHMWakeFraction}} and \code{\link{calcKristWakeFrac}})
#'
#'@seealso \itemize{
#'\item \code{\link{calcHMThrustFactor}}
#'\item \code{\link{calcKristThrustFactor}}
#'\item \code{\link{calcHMWakeFraction}}
#'\item \code{\link{calcKristWakeFrac}}}
#'
#' @return \code{hullEff} (vector of numericals, dimensionless)
#'
#' @examples
#' calcHullEff(c(0.19,0.17), c(0.32,0.29))
#'
#' @export

calcHullEff <- function(thrustFactor, wakeFraction ){

  hullEff<- (1-thrustFactor)/(1-wakeFraction)


  return(hullEff)
}
