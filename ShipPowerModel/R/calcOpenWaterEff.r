#' @title calcOpenWaterEff
#'
#' @description Calculate the open water efficiency (\code{openWaterEff})
#' (dimensionless) using the Kristensen method with Harvald regressions.
#'
#' @param Rtot Total ship resistance (vector of numericals, kN) (see
#' \code{\link{calcKristTotalRes}} or \code{\link{calcHMTotalRes}})
#' @param thrustFactor Thrust deduction factor (vector of numericals,
#' dimensionless) (see \code{\link{calcKristThrustFactor}} or
#' \code{\link{calcHMThrustFactor}})
#' @param nProp Number of propellers (vector of numericals, see
#' \code{\link{calcPropNum}})
#' @param wakeFraction Wake fraction coefficient (vector of numericals,
#' dimensionless) (see \code{\link{calcKristWakeFrac}} or
#' \code{\link{calcHMWakeFraction}})
#' @param propDiam Propeller diameter (vector of numericals, m) (see
#' \code{\link{calcPropDia}})
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see
#' \code{\link{calcSpeedUnitConversion}})
#' @param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can
#' supply either a vector of numericals, a single number, or rely on the default
#'
#' @return \code{openWaterEff} (vector of numericals, dimensionless)
#'
#'@references
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'@seealso \itemize{
#'\item \code{\link{calcKristTotalRes}}
#'\item \code{\link{calcHMTotalRes}}
#'\item \code{\link{calcKristThrustFactor}}
#'\item \code{\link{calcHMThrustFactor}}
#'\item \code{\link{calcPropNum}}
#'\item \code{\link{calcKristWakeFrac}}
#'\item \code{\link{calcHMWakeFraction}}
#'\item \code{\link{calcPropDia}}
#'\item \code{\link{calcSpeedUnitConversion}}}
#'
#' @examples
#'  calcOpenWaterEff(398.487,0.1894947,1,0.3199536,6.7,0,seawaterDensity=1.025)
#'
#' @export



calcOpenWaterEff<-function(Rtot,thrustFactor,nProp,wakeFraction,propDiam,shipSpeed,seawaterDensity=1.025){


   cth<- (8/pi)*Rtot/(1-thrustFactor)/nProp/seawaterDensity/((1-wakeFraction)*shipSpeed*propDiam)^2


openWaterEff<- ifelse(shipSpeed==0,
                  0,
                  ((2/(1+sqrt((cth)+1)))*pmax(0.65, 0.81-0.014*(cth)))
)

return(openWaterEff)

}
