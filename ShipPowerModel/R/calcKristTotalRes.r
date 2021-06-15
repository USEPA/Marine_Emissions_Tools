#' @title calcKristTotalRes
#'
#' @description Calculate total ship resistance (\code{Rtot}) (kN) using the
#' Kristensen method.
#'
#' @param wettedSA Wetted hull surface area (vector of numericals, m^2) (see
#' \code{\link{calcKristWettedSA}})
#' @param Cf Frictional resistance coefficient (vector of numericals,
#' dimensionless) (see \code{\link{calcCf}})
#' @param Cr Residual resistance coefficient (vector of numericals,
#' dimensionless) (see \code{\link{calcKristCr}})
#' @param Ca Incremental hull (roughness) resistance coefficient (vector of
#' numericals, dimensionless) (see \code{\link{calcKristCa}})
#' @param Caa Air resistance coefficient (vector of numericals, dimensionless)
#' (see \code{\link{calcKristCaa}})
#' @param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can
#' supply either a vector of numericals, a single number, or rely on the default
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see
#' \code{\link{calcSpeedUnitConversion}})
#' @param serviceMargin A service margin to account for weather and sea effects:
#' \itemize{\item Coastal operations = 10 \item At-sea operations = 15} Can
#' supply either a vector of numericals, a single number, or rely on the default
#'
#' @details
#' Note that service margin is included here as a resistance term.
#'
#' @return \code{Rtot} (vector of numericals, kN)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'@seealso \itemize{
#'\item \code{\link{calcCf}}
#'\item \code{\link{calcSpeedUnitConversion}}
#'}
#'
#' @family Kristensen Calculations
#' @family Resistance Calculations
#'
#' @examples
#' calcKristTotalRes(10714.62,0.0015,0.00043,6.46e-05,5e-05,1.025,10.8,15)
#'
#' @export

calcKristTotalRes<- function(wettedSA,Cf,Cr,Ca,Caa,seawaterDensity,shipSpeed,
                             serviceMargin){

  Rtot<- wettedSA*(Cf+Cr+Ca+Caa)*seawaterDensity*(shipSpeed^2)*0.5*(1+serviceMargin/100)
  return(Rtot)
}
