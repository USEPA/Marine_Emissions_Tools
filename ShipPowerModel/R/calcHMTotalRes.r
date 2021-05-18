#'@title calcHMTotalRes
#'
#' @description Calculate total ship resistance (\code{Rtot}) (kN) using the
#' Holtrop & Mennen method.
#'
#' @param Rapp Appendage resistance (vector of numericals, kN) (see 
#' \code{\link{calcHMAppendageRes}})
#' @param Rw Wave waking (similar to residual) resistance (vector of numericals, kN)
#' (see \code{\link{calcHMWaveMakingRes}})
#' @param Rb Bulbous bow resistance (vector of numericals, kN) (see 
#' \code{\link{calcHMBulbousBowRes}})
#' @param Rtr Immersed transom resistance (vector of numericals, kN) (see 
#' \code{\link{calcHMImmersedTransomRes}})
#' @param seawaterDensity Sea water density. Default = 1.025 (g/cm^2). Can 
#' supply either a vector of numericals, a single number, or rely on the default
#' @param wettedSA Wetted hull surface area (vector of numericals, m^2) (see 
#' \code{\link{calcHMWettedSA}})
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see 
#' \code{\link{calcSpeedUnitConversion}})
#' @param Cf Frictional resistance coefficient (vector of numericals, dimensionless) 
#' (see \code{\link{calcCf}})
#' @param formFactor Form factor (1+k) (vector of numericals, dimensionless)
#' (see \code{\link{calcHMFormFactor}})
#' @param Ca Incremental hull (roughness) resistance coefficient (vector of 
#' numericals, dimensionless) (see \code{\link{calcHMCa}})
#' @param serviceMargin A service margin to account for weather and sea effects:
#' \itemize{\item Coastal operations = 10 \item At-sea operations = 15 (default)}.
#' Can supply either a vector of numericals, a single number, or rely on the default
#'
#' @details
#' Note that service margin is included here as a resistance term.
#'
#' @return \code{Rtot} (vector of numericals, kN)
#'
#' @references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#' @seealso \code{\link{calcSpeedUnitConversion}}
#'
#' @family Holtrop-Mennen Calculations
#' @family Resistance Calculations
#'
#' @examples
#' calcHMTotalRes(Rapp=0.24,
#'                Rw=2.6,
#'                Rb=c(4.538188e-07,3.579217e-06,1.180116e-05,2.709520e-05,5.085940e-05),
#'                Rtr=c(0.58,2.2,4.8,8.1,12.1),
#'                seawaterDensity=1.025,
#'                wettedSA=10746.282,
#'                shipSpeed=seq(1,5,1),
#'                Cf=c(0.0019,0.0017,0.0016,0.0016,0.0015),
#'                formFactor=1.275601,
#'                Ca=0.0003356088,
#'                serviceMargin=15)
#'
#' @export

calcHMTotalRes<- function(Rapp,Rw,Rb,Rtr,seawaterDensity,wettedSA,shipSpeed,
                          Cf,formFactor,Ca,serviceMargin){
  Rtot<-
(    Rapp+Rw+Rb+Rtr+
    (.5*seawaterDensity*wettedSA*((shipSpeed)^2)*Cf)*(formFactor)+
    (.5*seawaterDensity*wettedSA*((shipSpeed)^2)*Ca))*(1+serviceMargin/100)
  return(Rtot)
}

