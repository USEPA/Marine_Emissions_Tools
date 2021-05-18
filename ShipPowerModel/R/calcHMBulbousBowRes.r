#'@title calcHMBulbousBowRes
#'
#'@description Calculate bulbous bow resistance (\code{Rb}) (kN) from the
#'Holtrop & Mennen method.
#'
#'@param maxDraft Maximum summer load line draft (vector of numericals, m)
#'@param forwardDraft Forward draft (deviation from actual draft indicates trim) 
#' (vector of numericals, m)
#'@param shipSpeed Ship actual speed (vector of numericals, m/s) (see 
#' \code{\link{calcSpeedUnitConversion}})
#'@param Abt Traverse bulb area (vector of numericals, m^2) (see 
#' \code{\link{calcAbt}})
#'@param hb Center of bulb area above keel line (vector of numericals, m) 
#' (see \code{\link{calchb}})
#'@param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can 
#' supply either a vector of numericals, a single number, or rely on the default
#'
#'@return \code{Rb} (vector of numericals, kN)
#'
#'@references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'@seealso \itemize{
#'\item \code{\link{calcSpeedUnitConversion}}
#'\item \code{\link{calcAbt}}
#'\item \code{\link{calchb}}
#'}
#'
#'@family Holtrop-Mennen Calculations
#'@family Resistance Calculations
#'
#'@examples
#' calcHMBulbousBowRes(seq(1,5,1),13.57,13.57,25.09,5.43,seawaterDensity=1.025)
#'
#'@export

calcHMBulbousBowRes<-function(shipSpeed,maxDraft,forwardDraft,
                            Abt,hb,seawaterDensity=1.025){
Rb<-
  0.11*exp(-3*((
  (0.56*sqrt(Abt)/(forwardDraft-1.5*hb))
)^-2))*((
  (shipSpeed)/sqrt(9.81*(forwardDraft-hb-0.25*sqrt(Abt))+0.15*((shipSpeed)^2))
)^3)*(Abt^1.5)*seawaterDensity*9.81/(1+((
  (shipSpeed)/sqrt(9.81*(forwardDraft-hb-0.25*sqrt(Abt))+0.15*((shipSpeed)^2))
)^2))


return(Rb)
}



