#'@title calcDispUnitConversion
#'
#'@description Convert displacement in tonnages to cubic meters.
#'
#'@param tonnageDisp Displacement (vector of numericals, tonnage)
#'@param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can 
#'supply either a vector of numericals, a single number, or rely on the default
#'
#'@return \code{disp} (vector of numericals, m^3)
#'
#'@examples
#'calcDispUnitConversion(70000, 1.025)
#'
#'@export

calcDispUnitConversion<- function(tonnageDisp, seawaterDensity=1.025){

  disp<- tonnageDisp/seawaterDensity

  return(disp)
}
