#'@title calcSpeedUnitConversion
#'
#'@description Convert speed in knots to m/s.
#'
#'@param shipSpeed Ship speed (vector of numericals, knots)
#'
#'@return speed (vector of numericals, m/s)
#'
#'@examples
#'calcSpeedUnitConversion(seq(10,15,1))
#'
#'@export

calcSpeedUnitConversion<- function(shipSpeed){

  speed<- shipSpeed*0.5144

  return(speed)
}
