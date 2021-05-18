#'@title calcAbt
#'
#'@description Estimate transverse bulb area (\code{Abt}) (m^2) using method in
#'Rakke (2016).
#'
#'@param Cm Midship area coefficient (dimensionless) (see \code{\link{calcCm}})
#'@param breadth Moulded breadth (m)
#'@param actualDraft Actual draft (m)
#'
#'@details
#'Transverse sectional area of the bulb at the position where the still-water
#'surface intersects the stem (m^2) using actual draft. Actual draft is
#'typically obtained from sources such as AIS messages or ship records.
#'
#'@return \code{Abt} (vector of numericals, m^2)
#'
#'@references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29. p. 166-170
#'
#'\href{http://hdl.handle.net/11250/2410741}{Rakke, S. G. 2016. "Ship
#'Emissions Calculation from AIS." NTNU}
#'
#'@seealso \code{\link{calcCm}}
#'
#'@examples
#'calcAbt(0.98, 32, 10)
#'calcAbt(c(0.98,0.99), c(32,45.5), c(10,15.5))
#'
#'@export

calcAbt <-function(Cm, breadth, actualDraft){

  Abt<-0.08*Cm*breadth*actualDraft

  return(Abt)
}
