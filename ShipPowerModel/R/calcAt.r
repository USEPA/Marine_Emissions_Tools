#' @title calcAt
#'
#' @description Estimate transom area (\code{At}) (m^2) using method in Rakke
#' (2016).
#'
#' @param Cm Midship area coefficient (vector of numericals, dimensionless) 
#' (see \code{\link{calcCm}})
#' @param breadth Moulded breadth (vector of numericals, m)
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#'
#' @return \code{At}
#'
#' @references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'\href{http://hdl.handle.net/11250/2410741}{Rakke, S. G. 2016. "Ship Emissions
#'Calculation from AIS." NTNU}
#'
#' @seealso \code{\link{calcCm}}
#'
#' @examples
#'calcAt(0.98, 32, 10)
#'calcAt(c(0.98,0.99), c(32,45.5), c(10,15.5))
#'
#' @export

calcAt <-function(Cm, breadth, maxDraft){

  At<- 0.051*Cm*breadth*maxDraft

  return(At)
}
