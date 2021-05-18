#' @title calcHMImmersedTransomRes
#'
#' @description
#' Calculate additional pressure resistance due to immersed transom (\code{Rtr})
#' (kN).
#'
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see
#'  \code{\link{calcSpeedUnitConversion}})
#' @param breadth Moulded breadth (vector of numericals, m)
#' @param Cwp Water plane area coefficient (vector of numericals, see 
#'  \code{\link{calcCwp}})
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param At Transom area (vector of numericals, m^2) (see \code{\link{calcAt}})
#' @param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can 
#' supply either a vector of numericals, a single number, or rely on the default
#'
#' @return \code{Rtr} (vector of numericals, kN)
#'
#' @references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'@seealso \itemize{
#'\item \code{\link{calcSpeedUnitConversion}}
#'\item \code{\link{calcCwp}}
#'\item \code{\link{calcAt}} }
#'
#' @family Holtrop-Mennen Calculations
#' @family Resistance Calculations
#'
#' @examples
#' calcHMImmersedTransomRes(seq(1,5,1),32.25,0.91,13.57,22.2,seawaterDensity=1.025)
#'
#' @export

calcHMImmersedTransomRes<-function(shipSpeed,breadth,Cwp,maxDraft,At,seawaterDensity=1.025){


Rtr<-
ifelse(shipSpeed/sqrt(2*9.81*At/(breadth+breadth*Cwp))<5,#case 1
  0.5*seawaterDensity*(shipSpeed^2)*At*(0.2*(1-0.2*(
    shipSpeed/sqrt(2*9.81*At/(breadth+breadth*Cwp))
    )
    ))
,#case 2
0)

return(Rtr)
}
