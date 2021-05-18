#' @title calcHMWettedSA
#'
#' @description Calculate hull wetted surface area (\code{wettedSA}) (m^2)
#' using the Holtrop & Mennen method.
#'
#'@param lwl Waterline length (vector of numericals, m) (see
#'\code{\link{calclwl}})
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param actualDraft Actual draft (vector of numericals, m)
#'@param Cm Midship section coefficient (vector of numericals, dimensionless)
#'(see \code{\link{calcCm}})
#'@param Cbw Waterline block coefficient (vector of numericals, dimensionless)
#'(see \code{\link{calcCbw}})
#'@param Cwp Water plane area coefficient (vector of numericals, see
#'\code{\link{calcCwp}})
#'@param Abt Traverse bulb area (vector of numericals, m^2) (see
#'\code{\link{calcAbt}})
#'
#'@details
#'Actual draft is typically obtained from sources such as AIS messages or ship
#'records.
#'
#' @return \code{wettedSA} (vector of numericals, m^2)
#'
#' @references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'@seealso \itemize{
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcCm}}
#'\item \code{\link{calcCbw}}
#'\item \code{\link{calcCwp}}
#'\item \code{\link{calcAbt}}}
#'
#' @family Holtrop-Mennen Calculations
#'
#' @examples
#'calcHMWettedSA(218.75,12.48,32.25,0.99,0.81,0.91,32.02)
#'
#' @export

calcHMWettedSA<- function(lwl,actualDraft,breadth,Cm,Cbw,Cwp,Abt){

wettedSA<-lwl*(2*actualDraft+breadth)*(Cm^0.5)*
    (0.453+0.4425*Cbw-0.2862*Cm-0.003467*(breadth/actualDraft)+
       0.3696*Cwp)+2.38*(Abt/Cbw)

    return(wettedSA)
}
