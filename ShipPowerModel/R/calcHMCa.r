#' @title calcHMCa
#'
#'@description Calculate incremental hull (roughness) resistance coefficient
#'(\code{Ca}) (dimensionless), which describes the effect of hull roughness and
#' still-air resistance, using the Holtrop & Mennen method.
#'
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param lwl Waterline length (vector of numericals, m) (see \code{\link{calclwl}})
#' @param Cbw Waterline block coefficient (vector of numericals, dimensionless) 
#'  (see \code{\link{calcCbw}})
#' @param breadth Moulded breadth (vector of numericals, m)
#' @param forwardDraft Forward draft (deviation from actual draft indicates trim)
#'  (vector of numericals, m)
#' @param Abt Traverse bulb area (vector of numericals, m^2) (see 
#'  \code{\link{calcAbt}})
#' @param hb Center of bulb area above keel line (vector of numericals, m) (see 
#'  \code{\link{calchb}})
#'
#' @return \code{Ca} (vector of numericals, dimensionless)
#'
#' @references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'@seealso \itemize{
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcCbw}}
#'\item \code{\link{calcAbt}}
#'\item \code{\link{calchb}}
#'}
#'
#' @family Holtrop-Mennen Calculations
#' @family Resistance Calculations
#'
#' @examples
#' calcHMCa(c(13.57,11.49),c(218.7500,209.2518),c(0.81,0.65),c(32.25,32.20),c(13.57,11.49),
#' c(25.09,55.86),c(5.43,4.6))
#'
#'calcHMCa(13.57,218.7500,0.81,32.25,13.57,25.0880,5.428)
#'
#' @export

calcHMCa<-function(maxDraft,lwl,Cbw,breadth,forwardDraft,Abt,hb){

Ca<-
  ifelse(forwardDraft/lwl<=0.04,#case 1
  0.006*((lwl+100)^-0.16)-0.00205+0.003*sqrt(lwl/7.5)*Cbw^4*
  (#c2
    exp(-1.89*sqrt(
    #c3
    0.56*Abt^1.5/(breadth*maxDraft*(
      0.31*sqrt(Abt)+forwardDraft-hb
    ))
  ))
  )*
  (0.04-
     (#c4
      forwardDraft/lwl
     ))
 ,#case 2
    0.006*((lwl+100)^-0.16)-0.00205+0.003*sqrt(lwl/7.5)*Cbw^4*
      (#c2
        exp(-1.89*sqrt(
          #c3
          0.56*Abt^1.5/(breadth*maxDraft*(
            0.31*sqrt(Abt)+forwardDraft-hb
          ))
        ))
      )*
      (0.04-
         (#c4
           0.04
         ))
  )

return(Ca)
}
