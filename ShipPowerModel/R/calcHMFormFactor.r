#'@title calcHMFormFactor
#'
#'@description Calculate form factor (1+k_1) from the Holtrop & Mennen method.
#'
#'@param maxDraft Maximum summer load line draft (vector of numericals, m)
#'@param lwl Waterline length (vector of numericals, m) (see \code{\link{calclwl}})
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#'@param Cp Prismatic coefficient (vector of numericals, dimensionless) (see 
#' \code{\link{calcCp}})
#'@param Cstern Afterbody form coefficient:
#'\itemize{\item V-shaped Hull = -10
#'         \item U-Shaped Hull =  10
#'         \item Normal Hull = 0 (default) }
#' Can supply either a vector of numericals, a single number, or rely on the default
#'@param lcb Longitudinal position of center of buoyancy (vector of numericals,
#' see \code{\link{calclcb}})
#'
#'@return \code{formFactor} (vector of numericals)
#'
#'@references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'Holtrop, J. and Mennen, G. G. J. 1984. "A Statistical Re-Analysis of Resistance
#'and Propulsion Data'.
#'
#'@seealso \itemize{
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcCp}}
#'\item \code{\link{calclcb}} }
#'
#'@family Holtrop-Mennen Calculations
#'
#'@examples
#' calcHMFormFactor(c(13.57,11.49),c(218.75, 209.25),c(32.25,32.20),c(80097,52382.04),c(0.81,0.67))
#' calcHMFormFactor(13.57,218.75,32.25,80097,0.81)
#'
#'@export

calcHMFormFactor<-function(maxDraft,lwl,breadth,maxDisplacement,Cp,Cstern=0,lcb=0){

  formFactor<-
    0.93+0.487118*
    #c14
    (1+0.011*Cstern)*
    ((breadth/lwl)^1.06806)*
    ((maxDraft/lwl)^0.46106)*
     (  #L/Lr
       (1/(1-Cp+(0.06*Cp*-lcb)/(4*Cp-1))
        )^0.121563)*
    ((lwl^3/maxDisplacement)^0.36486)*((1-Cp)^-0.604247)

return(formFactor)

}

