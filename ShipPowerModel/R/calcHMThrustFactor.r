#'@title calcHMThrustFactor
#'
#'@description Calculate thrust deduction factor (\code{thrustFactor})
#'(dimensionless) from the Holtrop & Mennen method.
#'
#'@param lwl Waterline length (vector of numericals, m) (see \code{\link{calclwl}})
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param maxDraft Maximum summer load line draft (vector of numericals, m)
#'@param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#'@param nProp Number of propellers (vector of numericals, see 
#' \code{\link{calcPropNum}})
#'@param Cp Prismatic coefficient (vector of numericals, dimensionless) 
#' (see \code{\link{calcCp}})
#'@param propDiam Propeller diameter (vector of numericals, m) (see 
#' \code{\link{calcPropDia}})
#'@param Cbw Waterline block coefficient (vector of numericals, dimensionless) (see
#' \code{\link{calcCbw}})
#'@param Cstern Afterbody form coefficient: \itemize{\item V-shaped Hull = -10
#'         \item U-Shaped Hull =  10
#'         \item Normal Hull = 0 (default)}
#' Can supply either a vector of numericals, a single number, or rely on the default
#'@param lcb Longitudinal position of center of buoyancy (vector of numericals, see 
#' \code{\link{calclcb}})
#'@param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can supply
#' either a vector of numericals, a single number, or rely on the default
#'
#'@details
#'Thrust deduction factor is a component of hull efficiency as well as a
#'component of propeller efficiency. It describes the increase in resistance on
#'the hull from water getting sucked back towards the propeller.
#'
#'Note: In "A Statistical Re-Analysis of Resistance and Propulsion Data", the
#'authors re-analyze with the inclusion of Series 64 hull forms for a total of
#'334 models included in the analysis. They suggest an update of the single
#'screw thrust equation but that the original equations should be used
#'for twin screw ships.
#'
#'Additionally, we are assuming conventional stern for all single screw ships.
#'Holtrop & Mennen also include an estimation for thrust factor for single screw
#'ships with open stern for fast sailing ships, but that is not included here.
#'
#'@return \code{thrustFactor} (vector of numericals, dimensionless)
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
#'\item \code{\link{calcPropNum}}
#'\item \code{\link{calcCp}}
#'\item \code{\link{calcPropDia}}
#'\item \code{\link{calcCbw}}
#'\item \code{\link{calclcb}}}
#'
#'@family Holtrop-Mennen Calculations
#'
#'@examples
#' calcHMThrustFactor(c(32.25,32.20),c(218.75,209.25),c(13.57,11.49),c(80097,52382.04),
#'                    c(1,1),c(0.81,0.67),c(6.7,7),c(0.81,0.65))
#' calcHMThrustFactor(32.25,218.75,13.57,80097,
#'                    1,0.81,6.7,0.81)
#'
#'@export

calcHMThrustFactor<- function(breadth,lwl,maxDraft,maxDisplacement,nProp,Cp,
                         propDiam,Cbw,Cstern=0,lcb=0,seawaterDensity=1.025){

  #================
  c10<- ifelse(lwl/breadth<5.2,#case 1
               0.25-(0.003328402/((breadth/lwl)-0.134615385))
  ,#case 2
    breadth/lwl
  )
  #================

  thrustFactor<- ifelse(nProp==1,#begin single screw estimation ([2] method)
    0.25014*((breadth/lwl)^0.28956)*
      ((sqrt(breadth*maxDraft)/maxDisplacement)^0.2624)/
    ((1-Cp+0.0225*lcb)^0.01762)+0.0015*Cstern

    #end single screw estimation
  ,#begin twin screw estimation ([1] method)
    0.325*Cbw-0.1885*propDiam/sqrt(breadth*maxDraft)

  )#end twin screw estimation


  return(thrustFactor)
}

