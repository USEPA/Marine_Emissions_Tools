#'@title calcHMWakeFraction
#'
#'@description Calculate wake fraction (\code{wakeFraction}) (dimensionless)
#'using the Holtrop & Mennen method.
#'
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param wettedSA Wetted hull surface area (vector of numericals, m^2) (see 
#' \code{\link{calcHMWettedSA}})
#'@param maxDraft Maximum summer load line draft (vector of numericals, m)
#'@param nProp Number of propellers (vector of numericals, see 
#' \code{\link{calcPropNum}})
#'@param lwl Waterline Length (vector of numericals, m) (see \code{\link{calclwl}})
#'@param propDiam Propeller diameter (vector of numericals, m) (see 
#' \code{\link{calcPropDia}})
#'@param formFactor Form factor (1+k) (vector of numericals, dimensionless) (see 
#' \code{\link{calcHMFormFactor}})
#'@param Cf Frictional resistance coefficient (vector of numericals, dimensionless)
#' (see \code{\link{calcCf}})
#'@param Ca Incremental hull (roughness) resistance coefficient (vector of 
#' numericals, dimensionless) (see \code{\link{calcHMCa}})
#'@param Cbw Waterline block coefficient (vector of numericals, dimensionless) 
#' (see \code{\link{calcCbw}})
#'@param Cp Prismatic coefficient (vector of numericals, dimensionless) (see 
#' \code{\link{calcCp}})
#'@param Cm Midship section coefficient (vector of numericals, dimensionless) 
#' (see \code{\link{calcCm}})
#'@param aftDraft Aft draft (deviation from actual draft indicates trim) 
#' (vector of numericals, m)
#'@param Cstern Afterbody form coefficient: \itemize{\item V-shaped Hull = -10
#'         \item U-Shaped Hull =  10
#'         \item Normal Hull = 0 (default)}
#' Can supply either a vector of numericals, a single number, or rely on the default
#'@param lcb Longitudinal position of center of buoyancy (vector of numericals, 
#' see \code{\link{calclcb}})
#'
#'@details
#' "The speed of advance of the propeller relative to the water in which it is working is
#'  lower than the observed speed of the vessel. This difference in speed,
#'  expressed as a percentage of the ship speed, is known as the wake fraction coefficient".
#'  \url{https://www.wartsila.com/encyclopedia/term/wake-fraction-coefficient}
#'
#'Wake fraction is a component of hull efficiency as well as a component of
#'propeller efficiency.
#'
#'Actual draft is typically obtained from sources such as AIS messages or ship
#'records.
#'
#'Note: In "A Statistical Re-Analysis of Resistance and Propulsion Data", the
#'authors re-analyze with the inclusion of Series 64 hull forms for a total of
#'334 models included in the analysis. They suggest an update of the single
#'screw wake fraction equation but that the original equations should be used
#'for twin screw ships.
#'
#'Viscous resistance coefficient: \code{Cv = (1+k) * Cf + Ca}
#'
#'We are assuming here that \code{1+k = 1+k_1} from \code{\link{calcHMFormFactor}}.
#'
#'Additionally, we are assuming conventional stern for all single screw ships.
#'Holtrop & Mennen also include an estimation for wake fraction for single screw
#'ships with open stern for fast sailing ships, but that is not included here.
#'
#'@return \code{wakeFraction} (vector of numericals, dimensionless)
#'
#'@references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'Holtrop, J. and Mennen, G. G. J. 1984. "A Statistical Re-Analysis of Resistance
#'and Propulsion Data'.
#'
#'@seealso \itemize{
#'\item \code{\link{calcPropNum}}
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcPropDia}}
#'\item \code{\link{calcCf}}
#'\item \code{\link{calcCbw}}
#'\item \code{\link{calcCp}}
#'\item \code{\link{calcCm}}
#'\item \code{\link{calclcb}}
#'}
#'
#'@family Holtrop-Mennen Calculations
#'
#'@examples
#' calcHMWakeFraction(c(32.25,32.20),
#'                    c(10746.28,8669.7),
#'                    c(13.57,11.49),
#'                    c(1,1),
#'                    c(218.75,209.25),
#'                    c(6.7,7),
#'                    c(1.27,1.18),
#'                    c(0.0015,0.0014),
#'                    c(0.00033,0.00035),
#'                    c(0.81,0.65),
#'                    c(0.81,0.67),
#'                    c(0.99,0.98),
#'                    c(13.57,11.49))
#'@export


calcHMWakeFraction<- function(breadth, wettedSA,maxDraft,nProp,
                              lwl,propDiam,formFactor,Cf,Ca,Cbw,
                    Cp,Cm,aftDraft=maxDraft,Cstern=0,lcb=0){

    c8<-ifelse(breadth/aftDraft<5,#case 1
      breadth*wettedSA/(lwl*propDiam*aftDraft)
    ,#case 2
      wettedSA*(7*breadth/aftDraft-25)/(lwl*propDiam*(breadth/aftDraft-3))
    )
#================
    c9<- ifelse(c8<28,#case 1
    c8,#case 2
      32-(16/(c8-24 ))
    )
#================
    c11<- ifelse(aftDraft/propDiam<2,#case 1
      aftDraft/propDiam
   , #case 2
    0.0833333*((aftDraft/propDiam)^3)+1.33333
    )
#================
c19<-ifelse(Cp<0.7,
            0.12997/(0.95-Cbw)-0.11056/(0.95-Cp),
            0.18567/(1.3571-Cm)-0.71276+0.38648*Cp
            )
#================

# wakeFraction<- ifelse(nProp==1,#begin single screw estimation using [2] method
#   c9*(#c20
#     1+0.015*Cstern
#   )*
#     (#Cv
#       formFactor*Cf+Ca
#     )*(lwl/aftDraft)*(0.050776+0.93405*c11*(
#       (#Cv
#         formFactor*Cf+Ca
#       )/(1-(#Cp1
#         1.45*Cp-0.315-0.0225*lcb
#       ))
#     )
#     )+0.27915*(#c20
#       1+0.015*Cstern
#     )*sqrt(
#       breadth/(lwl*(1-(#Cp1
#         1.45*Cp-0.315-0.0225*lcb
#       )))
#     )+c19*
#     (#c20
#       1+0.015*Cstern
#     )
# #end single screw estimation
# ,#begin twin screw estimation using [1] method
#  0.3095*Cbw+10*
#     (#Cv
#       formFactor*Cf+Ca
#     )*Cbw-(0.23*propDiam/sqrt(breadth*maxDraft))
# )#end twin screw estimation

wakeFraction<-Cp

#single propeller and will not create error by square rooting negative values
index<-which(nProp==1 & (breadth/(lwl*(1-(1.45*Cp-0.315-0.0225*lcb))))>0)
wakeFraction[index]<- c9[index]*(#c20
                1+0.015*Cstern
              )*
              (#Cv
                formFactor[index]*Cf[index]+Ca[index]
              )*(lwl[index]/aftDraft[index])*(0.050776+0.93405*c11[index]*(
                (#Cv
                  formFactor[index]*Cf[index]+Ca[index]
                )/(1-(#Cp1
                  1.45*Cp[index]-0.315-0.0225*lcb[index]
                ))
              )
              )+0.27915*(#c20
                1+0.015*Cstern
              )*sqrt(
                breadth[index]/(lwl[index]*(1-(#Cp1
                  1.45*Cp[index]-0.315-0.0225*lcb[index]
                )))
              )+c19[index]*
              (#c20
                1+0.015*Cstern
              )

#twin propeller or those that will create errors with single propeller method
index<-which(nProp==2 | (breadth/(lwl*(1-(1.45*Cp-0.315-0.0225*lcb))))<0)
wakeFraction[index]<- 0.3095*Cbw[index]+10*
            (#Cv
              formFactor[index]*Cf[index]+Ca[index]
            )*Cbw[index]-(0.23*propDiam[index]/sqrt(breadth[index]*maxDraft[index]))

return(wakeFraction)
}

