#'@title calcHMWaveMakingRes
#'
#'@description Calculate wave resistance (\code{Rw}) (kN) from the Holtrop &
#'Mennen method.
#'
#'@param lwl Waterline length (vector of numericals, m) (see
#'\code{\link{calclwl}})
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param Cp Prismatic coefficient (vector of numericals, dimensionless) (see
#'\code{\link{calcCp}})
#'@param Cwp Water plane area coefficient (vector of numericals, see
#'\code{\link{calcCwp}})
#'@param Cm Midship section coefficient (vector of numericals, dimensionless)
#'(see \code{\link{calcCm}})
#'@param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#'@param maxDraft Maximum summer load line draft (vector of numericals, m)
#'@param froudeNum Froude number (vector of numericals, dimensionless) (see
#'\code{\link{calcFroudeNum}})
#'@param At Transom area (vector of numericals, m^2) (see \code{\link{calcAt}})
#'@param hb Center of bulb area above keel line (vector of numericals, m) (see
#' \code{\link{calchb}})
#'@param Abt Traverse bulb area (vector of numericals, m^2) (see
#'\code{\link{calcAbt}})
#'@param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can
#' supply either a vector of numericals, a single number, or rely on the default
#'@param forwardDraft Forward draft (deviation from actual draft indicates trim)
#' (vector of numericals, m)
#'@param lcb Longitudinal position of center of buoyancy (vector of numericals,
#'see \code{\link{calclcb}})
#'
#'@details
#'Note: This calculates resistance, not a coefficient.Therefore, it does not
#'need to be multiplied by wetted surface area like the frictional resistance
#'coefficient is.
#'
#'Note: In "A Statistical Re-Analysis of Resistance and Propulsion Data" the
#'authors re-analyze with the inclusion of Series 64 hull forms for a total of
#'334 models included in the analysis. Their original paper insufficiently
#'modeled high speed craft with Froude number >= 0.55, thus the original wave
#'making resistance equation is used for \code{froudeNum} < 0.55 and the new
#'wave making resistance equation is used for \code{froudeNum} >= 0.55. The
#'authors also include wave making resistance equations in the updated paper for
#'Froude number < 0.55, but these require more computing power and they mention
#'that they closely resemble the original equation. Therefore, the original
#'equation is used for \code{froudeNum} < 0.55.
#'
#'Extra Info:
#'
#'\code{c2}: Accounts for the reduction of the wave resistance due to action of
#'a bulbous bow
#'
#'\code{c5}: Represents the influence of a transom stern on the wave resistance
#'
#'\code{i_E}: The angle of the waterline at the bow in the degrees with reference
#' to center plane but neglecting the local shape at the stem
#'
#'@return \code{Rw} (vector of numericals, kN)
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
#'\item \code{\link{calcCwp}}
#'\item \code{\link{calcCm}}
#'\item \code{\link{calcFroudeNum}}
#'\item \code{\link{calcAt}}
#'\item \code{\link{calchb}}
#'\item \code{\link{calcAbt}}
#'\item \code{\link{calclcb}}}
#'
#'@family Holtrop-Mennen Calculations
#'@family Resistance Calculations
#'
#' @examples
#'  calcHMWaveMakingRes(lwl=c(218.75,209.25),
#'                      breadth=c(32.25,32.20),
#'                      Cp=c(0.81,0.67),
#'                      Cwp=c(0.91,0.84),
#'                      Cm=c(0.99,0.98),
#'                      maxDisplacement=c(80097,52382.04),
#'                      maxDraft=c(13.57,11.49),
#'                      froudeNum=c(0,0.25),
#'                      At=c(22.2,18.5),
#'                      hb=c(5.43,4.6),
#'                      Abt=c(32.02,27.98),
#'                      seawaterDensity=1.025,
#'                      forwardDraft=c(13.57,11.49))
#' @export

calcHMWaveMakingRes<- function(lwl,breadth,Cp,Cwp,
                               Cm,maxDisplacement,maxDraft,
                               froudeNum,
                               At,
                               hb,
                               Abt,
                               seawaterDensity,
                               forwardDraft,
                               lcb=0){


c1<- ifelse(breadth/lwl<0.11,
  2223105*(
    #c7
    (
     0.229577*(breadth/lwl)^0.33333
    )^3.78613)*
    ((maxDraft/breadth)^1.07961)*(90-

                                   #i_E
                                   (
                                    1+89*exp(-(lwl/breadth)^0.80856*
                                               ((1-Cwp)^0.30484)*
                                               (1-Cp-0.0225*lcb)^0.6367*
                                               ((#Lr
                            (lwl*(1-Cp+(0.06*Cp*lcb)/(4*Cp-1)))/
                                                   breadth
                                               )^0.34574)*
                                               (100*maxDisplacement/(lwl^3))^0.16302
                                             )
                                   )
                                   )^-1.37565
,ifelse(breadth/lwl<0.25, #case 2 (<=0.11b/lwl<0.25)
  2223105*(
    #c7
    (
     breadth/lwl
    )^3.78613)*
    ((maxDraft/breadth)^1.07961)*(90-

                                   #i_E
                                   (
                                     1+89*exp(-(lwl/breadth)^0.80856*
                                                ((1-Cwp)^0.30484)*
                                                (1-Cp-0.0225*lcb)^0.6367*
                                                ((#Lr
                                                  (lwl*(1-Cp+(0.06*Cp*lcb)/(4*Cp-1)))/
                                                    breadth
                                                )^0.34574)*
                                                (100*maxDisplacement/(lwl^3))^0.16302
                                     )
                                   )
    )^-1.37565
,#case #3 (b/lwl>= 0.25)
  2223105*(
    #c7
    (
      0.5-0.0625*(lwl/breadth)
    )^3.78613)*
    ((maxDraft/breadth)^1.07961)*(90-

                                   #i_E
                                   (
                                     1+89*exp(-(lwl/breadth)^0.80856*
                                                ((1-Cwp)^0.30484)*
                                                (1-Cp-0.0225*lcb)^0.6367*
                                                ((#Lr
                                                  (lwl*(1-Cp+(0.06*Cp*lcb)/(4*Cp-1)))/
                                                    breadth
                                                )^0.34574)*
                                                (100*maxDisplacement/(lwl^3))^0.16302
                                     )
                                   )
    )^-1.37565
))
#=========================================
#both methods
c2<-exp(-1.89*sqrt(
  #c3
  0.56*Abt^1.5/(breadth*maxDraft*(
    0.31*sqrt(Abt)+forwardDraft-hb
  ))
))
#=========================================
#both methods
c5<-1-0.8*At/(breadth*maxDraft*Cm)
#=========================================
m1<- ifelse(Cp<0.8,#case 1
  0.0140407*(lwl/maxDraft)-(1.75254*(maxDisplacement^(1/3))/lwl)-
  4.79323*(breadth/lwl)-
  #c16
  (
    8.07981*Cp-13.8673*(Cp^2)+6.984388*(Cp^3)
  ),#case 2
    0.0140407*(lwl/maxDraft)-(1.75254*(maxDisplacement^(1/3))/lwl)-
      4.79323*(breadth/lwl)-
      #c16
      (
     1.73014-0.7067*Cp
      ))
#=========================================
#both methods
c15<-ifelse((lwl^3/maxDisplacement)<512,
            -1.69385,
            ifelse((lwl^3/maxDisplacement)<1726.91,
                   -1.69385+(
                     (lwl/(maxDisplacement^(1/3)))-8
                   )/2.36,
                   0
            )
)
#=========================================
#orig

m2<-c15*(Cp^2)*exp(-0.1*froudeNum^-2)
#=========================================
#both methods
alpha<- ifelse((lwl/breadth)<12,#case 1
  1.446*Cp-0.03*(lwl/breadth)
,#case 2
  1.446*Cp-0.36
)
#=========================================
#Fn>0.55

 m4<-c15*0.4*exp(-0.034*(froudeNum^-3.29))
#=========================================
#Fn>0.55
c17<-6919.3*(Cm^-1.3346)*
  ((maxDisplacement/(lwl^3))^2.00977)*
  (((lwl/breadth)-2)^1.40692)
#=========================================
#Fn>0.55
m3<- -7.2035*((breadth/lwl)^0.326869)*
  ((maxDraft/breadth)^0.605375)
#=========================================
#Avoids causing errors by calculating with 0 Froude Num from 0 speed
Rw<-froudeNum

index<-which(froudeNum>0)
Rw[index]<- ifelse(froudeNum[index]<0.55,
   #case 1: from 'An Approximate Power Prediction Method'(Holtrop & Mennen, 1982)
  c1[index]*c2[index]*c5[index]*maxDisplacement[index]*seawaterDensity*9.81*
  exp(m1[index]*(froudeNum[index]^-0.9)+
    m2[index]*cos(alpha[index]*froudeNum[index]^-2)
    ),
  #case 2: from 'Statistical Re-Analysis of Resitance and Propulsion Data'(Holtrop,1984)
c17[index]*c2[index]*c5[index]*maxDisplacement[index]*seawaterDensity*9.81*
  exp(m3[index]*(froudeNum[index]^-0.9)+
        m4[index]*cos(alpha[index]*froudeNum[index]^-2)
    )
  )


  return(Rw)
}
