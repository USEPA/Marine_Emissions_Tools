#' @title calcHMPwr
#'
#' @description Calculate instantaneous main engine power (kW) using the Holtrop & Mennen method.
#'
#' @param totalInstalledPwr Total installed main engine power (vector of numericals, kW) (maximum
#' continuous rated power)
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see 
#'  \code{\link{calcSpeedUnitConversion}})
#' @param actualDraft Actual draft (vector of numericals, m)
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param shipType Ship type (vector of strings, see \code{\link{calcShipType}}). 
#' Must align with \code{tankerBulkCarrierShipTypes}, \code{tugShipTypes}, 
#' \code{roroPaxShipTypes}, \code{gCargoShipTypes},
#' \code{containerShipTypes} groupings
#' @param lwl Waterline length (vector of numericals, m) (see \code{\link{calclwl}})
#' @param breadth Moulded breadth (vector of numericals, m)
#' @param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#' @param Cb Maximum block coefficient (vector of numericals, dimensionless) (see 
#' \code{\link{calcCb}})
#' @param nProp Number of propellers (vector of numericals, see \code{\link{calcPropNum}})
#' @param serviceMargin A service margin to account for weather and sea effects:
#' \itemize{\item At-sea operations = 15 (Default) \item Coastal operations = 10} Can 
#' supply either a vector of numericals, a single number, or rely on the default
#' @param shaftEff Shaft efficiency (dimensionless). Default = 0.98.
#'  Ratio of power delivered to the propeller and the brake power delivered by
#'  the engine. Can supply either a vector of numericals, a single number, or rely on the default.
#' @param relRotationEff Relative rotational efficiency (dimensionless).
#'  Default = 1. Accounts for effect of rotational flow of water around propeller.
#' Can supply either a vector of numericals, a single number, or rely on the default
#' @param seawaterTemp Sea water temperature. Default = 15 (degrees Celsius). Can 
#' supply either a vector of numericals, a single number, or rely on the default
#' @param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can 
#' supply either a vector of numericals, a single number, or rely on the default
#' @param pwrUpperBoundPercent Percent of total installed power at which
#' required power is capped. Default = 1, which indicates required power cannot
#' exceed \code{totalInstalledPwr}. Can supply either a vector of numericals, a 
#' single number, or rely on the default
#' @param pwrLowerBoundPercent Percent of total installed power to act as lower
#' bound for required power. Default = 0.02, which indicates required power
#' cannot go below 2\% of \code{totalInstalledPwr}. Can supply either a vector
#' of numericals, a single number, or rely on the default
#' @param CmEquationType Type of equation to estimate the midship section
#' coefficient (see \code{\link{calcCm}}): \itemize{
#' \item"kristensen (Default)"
#' \item"benford"
#' \item"schneekluth"}
#' This argument is not vectorized, as it takes only a single string
#' @param tankerBulkCarrierShipTypes Ship types specified in input
#' \code{shipTypes} to be modeled as tankers and bulk carriers
#' @param tugShipTypes Ship types specified in input \code{shipTypes} to be
#' modeled as tugs (vector of strings)
#' @param roroPaxShipTypes Ship types specified in input \code{shipTypes} to be
#' modeled as RORO and passenger ships (vector of strings)
#' @param gCargoShipTypes Ship types specified in input \code{shipTypes} to be
#' modeled as general cargo (vector of strings)
#' @param containerShipTypes Ship types specified in input \code{shipTypes} to
#' be modeled as container ships (vector of strings)
#' @param Cstern Afterbody form coefficient: \itemize{\item V-shaped Hull = -10
#'         \item U-Shaped Hull =  10
#'         \item Normal Hull = 0 (default)}
#' Can supply either a vector of numericals, a single number, or rely on the default
#' @param forwardDraft Forward draft (deviation from actual draft indicates trim) 
#' (vector of numericals, m)
#' @param aftDraft Aft draft (deviation from actual draft indicates trim)
#' (vector of numericals, m)
#' @param appendagesList List of appendages on ship (vector of strings) \itemize{
#'  \item"rudder behind skeg"
#'  \item"rudder behind stern"
#'  \item"twin-screw balance rudders"
#'  \item"shaft brackets"
#'  \item"skeg"
#'  \item"strut bossings"
#'  \item"hull bossings"
#'  \item"shafts"
#'  \item"stabilizer fins"
#'  \item"dome"
#'  \item"bilge keels"}
#' @param wettedAppSAList List of wetted surface areas corresponding to list of
#' appendages (vector of numericals, m^2)
#'
#' @details
#' Primary method from Holtrop & Mennen (1982). Updated equations for high speed
#' operations (Froude number > 0.55) from Holtrop & Mennen (1984). Estimation of
#' some inputs use methodology from Rakke (2016).
#'
#' This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierShipTypes}, \code{tugShipTypes}, \code{roroPaxShipTypes},
#' \code{gCargoShipTypes}, \code{containerShipTypes} grouping parameters to
#' provide these ship type groupings. Any ship types not included in these groupings
#' will be considered as miscellaneous vessels.
#'
#' Ship speed and actual draft are typically obtained from sources such as AIS
#' messages or ship records.
#'
#' @return power (vector of numericals, kW)
#'
#' @references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'Holtrop, J. and Mennen, G. G. J. 1984. "A Statistical Re-Analysis of Resistance
#'and Propulsion Data'.
#'
#'\href{http://hdl.handle.net/11250/2410741}{Rakke, S. G. 2016. "Ship Emissions
#'Calculation from AIS." NTNU.}
#'
#'@seealso \itemize{
#'\item \code{\link{calcSpeedUnitConversion}}
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcCb}}
#'\item \code{\link{calcPropNum}}
#'\item \code{\link{calcCm}}
#'\item \code{vignette("OverviewOfPowerModels", package="ShipPowerModel")}
#'\item \code{vignette("HoltropMennen.Example", package="ShipPowerModel")}
#'}
#'
#' @family Holtrop-Mennen Calculations
#'
#' @examples
#' calcHMPwr(
#' totalInstalledPwr=rep(9363,2),,
#' shipSpeed=seq(0,1,1),
#' actualDraft=rep(12.48,2),
#' maxDraft=rep(13.57,2),
#' shipType=rep("bulk.carrier",2),
#' lwl=rep(218.75,2),
#' breadth=rep(32,2),
#' maxDisplacement=rep(80097,2),
#' Cb=rep(0.8099003,2),
#' nProp=rep(1,2),
#' serviceMargin=15,
#' shaftEff=0.98,
#' relRotationEff=1,
#' seawaterTemp=15,
#' seawaterDensity=1.025,
#' Cstern=0,
#' CmEquationType="kristensen",
#' forwardDraft=rep(13.57,2),
#' aftDraft=rep(13.57,2),
#' appendagesList=c(""),
#' wettedAppSAList=NA
#' )
#'
#' @export


  calcHMPwr<- function(
  totalInstalledPwr,
  shipSpeed,
  actualDraft,
  maxDraft,
  shipType,
  lwl,
  breadth,
  maxDisplacement,
  Cb,
  nProp,
  serviceMargin=15,
  shaftEff=0.98,
  relRotationEff=1,
  seawaterTemp=15,
  seawaterDensity=1.025,
  pwrUpperBoundPercent=1,
  pwrLowerBoundPercent=0.02,
  CmEquationType="kristensen",
  tankerBulkCarrierShipTypes=c("tanker","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier"),
  tugShipTypes=c("service.tug","tug"),
  roroPaxShipTypes=c("passenger","ferry.pax","ferry.ro.pax","cruise","cruise.ed","yacht","ro.ro"),
  gCargoShipTypes=c("general.cargo"),
  containerShipTypes=c("container.ship"),
  Cstern=0,
  forwardDraft=NULL,
  aftDraft=NULL,
  appendagesList=c(""),
  wettedAppSAList=NA
 ){
#===========================================
if(is.null(forwardDraft)){forwardDraft<-maxDraft}
if(is.null(aftDraft)){aftDraft<-maxDraft}

#Inputs
    Cbw<-calcCbw(Cb, actualDraft, maxDraft)
    Cm<-calcCm(shipType,Cbw,maxDraft,actualDraft,CmEquationType,tankerBulkCarrierShipTypes,tugShipTypes,roroPaxShipTypes)
    Cp<-calcCp(Cm, Cbw,shipType, bounds="holtrop mennen", roroPaxContainerShipTypes=union(roroPaxShipTypes,containerShipTypes),gCargoShipTypes,tankerBulkCarrierShipTypes)
    Cwp<-calcCwp(Cbw, CwpEquationType="kristensen")
    froudeNum<-calcFroudeNum(shipSpeed, lwl)
    At<-calcAt(Cm, breadth, maxDraft)
    hb<-calchb(maxDraft)
    Abt<-calcAbt(Cm, breadth, maxDraft)
    propDiam<-calcPropDia(shipType, maxDraft,tankerBulkCarrierGCargoShipTypes=union(tankerBulkCarrierShipTypes,gCargoShipTypes),containerShipTypes)
    lcb<-calclcb(lwl)
#===========================================
Cf<-calcCf(shipSpeed,
           lwl,
           seawaterTemp,
           seawaterDensity)
#=============================================
Rapp<-calcHMAppendageRes(shipSpeed,
                          Cf,
                          appendagesList,
                          wettedAppSAList,
                          seawaterDensity)
#=============================================
Rw<-calcHMWaveMakingRes(lwl,
                         breadth,
                        Cp,
                        Cwp,
                        Cm,
                         maxDisplacement,
                         maxDraft,
                         froudeNum,
                        At,
                        hb,
                        Abt,
                         seawaterDensity,
                         forwardDraft,
                        lcb)
#========================================

FormFactor<-calcHMFormFactor(maxDraft,
                              lwl,
                              breadth,
                              maxDisplacement,
                             Cp,
                              Cstern,
                             lcb)
#==========================================

wettedSA<-calcHMWettedSA(lwl,
                          actualDraft,
                          breadth,
                          Cm,
                          Cbw,
                          Cwp,
                          Abt)
#==============================================
Rtr<-calcHMImmersedTransomRes(shipSpeed,
                               breadth,
                               Cwp,
                               maxDraft,
                             At,
                             seawaterDensity)
#==============================================
Rb<-calcHMBulbousBowRes(shipSpeed,
                         maxDraft,
                         forwardDraft,
                         Abt,
                         hb,
                         seawaterDensity)
#=============================================================
Ca<-calcHMCa(maxDraft,
                         lwl,
                         Cbw,
                         breadth,
                         forwardDraft,
                        Abt,
                         hb)
#=============================================================
w<-calcHMWakeFraction(breadth,
                      wettedSA,
                       maxDraft,
                       nProp,
                       lwl,
                      propDiam,
                      FormFactor,
                      Cf,
                      Ca,
                     Cbw,
                      Cp,
                      Cm,
                       aftDraft,
                      Cstern,
                      lcb)

t<-calcHMThrustFactor(breadth,
                       lwl,
                       maxDraft,
                       maxDisplacement,
                       nProp,
                       Cp,
                       propDiam,
                       Cbw,
                       Cstern,
                       lcb,
                       seawaterDensity)
#=============================================================

R<- calcHMTotalRes(Rapp,Rw,Rb,Rtr,seawaterDensity,wettedSA,shipSpeed,
                   Cf,FormFactor,Ca,serviceMargin)

#=============================================================
no<- calcOpenWaterEff(R,t,nProp,w,propDiam,shipSpeed)

#=============================================================
power<-calcResistanceShipPwr(R,shipSpeed, (1-t)/(1-w), no,totalInstalledPwr, shaftEff,relRotationEff, pwrUpperBoundPercent, pwrLowerBoundPercent)


 return(power)
}
