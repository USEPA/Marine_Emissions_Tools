#'@title calcKristPwr
#'
#' @description Calculate instantaneous main engine power (kW) using the
#' Kristensen 2016 updates of Harvald equations.
#'
#' @param totalInstalledPwr Total installed main engine power (vector of
#'  numericals, kW) (maximum
#' continuous rated power)
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see
#'  \code{\link{calcSpeedUnitConversion}})
#' @param actualDraft Actual draft (vector of numericals, m)
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param shipType Ship type (vector of strings, see \code{\link{calcShipType}}),
#' determined by Stat 5 code: \itemize{
#' \item"container.ship"
#' \item"bulk.carrier"
#' \item"tanker"
#' \item"general.cargo"
#' \item"vehicle.carrier"
#' \item"reefer"
#' \item"ro.ro"
#' \item"passenger"
#' \item"tug"
#' \item"misc"
#'}
#' @param lwl Waterline length (vector of numericals, m) (see
#' \code{\link{calclwl}})
#' @param breadth Moulded breadth (vector of numericals, m)
#' @param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#' @param Cb Maximum block coefficient (vector of numericals, dimensionless)
#' (see \code{\link{calcCb}})
#' @param nProp Number of propellers (vector of numericals, see
#' \code{\link{calcPropNum}})
#' @param dwt Ship maximum deadweight tonnage (vector of numericals, tonnage)
#' @param serviceMargin A service margin to account for weather and sea effects:
#' \itemize{\item At-sea operations = 15 (Default) \item Coastal operations = 10}
#' Can supply either a vector of numericals, a single number, or rely on the
#' default
#' @param shaftEff Shaft efficiency (dimensionless). Default = 0.98.
#'  Ratio of power delivered to the propeller and the brake power delivered by
#'  the engine. Can supply either a vector of numericals, a single number, or
#'  rely on the default
#' @param relRotationEff Relative rotational efficiency (dimensionless).
#'  Default = 1. Accounts for effect of rotational flow of water around propeller.
#'  Can supply either a vector of numericals, a single number, or rely on the
#'  default
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
#' \item"kristensen"
#' \item"benford"
#' \item"schneekluth"}
#' @param tankerBulkCarrierShipTypes Ship types specified in input
#' \code{shipTypes} to be modeled as tankers and bulk carriers.
#' @param tugShipTypes Ship types specified in input \code{shipTypes} to be
#' modeled as tugs
#' @param roroPaxShipTypes Ship types specified in input \code{shipTypes} to be
#' modeled as RORO and passenger ships
#' @param gCargoShipTypes Ship types specified in input \code{shipTypes} to be
#' modeled as general cargo
#' @param containerShipTypes Ship types specified in input \code{shipTypes} to
#' be modeled as container ships
#'
#' @details
#' Primary method from Kristensen (2013). Estimation of some inputs use
#' methodology from Rakke (2016).
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
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'Kristensen, H. O. 2016. "Revision of statistical analysis and determination of
#'regression formulas for main dimensions of container ships based on data from
#'Clarkson."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'\href{http://hdl.handle.net/11250/2410741}{Rakke, S. G. 2016. "Ship Emissions
#'Calculation from AIS." NTNU.}
#'
#'@seealso \itemize{
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcSpeedUnitConversion}}
#'\item \code{\link{calcCb}}
#'\item \code{\link{calcPropNum}}
#'\item \code{\link{calcCm}}
#'\item \code{\link{calcShipType}}
#'\item \code{vignette("OverviewOfPowerModels", package="ShipPowerModel")}
#'\item \code{vignette("Kristensen.Example", package="ShipPowerModel")}
#'}
#'
#' @family Kristensen Calculations
#'
#' @examples
#' calcKristPwr(
#' totalInstalledPwr=9363,
#' shipSpeed=10.8,
#' actualDraft=12.5,
#' maxDraft=13.6,
#' shipType="bulk.carrier",
#' lwl=218,
#' breadth=32.25,
#' maxDisplacement=80097,
#' Cb=0.8162717,
#' nProp=1,
#' dwt=70000,
#' serviceMargin=15,
#' shaftEff=0.98,
#' relRotationEff=1,
#' seawaterTemp=15,
#' seawaterDensity=1.025,
#' CmEquationType="kristensen"
#' )
#'
#' @export

calcKristPwr<-function(
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
  dwt,
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
  containerShipTypes=c("container.ship")
){
#=============================================================
  #Inputs
  Cbw<-calcCbw(Cb, actualDraft, maxDraft)
  actualDisplacement<-calcActualDisp(Cb,
                                     Cbw,
                                     actualDraft,
                                     maxDraft,
                                     maxDisplacement)

  Cm<-calcCm(shipType,Cbw,maxDraft,actualDraft,CmEquationType,tankerBulkCarrierShipTypes,tugShipTypes,roroPaxShipTypes)


  Cp<-calcCp(Cm, Cbw,shipType, bounds="none", roroPaxContainerShipTypes=union(roroPaxShipTypes,containerShipTypes),gCargoShipTypes,tankerBulkCarrierShipTypes)

  propDiam<-calcPropDia(shipType, maxDraft,tankerBulkCarrierGCargoShipTypes=union(tankerBulkCarrierShipTypes,gCargoShipTypes),containerShipTypes)

  M<- calcShipM(actualDisplacement,lwl)

  froudeNum<- calcFroudeNum(shipSpeed,lwl)
#=============================================================
#Frictional Resistance
Cf<-calcCf(shipSpeed, lwl, seawaterTemp, seawaterDensity)
#=============================================================
#Wetted Surface Area
wettedSA<-calcKristWettedSA(shipType,
                            maxDisplacement,
                            maxDraft,
                            actualDraft,
                            lwl,
                            breadth,
                            Cbw,
                            seawaterDensity,
                            tankerBulkCarrierGCargoShipTypes=union(tankerBulkCarrierShipTypes,gCargoShipTypes),
                            containerShipTypes,
                            paxTugShipTypes=union(roroPaxShipTypes, tugShipTypes))

#==========================================
#Incremental Resistance
Ca<-calcKristCa(shipType,actualDisplacement,
                tankerBulkCarrierGCargoShipTypes=union(tankerBulkCarrierShipTypes,gCargoShipTypes),
                containerShipTypes)
#==========================================
#Air Resistance
Caa<-calcKristCaa(shipType,dwt,
                  tankerBulkCarrierGCargoShipTypes=union(tankerBulkCarrierShipTypes,gCargoShipTypes),
                  containerShipTypes)
#==========================================
#Residual Resistance
Cr<-calcKristCr(shipType,
                M,
                froudeNum,
                actualDraft,
                breadth,
                Cp,
                tankerBulkCarrierGCargoShipTypes=union(tankerBulkCarrierShipTypes,gCargoShipTypes))

#=============================================================
#Thrust Deduction Factor
t<-calcKristThrustFactor(
                    shipType,
                    breadth,
                    lwl,
                    Cbw,
                    propDiam,
                    M,
                    nProp,
                    tankerBulkCarrierShipTypes)
#=============================================================
#Wake Fraction
#*(1-t)/(1-w)= hull efficiency
w<-calcKristWakeFrac(shipType,
                breadth,
                lwl,
                Cbw,
                propDiam,
                M,
                nProp,
                tankerBulkCarrierShipTypes)
#==========================================================
#Total Resistance
R<-calcKristTotalRes(wettedSA,
                     Cf,
                     Cr,
                     Ca,
                     Caa,
                     seawaterDensity,
                     shipSpeed,
                     serviceMargin)
#==========================================================
# Open Water Efficiency
no<-calcOpenWaterEff(R,
                     t,
                     nProp,
                     w,
                     propDiam,
                     shipSpeed,
                     seawaterDensity)
#=============================================================
power<-calcResistanceShipPwr(R,shipSpeed, (1-t)/(1-w), no,totalInstalledPwr, shaftEff,relRotationEff, pwrUpperBoundPercent, pwrLowerBoundPercent)

return(power)
}
