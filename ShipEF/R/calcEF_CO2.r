#'@title calcEF_CO2
#'
#'@description
#'Calculates the appropriate carbon dioxide (CO2) emission factor (g/kWh) for
#'the given parameters.
#'
#'@param engineType Engine type (string or vector of strings) (see
#'\code{\link{calcEngineType}}). Valid values are: \itemize{
#'\item "SSD"
#'\item "MSD"
#'\item "MSD-ED"
#'\item "GT"
#'\item "GT-ED"
#'\item "ST"
#'\item "LNG"
#'\item "HSD" (auxiliary only)
#'\item "Boiler" (boiler only)
#'}
#'@param location Location of vessel (string or vector of strings). Valid values are:
#'\itemize{
#'   \item "ECA"
#'   \item "OutsideECA"
#'   \item "GreatLakes"
#' }
#'@param loadFactor Fractional percentage (between 0 and 1) of main engine
#' required to propel vessel at given speed (vector of numericals) (see
#' ShipPowerModel library). This parameter is optional. By default, it is not
#' used and the resulting emission factor is independent of engine load.
#'@param main_aux_boiler Is this calculation for a propulsive (main), auxiliary
#'(aux), or boiler engine? Options: \itemize{
#' \item "main" (Default)
#' \item "aux"
#' \item "boiler"
#'}
#'
#'@details
#'Location is important for determining the fuel being used, as type of fuel
#'typically used varies by location.
#'
#'For more information about calculating CO2 emission factors, see Section 3.5.6
#'of the Port Emissions Inventory Guidance.
#'
#'@return \code{EF_CO2} (g/kWh) (vector of numericals)
#'
#'@references
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Ports Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@examples
#'calcEF_CO2(engineType = c("SSD","MSD","LNG"),
#'           location = c("ECA","OutsideECA","GreatLakes"),
#'           loadFactor=c(0.3,.7,.9),
#'           main_aux_boiler = "main")
#'
#'calcEF_CO2(engineType = c("SSD","SSD","LNG"),
#'           location = c("ECA","ECA","GreatLakes"),
#'           main_aux_boiler = "main")
#'
#'calcEF_CO2(engineType = c("HSD","MSD","LNG"),
#'           location = c("ECA","ECA","GreatLakes"),main_aux_boiler="aux")
#'
#'calcEF_CO2(engineType = c("MSD","SSD"),
#'           location = c("ECA","OutsideECA"),
#'           main_aux_boiler = "boiler")
#'
#'calcEF_CO2(engineType = "SSD",
#'           location = c("ECA","OutsideECA","GreatLakes"),
#'           loadFactor=c(0.3,.7,.9),
#'           main_aux_boiler = "main")
#'
#'calcEF_CO2(engineType = c("SSD","MSD","LNG"),
#'           location = "ECA",
#'           loadFactor=c(0.3,.7,.9),
#'           main_aux_boiler = "main")
#'
#'calcEF_CO2(engineType = "SSD",
#'           location = "ECA",
#'           loadFactor=c(0.3,.7,.9),
#'           main_aux_boiler = "main")
#'
#'calcEF_CO2(engineType = c("SSD","MSD","LNG"),
#'           location = "ECA",
#'           loadFactor=NULL,
#'           main_aux_boiler = "main")
#'@import data.table
#'@importFrom utils data
#'@importFrom utils tail
#'@importFrom stats weighted.mean
#'@export

calcEF_CO2<- function(engineType, location, loadFactor=NULL,
                       main_aux_boiler="main"
){
  #bind variables to make devtools::check() happy
  BSFC_LoadFactor<-BSFC<-conversionFactor<-.<-Proportion<-ID<-NULL

  #Read In Emission Factor and Conversion Factor DataFrames

  if(main_aux_boiler=="main"){
    EF<-ShipEF::MainBSFC
    fuelMixTable<-ShipEF::MainFuelMixTable
  }else if(main_aux_boiler=="aux"){
    EF<-ShipEF::AuxBSFC
    fuelMixTable<-ShipEF::AuxFuelMixTable
  }else if(main_aux_boiler=="boiler"){
    EF<-ShipEF::BoilerBSFC
    fuelMixTable<-ShipEF::BoilerFuelMixTable
  }

  CO2ConversionFactor<-ShipEF::CO2ConversionFactor
  #=======================================
  if(main_aux_boiler=="boiler"){engineType<-"Boiler"}

  EF<-EF[CO2ConversionFactor, on=c("fuelType"), allow.cartesian=TRUE]
  EF<-fuelMixTable[EF, on=c("fuelType","engineType"), allow.cartesian=TRUE]

  #Create Table of Input Values
  inputs<-data.table::data.table(engineType=engineType, Location=location, loadFactor=loadFactor)
  # assign unique value to each row so when the table gets exploded by merges later, we can aggregate back to original inputs
  inputs[, ID := .I] 

  #Use BSFC Baseline for auxiliary and boiler emission factors (by setting loadFactor to NA)
  if(main_aux_boiler!="main"|is.null(loadFactor)){
    inputs[,loadFactor:=NA]
  }
  
  #join inputs to emission factor table (Adds ID and load Factor)
  EF<-EF[inputs,on=c("Location","engineType"),allow.cartesian=TRUE]

  #Updated BSFC According to Load Factor, Using Jalkanen 2012/IMO GHG 3
  EF[,BSFC_LoadFactor:=ShipEF::calcLoadBSFC(loadFactor = loadFactor, BSFC_Baseline = BSFC)]

  #Weight Emission Factor According to Proportion of Fuel Assumed per Engine Type and Location
  EF<-EF[,co2:=BSFC_LoadFactor*conversionFactor]
  EF<-EF[,.(co2=weighted.mean(co2,w=Proportion)),by=c("ID","Location","engineType")]

  co2<-EF[,c("co2"),with=FALSE]

  return(co2)
}
