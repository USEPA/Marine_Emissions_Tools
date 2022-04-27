#'@title calcEF_NOx
#'
#'@description
#'Calculates the appropriate nitrogen oxide (NOx) emission factor (g/kWh) for
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
#'@param tier NOx engine tier (string or vector of strings) (see \code{\link{calcTier}}).
#'Valid values are:
#'\itemize{
#'  \item "Tier 0"
#'  \item "Tier 1"
#'  \item "Tier 2"
#'  \item "Tier 3"
#'}
#'@param loadFactor Fractional percentage (between 0 and 1) of main engine
#' required to propel vessel at given speed (vector of numericals) (see
#' ShipPowerModel library). Required for `main_aux_boiler` = "main".
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
#'For more information about calculating NOx emission factors, see Section 3.5.1
#'of the Port Emissions Inventory Guidance.
#'
#'@return \code{EF_NOx} (g/kWh) (vector of numericals)
#'
#'@references
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Ports Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@examples
#'calcEF_NOx(engineType = c("SSD","MSD","MSD-ED","GT"),
#'           location = c("ECA","OutsideECA","GreatLakes","ECA"),
#'           tier = c("Tier 2","Tier 3","Tier 1", "Tier 0"),
#'           loadFactor = c(0.3, 0.3, 0.3, 0.3),
#'           main_aux_boiler = "main")
#'
#'calcEF_NOx(engineType = c("HSD","MSD","HSD","LNG"),
#'          location = c("ECA","OutsideECA","GreatLakes","ECA"),
#'           tier = c("Tier 2","Tier 3","Tier 1", "Tier 0"),
#'           loadFactor = c(0.3, 0.3, 0.3, 0.3),
#'           main_aux_boiler = "aux")
#'
#'calcEF_NOx(engineType = c("Boiler","Boiler"),
#'           location = c("ECA","OutsideECA"),
#'           tier = c("Tier 2", "Tier 1"),
#'           loadFactor = c(0.3, 0.3, 0.3, 0.3),
#'           main_aux_boiler = "boiler")
#'
#'calcEF_NOx(engineType = "SSD",
#'           location = c("ECA","OutsideECA","GreatLakes","ECA"),
#'           tier = c("Tier 2","Tier 3","Tier 1", "Tier 0"),
#'           loadFactor = c(0.3, 0.3, 0.3, 0.3),
#'           main_aux_boiler = "main")
#'
#'calcEF_NOx(engineType = "SSD",
#'           location = c("ECA","OutsideECA","GreatLakes","ECA"),
#'           tier = "Tier 2",
#'           loadFactor = c(0.3, 0.3, 0.3, 0.3),
#'           main_aux_boiler = "main")
#'
#'@import data.table
#'@importFrom utils data
#'@importFrom utils tail
#'@importFrom stats weighted.mean
#'@export

calcEF_NOx<-function(engineType, location, tier, loadFactor=NULL, main_aux_boiler="main")
{
  #bind variables to make devtools::check() happy
  MainFuelMixTable<-AuxNOxEF<-AuxFuelMixTable<-BoilerNOxEF<-BoilerFuelMixTable<-Proportion<-.<-NULL
  
  if(is.null(loadFactor) & main_aux_boiler=="main") {
    stop("loadFactor is a required argument when calculating NOx emission factors for main engines (see ?calcEF_NOx)")
  }

  #Read In Emission Factor DataFrames
  if(main_aux_boiler=="main"){
    EF<-ShipEF::MainNOxEF
    fuelMixTable<-ShipEF::MainFuelMixTable
  }else if(main_aux_boiler=="aux"){
    EF<-ShipEF::AuxNOxEF
    fuelMixTable<-ShipEF::AuxFuelMixTable
  }else if(main_aux_boiler=="boiler"){
    EF<-ShipEF::BoilerNOxEF
    fuelMixTable<-ShipEF::BoilerFuelMixTable
  }
  #=================================================================
  if(main_aux_boiler=="boiler"){
    engineType<-"Boiler"
    tier<-"Tier 0"
  }

  EF<-fuelMixTable[EF, on=c("engineType","fuelType"), allow.cartesian=TRUE]
  EF<-EF[,.(nox=weighted.mean(nox,w=Proportion)),by=c("Location","engineType","tier")]

  # set up user input data as a data.table
  dt<-data.table::data.table(engineType=engineType, Location=location, tier=tier, loadFactor=loadFactor)
  
  if(main_aux_boiler == "main") {
    # When T3 main engines are operating below the threshold defined below, assume
    # that SCR technology is not working and emissions default to T2. Therefore,
    # set tier to T2 for loads below the threshold
    scrThreshold <- 0.25
    dt[(loadFactor < scrThreshold) & (tier == "Tier 3"), tier := "Tier 2"]
  }
  
  # merge user input data.table with emission factor table to get the corresponding NOx EFs
  nox<-EF[dt,on=c("Location","engineType","tier")][,c("nox")]

  return(nox)
}
