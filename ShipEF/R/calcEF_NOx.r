#'@title calcEF_NOx
#'
#'@description
#'Calculates the appropriate nitrogen oxide (NOx) emission factor (g/kWh) for
#'the given parameters.
#'
#'@param engineType Engine type (vector of strings) (see
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
#'@param location Location of vessel (vector of strings). Valid values are:
#'\itemize{
#'   \item "ECA"
#'   \item "OutsideECA"
#'   \item "GreatLakes"
#' }
#'@param tier NOx engine tier (vector of strings) (see \code{\link{calcTier}}).
#'Valid values are:
#'\itemize{
#'  \item "Tier 0"
#'  \item "Tier 1"
#'  \item "Tier 2"
#'  \item "Tier 3"
#'}
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
#'           main_aux_boiler = "main")
#'
#'calcEF_NOx(engineType = c("HSD","MSD","HSD","LNG"),
#'           location = c("ECA","OutsideECA","GreatLakes","ECA"),
#'           tier = c("Tier 2","Tier 3","Tier 1", "Tier 0"),
#'           main_aux_boiler = "aux")
#'
#'calcEF_NOx(engineType = c("Boiler","Boiler"),
#'           location = c("ECA","OutsideECA"),
#'           tier = c("Tier 2", "Tier 1"),
#'           main_aux_boiler = "boiler")
#'
#'@import data.table
#'@importFrom utils data
#'@importFrom utils tail
#'@importFrom stats weighted.mean
#'@export

calcEF_NOx<-function(engineType, location, tier, main_aux_boiler="main")
{
  #bind variables to make devtools::check() happy
  MainFuelMixTable<-AuxNOxEF<-AuxFuelMixTable<-BoilerNOxEF<-BoilerFuelMixTable<-Proportion<-.<-NULL

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
    engineType<-rep("Boiler",length(engineType))
    tier<-rep("Tier 0",length(engineType))
    }

  EF<-fuelMixTable[EF, on=c("engineType","fuelType"), allow.cartesian=TRUE]
  EF<-EF[,.(nox=weighted.mean(nox,w=Proportion)),by=c("Location","engineType","tier")]

  df<-data.table::data.table(engineType=engineType, Location=location, tier=tier)
  nox<-EF[df,on=c("Location","engineType","tier")][,c("nox")]

  return(nox)
}
