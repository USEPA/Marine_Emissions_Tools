#'@title calcEF_SO2
#'
#'@description
#'Calculates the appropriate sulfur dioxide (SO2) emission factor (g/kWh) for
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
#'@param ECAfuelSulfurPercentage Fuel sulfur cap (percentage by weight) for the
#'Emissions Control Area (ECA). Default = 0.1\% (in effect Jan. 1, 2015)
#'@param GlobalfuelSulfurPercentage Fuel sulfur cap (percentage by weight) for
#'outside the Emissions Control Area (ECA). Default = 0.5\% (in effect Jan. 1,
#'2020)
#'@param main_aux_boiler Is this calculation for a propulsive (main), auxiliary
#'(aux), or boiler engine? Options: \itemize{
#' \item "main" (Default)
#' \item "aux"
#' \item "boiler"
#'}
#'
#'@details
#'Location is important for determining the fuel being used, as fuel sulfur
#'requirements and type of fuel typically used vary by location.
#'
#'For more information about calculating SO2 emission factors, see Section 3.5.7
#'of the Port Emissions Inventory Guidance.
#'
#'@return \code{EF_SO2} (g/kWh) (vector of numericals)
#'
#'@references
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Ports Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@seealso \itemize{
#' \item \code{\link{calcEngineType}}
#' \item ShipPowerModel library
#'}
#'
#'@examples
#'calcEF_SO2(engineType = c("SSD","SSD","MSD-ED"),
#'           location = c("ECA","ECA","OutsideECA"),
#'           loadFactor = c(0.3, 0.4, 0.8))
#'
#'calcEF_SO2(engineType = c("SSD","SSD","MSD-ED"),
#'           location = c("ECA","ECA","OutsideECA"),
#'           loadFactor = NULL)
#'
#'calcEF_SO2(engineType = c("HSD","MSD","LNG"),
#'           location = c("ECA","ECA","ECA"),
#'           loadFactor = c(0.3, 0.4, NA),
#'           main_aux_boiler = "aux")
#'
#'calcEF_SO2(engineType = c("Boiler","Boiler"),
#'           location = c("ECA","OutsideECA"),
#'           loadFactor = c(NA, NA, NA),
#'           main_aux_boiler = "boiler")
#'
#'calcEF_SO2(engineType = "SSD",
#'           location = c("ECA","ECA","OutsideECA"),
#'           loadFactor = NULL)
#'@import data.table
#'@importFrom utils data
#'@importFrom utils tail
#'@importFrom stats weighted.mean
#'@export

calcEF_SO2<- function(engineType, location, loadFactor=NULL,
                       ECAfuelSulfurPercentage=0.1,GlobalfuelSulfurPercentage=0.5,
                       main_aux_boiler="main"
                       )
{

  #bind variables to make devtools::check() happy
  MainBSFC<-MainFuelMixTable<-AuxBSFC<-AuxFuelMixTable<-BoilerBSFC<-BoilerFuelMixTable<-
    BSFC_LoadFactor<-BSFC_LoadFactor<-BSFC<-fuelSulfurLevel<-fuelType<-.<-Proportion<-ID<-NULL

  #Read In Emission Factor DataFrames
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

  #=======================================
  if(main_aux_boiler=="boiler"){engineType<-"Boiler"}

  #join, BSFC, with fuel mix table,
  EF<-fuelMixTable[EF, on=c("fuelType","engineType"), allow.cartesian=TRUE]

  # create table mapping location to fuel sulfur percentage and join to the EF table
  #EF<-data.table::data.table(Location=c("ECA","OutsideECA","GreatLakes"),
  #                           fuelSulfurLevel=c(ECAfuelSulfurPercentage,GlobalfuelSulfurPercentage,ECAfuelSulfurPercentage)
  #)[EF, on=c("Location")]

  # create table mapping fuel type to fuel sulfur percentage and join to the EF table
  # TODO: Consider this edit for PM??
  EF<-data.table::data.table(fuelType=c("MGO/MDO","RM/HFO","LNG"),
                             fuelSulfurLevel=c(ECAfuelSulfurPercentage,GlobalfuelSulfurPercentage,ECAfuelSulfurPercentage)
  )[EF, on=c("fuelType")]

  #Create Table of Input Values for engine type and location
  inputs<-data.table::data.table(engineType=engineType, Location=location, loadFactor=loadFactor)
  # assign unique value to each row so when the table gets exploded by merges later, we can aggregate back to original inputs
  inputs[, ID := .I] 

  #add load factor to inputs table
  #Use BSFC Baseline for auxiliary and boiler emission factors (by setting loadFactor to NA)
  if(main_aux_boiler!="main"|is.null(loadFactor)){
    inputs[,loadFactor:=NA]
  }

  #join inputs to emission factor table (Adds ID, and load Factor)
  EF<-EF[inputs,on=c("Location","engineType"),allow.cartesian=TRUE]

  #Updated BSFC According to Load Factor, Using Jalkanen 2012/IMO GHG 3
  EF[,BSFC_LoadFactor:=ShipEF::calcLoadBSFC(loadFactor = loadFactor,
                                    BSFC_Baseline = BSFC)
     ]


  #Calculate Emission Factor based on Fuel Sulfur percent
  EF<-EF[,so2:=BSFC_LoadFactor*2*0.97753*(fuelSulfurLevel/100)]

  #correct SO2 emission factors
  #EF[fuelType == "LNG",
  #   so2:=0.00269]
  EF[fuelType == "LNG",
     so2:=BSFC_LoadFactor*(2e-4)]

  #Weight Emission Factor According to Proportion of Fuel Assumed per Engine Type and Location
  EF<-EF[,.(so2=weighted.mean(so2,w=Proportion)),by=c("ID","Location","engineType")]

  so2<-EF[,c("so2"),with=FALSE]

  return(so2)
}
