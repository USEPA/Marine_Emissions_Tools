#'@title calcEF_PM
#'
#'@description
#'Calculates the appropriate particulate matter (PM10 or PM2.5) emission factor
#'(g/kWh) for the given parameters.
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
#'@param loadFactor Fractional percentage (between 0 and 1) of main engine
#' required to propel vessel at given speed (vector of numericals) (see
#' ShipPowerModel library). This parameter is optional. By default, it is not
#' used and the resulting emission factor is independent of engine load.
#'@param ECAfuelSulfurPercentage Fuel sulfur cap (percentage by weight) for the
#'Emissions Control Area (ECA). Default = 0.1\% (in effect Jan. 1, 2015)
#'@param GlobalfuelSulfurPercentage Fuel sulfur cap (percentage by weight) for
#'outside the Emissions Control Area (ECA). Default = 0.5\% (in effect Jan. 1,
#'2020)
#'@param pmSize Indicates whether output is for PM10 or PM2.5. Valid values are:
#'\itemize{\item"pm10" \item"pm2.5"}
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
#'For more information about calculating PM emission factors, see Section 3.5.3
#'of the Port Emissions Inventory Guidance.
#'
#'@return \code{EF_PM} (g/kWh) (vector of numericals)
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
#'calcEF_PM(engineType = c("SSD","MSD","MSD-ED","SSD"),
#'           location = c("ECA","OutsideECA","GreatLakes","ECA"),
#'           loadFactor = c(0.02,0.3,0.8,1),
#'           pmSize = "pm2.5",
#'           main_aux_boiler = "main")
#'calcEF_PM(engineType = c("SSD","MSD","MSD-ED","SSD"),
#'           location = c("ECA","OutsideECA","GreatLakes","ECA"),
#'           loadFactor = NULL,
#'           pmSize = "pm2.5",
#'           main_aux_boiler = "main")
#'calcEF_PM(engineType = c("HSD","MSD","LNG"),
#'           location = c("ECA","ECA","OutsideECA"),
#'           pmSize = "pm10",
#'           main_aux_boiler = "aux")
#'calcEF_PM(engineType = c("MSD","Boiler"),
#'           location = c("ECA","OutsideECA"),
#'           pmSize = "pm10",
#'           main_aux_boiler = "boiler")
#'
#'@import data.table
#'@importFrom utils data
#'@importFrom utils tail
#'@importFrom stats weighted.mean
#'@export


calcEF_PM<-function(engineType,
                     location,
                     loadFactor=NULL,
                     ECAfuelSulfurPercentage=0.1,
                     GlobalfuelSulfurPercentage=0.5,
                     pmSize="pm10",
                     main_aux_boiler="main"
                     )
{
  #bind variables to make devtools::check() happy
  MainBSFC<-MainFuelMixTable<-AuxBSFC<-AuxFuelMixTable<-BoilerBSFC<-BoilerFuelMixTable<-
    BSFC_LoadFactor<-BSFC_LoadFactor<-BSFC<-pm10<-PMnom<-PMnom<-fuelSulfurLevel<-
    FSC<-MWR<-pm10EF<-.<-Proportion<-NULL

  #Read In Emission Factor DataFrames
  if(main_aux_boiler=="main"){
    EFBSFC<-ShipEF::MainBSFC
    fuelMixTable<-ShipEF::MainFuelMixTable
  }else if(main_aux_boiler=="aux"){
    EFBSFC<-ShipEF::AuxBSFC
    fuelMixTable<-ShipEF::AuxFuelMixTable
  }else if(main_aux_boiler=="boiler"){
    EFBSFC<-ShipEF::BoilerBSFC
    fuelMixTable<-ShipEF::BoilerFuelMixTable
  }

  EFSulfurEQCoefficients<-ShipEF::EFSulfurEQCoefficients

  EF_PM_ST_GT_LNG<-ShipEF::EF_PM_ST_GT_LNG

  #==================================================================
  #EngineType doesn't matter for boilers
  if(main_aux_boiler=="boiler"){engineType<-rep("Boiler",length(engineType))}

  #join, fuel sulfur coefficients, with fuel mix table, and BSFC table
  EF<-EFSulfurEQCoefficients[fuelMixTable[EFBSFC,
                                          on=c("fuelType","engineType"),
                                          allow.cartesian=TRUE],
                            on=c("fuelType"),
                            allow.cartesian=TRUE]

  # create table mapping location to fuel sulfur percentage and join to the EF table
  EF<-data.table::data.table(Location=c("ECA","OutsideECA","GreatLakes"),
                             fuelSulfurLevel=c(ECAfuelSulfurPercentage,GlobalfuelSulfurPercentage,ECAfuelSulfurPercentage)
                            )[EF, on=c("Location")]

  #Create Table of Input Values for engine type and location
  df<-data.table::data.table(ID=seq(1,length(engineType)),
                             engineType=engineType,
                             Location=location)

  #add load factor to inputs table
  #Use BSFC Baseline for auxiliary and boiler emission factors (by setting loadFactor to NA)
  if(main_aux_boiler!="main"|is.null(loadFactor)){
    df[,loadFactor:=NA]
  }else{df[,loadFactor:=loadFactor]}

  #join inputs to emission factor table (Adds ID, and load Factor)
  EF<-EF[df,
         on=c("engineType","Location"),
         allow.cartesian=TRUE]

  #Add BSFC_LoadFactor using BSFC and Load Factor, Using Jalkanen 2012/IMO GHG 3 Method
  EF[,BSFC_LoadFactor:=calcLoadBSFC(loadFactor = loadFactor,
                                    BSFC_Baseline = BSFC)
     ]

  #Calculate EFs for MSD and SSD engines
  # NOTE: This equation has been modified from the C3 RIA equation to appropriately respond to changing the BSFC
  EF<-EF[,pm10:=PMnom + fuelSulfurLevel*BSFC_LoadFactor*FSC*MWR*0.0001]

  #Fills In Emission Factors for Non-Calculated EFs
  EF<-EF_PM_ST_GT_LNG[EF, on=c("engineType", "fuelType")]
  EF<-EF[is.na(pm10EF)==FALSE, pm10:=pm10EF]

  #Calculate Emission Factors Weighted By Engine Type and Location Fuel Use Proportions
  EF<-EF[,.(pm10=weighted.mean(pm10,w=Proportion),
            pm2.5=weighted.mean(0.92*pm10,w=Proportion)
  ),by=c("ID","Location","engineType")]


  if(tolower(pmSize)==c("pm10")){
    return(EF[,c("pm10"),with=FALSE])
  }else if(tolower(pmSize)==c("pm2.5")){
    return(EF[,c("pm2.5"),with=FALSE])
  }
}
