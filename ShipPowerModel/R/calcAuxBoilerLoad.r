#' @title calcAuxBoilerLoad
#'
#' @description
#' Assigns auxiliary or boiler engine loads (kW) according to ship type, sub
#' type, and operating mode, based on the requested methodology.
#'
#' @param opMode Operating mode (vector of strings) (see
#' \code{\link{calcOperatingMode}})
#' @param shipType Ship type (vector of strings) (see
#' \code{\link{calcShipType}})
#' @param subType Ship subtype (vector of strings) (see
#' \code{\link{calcSubType}})
#' @param method Select the methodology that should be used (i.e., which default
#' loads to use): \itemize{\item"imo"\item"starcrest"}
#' @param output Is this assignment for an auxiliary or boiler engine? Default
#'= "aux". Options: \itemize{
#' \item "aux"
#' \item "boiler"
#'}
#' Note: this argument is not used if \code{inputTableLocation} is used.
#' @param inputTableLocation File path (optional). Used to specify a user-supplied
#' set of auxiliary or boiler loads. If used, this is typically the same table
#' as the ship subtype table so that correct loads are assigned. See details for
#' formatting requirements.
#'
#' @details
#' This function has two sets of default auxiliary and boiler load assumptions
#' available for use: those consistent with IMO's subtypes or Starcrest's
#' subtypes. Since ship subtypes are used when assigning default auxiliary and
#' boiler engine load assumptions, the methodology selected here should match
#' with the methodology used for assigning ship subtypes. The IMO subtypes are
#' based on the Third GHG Study, and the Starcrest subtypes are based on the
#' 2017 Port of Los Angeles Air Emissions Inventory.
#'
#' If user-supplied auxiliary and/or boiler loads are available, or if custom
#' subtypes are used, these can be specified using the \code{inputTableLocation}
#' parameter. This file should be in .csv format with six columns and a header
#' row (additional columns are acceptable).
#' The headers should be: \itemize{
#'   \item "shipType"
#'   \item "subType"
#'   \item "Transit"
#'   \item "Maneuvering"
#'   \item "Berth"
#'   \item "Anchorage" }
#'
#' The Transit, Maneuvering, Berth, and Anchorage columns should have values in
#' kW, and should contain either auxiliary *or* boiler loads. If user-supplied
#' loads are available for *both* auxiliary *and* boiler loads, these will need
#' to be saved in separate files, and this function will be called twice (once
#' with each file).
#'
#' For more information about default auxiliary and boiler loads, see Section
#' 3.6 and Appendix E of the Port Emissions Inventory Guidance.
#'
#' @return Auxiliary or boiler loads (kW) (vector of numericals)
#'
#' @references
#'International Maritime Organization. 2014. "Third IMO GHG study 2014 - Final
#'report." London: International Maritime Organization.
#'
#' \href{https://www.portoflosangeles.org/environment/air-quality/air-emissions-inventory}{
#' Starcrest Consulting Group. 2018. "Port of Los Angeles Air Emissions
#' Inventory - 2017." APP 171019-517 A.}
#'
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Port Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#' @seealso \itemize{
#' \item \code{\link{calcOperatingMode}}
#' \item \code{\link{calcShipType}}
#' \item \code{\link{calcSubType}}
#' }
#'
#' @examples
#' calcAuxBoilerLoad(opMode = c("Berth","Maneuvering"),
#'                   shipType = c("ro.ro","container.ship"),
#'                   subType = c("ro.ro","container.ship.3000"),
#'                   method="starcrest",
#'                   output="boiler")
#'
#' @import data.table
#' @importFrom utils data
#' @importFrom utils tail
#' @export

calcAuxBoilerLoad<- function(opMode, shipType, subType, method="imo", output="aux", inputTableLocation=NULL){

  #bind variables to make devtools::check() happy
  IMOGHG3_AuxLoadFactor<-IMOGHG3_BoilerLoadFactor<-Starcrest2016_AuxLoadFactor<-
    Starcrest2016_BoilerLoadFactor<-Starcrest2016_BoilerLoadFactor<-NULL


#Read in data
  if(is.null(inputTableLocation)==FALSE){
    Loads<-data.table::fread(inputTableLocation)
  }else if(method=="imo"){
    if(output=="aux"){
    Loads<-ShipPowerModel::IMOGHG3_AuxLoadFactor
    } else if(output=="boiler"){
    Loads<-ShipPowerModel::IMOGHG3_BoilerLoadFactor
    }
  }else if(method=="starcrest"){
    if(output=="aux"){
    Loads<-ShipPowerModel::Starcrest2016_AuxLoadFactor
  } else if(output=="boiler"){
    Loads<-ShipPowerModel::Starcrest2016_BoilerLoadFactor
  }
  }

table<-data.table::data.table("shipType"=shipType,
                              "subType"=subType,
                              "opMode"=opMode)

Loads<-Loads[table, on=c("shipType","subType")]

for(mode in unique(Loads$opMode)){

  Loads[opMode==mode,AuxBoiler:=as.numeric(get(mode))]
}

AuxBoiler<- Loads$AuxBoiler
return(AuxBoiler)
}
