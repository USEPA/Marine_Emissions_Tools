#' @title calcShipType
#'
#' @description
#' Assigns a generalizable ship type based on the \code{Vessel_Type} field in
#' ship registry data.
#'
#' @param vesselType Unstandardized vessel types from ship registry data
#' (vector of strings)
#' @param method Select the methodology that should be used (i.e., which set of
#' ship types to return): \itemize{\item"imo"\item"starcrest"}
#' @param inputTableLocation File path (optional). Used to specify a
#' user-supplied mapping between unstandardized \code{VesselType}s and
#' standardized \code{shipType}s. See details for formatting requirements.
#'
#' @details
#' There are different sets of ship types available for use: IMO's ship types
#' or Starcrest's ship types. Since ship types are important for assigning
#' default auxiliary and boiler engine assumptions, the methodology selected
#' here should match with the methodology used for assigning auxiliary and
#' boiler loads. The IMO ship types are based on the Third GHG Study, and the
#' Starcrest ship types are based on the 2017 Port of Los Angeles air emissions
#' inventory.
#'
#' If the default mapping between the unstandardized \code{VesselType}s and
#' standardized \code{shipType}s does not catch everything (i.e, if there are
#' unstandardized \code{VesselType}s that are not standardized by this function),
#' or if you want to use your own standardized \code{shipType}s, you can supply
#' a custom mapping file using the \code{inputTableLocation} argument.
#'
#'If user-supplied ship type mapping is used, the file should be
#'in .csv format with two columns and a header row. The headers should be
#'"Vessel_Type" and "shipType", and the values should specify which
#'\code{Vessel_Type}s map to which \code{shipType}s. See \code{data(shipMap)}
#'for the default mapping used by this function.
#'
#' For more information about ship type, see Section 3.3.3 of the
#' Port Emissions Inventory Guidance.
#'
#' @return \code{shipType}, a data.table column of the general ship type,
#' corresponding to the input data. \code{NA}s represent Vessel_Types not in
#' \code{shipMap} (or the \code{inputTableLocation} parameter, if that was used).
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
#' \item \code{\link{calcAuxBoilerLoad}}
#' \item \code{\link{calcSubType}}
#' }
#'
#' @examples
#' calcShipType(vesselType = c("Fully Cellular Container",
#'                             "Deck Cargo Carrier",
#'                             "Open Hatch Carrier",
#'                             "Cruise Ship",
#'                             "LNG Carrier")
#'              )
#'
#' calcShipType(vesselType = c("Fully Cellular Container",
#'                             "Deck Cargo Carrier",
#'                             "Open Hatch Carrier",
#'                             "Cruise Ship",
#'                             "LNG Carrier"),
#'              method="starcrest")
#'
#' @import data.table
#' @export


calcShipType<- function(vesselType, method="imo", inputTableLocation=NULL){

  #Ship Type aligns with sub type and auxiliary and boiler load assignment. Make sure all three match up in methodogoloy or input table method
  if(is.null(inputTableLocation)==FALSE){
    shipMap<-fread(inputTableLocation)
  }else{
  shipMap<-ShipPowerModel::shipMap
}

table<-data.table::data.table("Vessel_Type"=vesselType)

if(is.null(inputTableLocation)==FALSE){
  shipType<-shipMap[table, on="Vessel_Type"][,!"Vessel_Type",with=FALSE]
  }else if(method=="imo"){
shipType<-shipMap[table, on="Vessel_Type"][,"imoShipType",with=FALSE]
} else if(method=="starcrest"){
shipType<-shipMap[table, on="Vessel_Type"][,"starcrestShipType",with=FALSE]
}

return(data.table::as.data.table(shipType))
}
