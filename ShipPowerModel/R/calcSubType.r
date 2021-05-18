#' @title calcSubType
#'
#' @description Assign ship subtype according to the vessel's ship type and
#' size, based on the requested methodology.
#'
#' @param shipType Ship type (vector of strings) (see \code{\link{calcShipType}})
#' @param DWT Deadweight tonnage (metric tons) (vector of numericals)
#' @param GT Gross tonnage (metric tons) (vector of numericals)
#' @param TEU Twenty-foot equivalent units (TEUs) (vector of numericals)
#' @param nPassengers Maximum number of passengers (vector of numericals. Note:
#' only applicable when using the Starcrest method)
#' @param method Select the methodology that should be used (i.e., which set of
#' subtypes to return): \itemize{
#'   \item "imo"
#'   \item "starcrest"
#' }
#' Note: this argument is not used if \code{inputTableLocation} is used.
#' @param inputTableLocation File path (optional). Used to specify a user-supplied
#' set of subtypes. If used, this is typically the same as the auxiliary and
#' boiler load table so that correct loads are assigned. See details for
#' formatting requirements.
#'
#' @details
#' When passing ship size data in the \code{DWT}, \code{GT}, \code{TEU}, and
#' \code{nPassengers} parameters, use \code{NA} if the parameter is unknown or
#' not applicable (e.g., TEU doesn't apply for passenger vessels).
#'
#' This function has two sets of ship subtypes available for use: IMO's subtypes
#' or Starcrest's subtypes. Since ship subtypes are important for assigning
#' default auxiliary and boiler engine assumptions, the methodology selected
#' here should match with the methodology used for assigning auxiliary and
#' boiler loads. The IMO subtypes are based on the Third GHG Study, and the
#' Starcrest subtypes are based on the 2017 Port of Los Angeles Air Emissions
#' Inventory.
#'
#' If custom subtypes are desired, user-supplied subtypes could be specified
#' using the \code{inputTableLocation} parameter. This file should be in .csv
#' format with five columns and a header row (additional columns are acceptable).
#' The headers should be: \itemize{
#'   \item "shipType"
#'   \item "sizeMin"
#'   \item "sizeMax"
#'   \item "sizeUnits"
#'   \item "subType" }
#'
#' Valid values for "sizeUnits" are: \itemize{
#'   \item "Deadweight"
#'   \item "TEU"
#'   \item "Gross_Tonnage"
#'   \item "Number_of_Passengers" }
#'
#' \code{sizeMin} is inclusive and \code{sizeMax} is exclusive (i.e.,
#' \code{subType} is matched where \code{sizeMin} <= vessel size < \code{sizeMax}).
#'
#' See \code{data(IMOGHG3_AuxLoadFactor)} or \code{data(Starcres2016_AuxLoadFactor)}
#' for an example of how this table can be structured.
#'
#' For more information about ship subtypes, see Section 3.3.3 of the
#' Port Emissions Inventory Guidance.
#'
#' Note: IMO GHG 3 uses vehicle carrier subtypes which are based on number of
#' vehicles. This information is frequently unavailable in vessel characteristics
#' datasets. However, the auxiliary loads are assumed to be the same for all
#' vehicle carriers regardless of size, so these number of vehicle subtypes are
#' replaced by those used in the C3RIA.
#'
#' Note: IMO GHG 3 uses cubic meters (m^3) for liquified gas tankers. This
#' information is unavailable in vessel characteristics datasets, and it is
#' unclear which ship parameter value was used to determine these values.
#' Therefore, these vessel subtypes are defined by the same DWT bins as those
#' used for chemical tankers.
#'
#' @return \code{subType} (vector of strings)
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
#'\href{https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=P1005ZGH.TXT}{EPA. 2009.
#'"Regulatory impact analysis: Control of emissions air pollution from category
#'3 marine diesel engines." Ann Arbor, MI: Office of Transportation and Air
#'Quality. US Environmental Protection Agency.}
#'
#' @seealso \itemize{
#' \item \code{\link{calcAuxBoilerLoad}}
#' \item \code{\link{calcShipType}}
#' }
#'
#' @examples
#' calcSubType(shipType = c("reefer","bulk.carrier"),
#'             DWT = c(NA,56188.016),
#'             GT = c(NA,33511),
#'             TEU = c(NA,NA),
#'             nPassengers = NULL)
#'
#' @import data.table
#' @export

calcSubType<- function(shipType, DWT, GT, TEU, nPassengers=NULL, method="imo", inputTableLocation=NULL)
{
  #bind variables to make devtools::check() happy
  IMOGHG3_AuxLoadFactor<-Starcrest2016_AuxLoadFactor<-sizeMax<-sizeMin<-ID<-NULL

#Read in Sub Type Data
  if(is.null(inputTableLocation)==FALSE){
    SubTypes<-data.table::fread(inputTableLocation)
  }else if(method=="imo"){
    SubTypes<-ShipPowerModel::IMOGHG3_AuxLoadFactor
  }else if(method=="starcrest"){
    SubTypes<-ShipPowerModel::Starcrest2016_AuxLoadFactor
  }
#Since number of passenger (nPassengers) is only required for the starcrest method, it should not be a required input.
#therefore, if nPassengers is NULL it will be "created" as a NA column of same length as others
  if(is.null(nPassengers)){
    nPassengers<-c(rep(NA, length(shipType)))
  }

#Create table of required data, including ID column to make sure output is order of input data table
#sets names of size units to the names described in the imo sub types data frame
table<-data.table::data.table("ID"=seq(1,length(shipType)),
                       "shipType"=shipType,
                       "Deadweight"=as.numeric(DWT),
                       "Gross_Tonnage"=as.numeric(GT),
                       "TEU"=as.numeric(TEU),
                       "Number_of_Passengers"=as.numeric(nPassengers)
                       )
#For shiptypes with just one sub type, where the required size field is missing, set NAs to 0 so sub type can still be applied
table[shipType%in%SubTypes[subType==shipType]$shipType][is.na(table[shipType%in%SubTypes[subType==shipType]$shipType])]<-0

dfSubTypes<-SubTypes[table, on=c("shipType"), allow.cartesian=TRUE]

#split up analysis by size units
dfSubTypes<-split(dfSubTypes, dfSubTypes$sizeUnits)

for(i in 1:length(dfSubTypes)){

  #subset for size field that was within bounds
  dfSubTypes[[i]]<-dfSubTypes[[i]][(get(names(dfSubTypes)[i])<sizeMax &
                           get(names(dfSubTypes)[i])>=sizeMin)]
}

dfSubTypes<-data.table::rbindlist(dfSubTypes)

#Bind rows with missing required values for assigning sub type. SubType for these rows is NA
dfSubTypes<- rbind(dfSubTypes, cbind(table[!table$ID%in%dfSubTypes$ID][,names(dfSubTypes)[!names(dfSubTypes)%in%names(table)]:=NA]))

subType<-dfSubTypes[order(ID),"subType",with=FALSE]

return(subType)
}
