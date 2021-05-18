#'@title calcKristCa
#'
#'@description Calculate incremental hull (roughness) resistance coefficient
#'(\code{Ca}) (dimensionless) using the Kristensen method.
#'
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}}).
#'Must align with \code{tankerBulkCarrierGCargoShipTypes} and
#' \code{containerShipTypes} groupings
#'@param actualDisplacement Actual loaded displacement (vector of numericals,
#' m^3) (see \code{\link{calcActualDisp}})
#'@param tankerBulkCarrierGCargoShipTypes Ship types specified in input
#'\code{shipTypes} to be modeled as tankers, bulk carriers and general cargo
#'vessels (vector of strings)
#'@param containerShipTypes Ship types specified in input \code{shipTypes} to be
#'modeled as container ships (vector of strings)
#'
#'@details
#'Models the effect of realistic hull roughness on resistance, which is not
#'captured in the frictional and residual resistance coefficients from tank
#'towing operations.
#'
#' This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierGCargoShipTypes}, \code{containerShipTypes} grouping
#' parameters to provide these ship type groupings. Any ship types not included
#' in these groupings will be considered as miscellaneous
#'
#'@return \code{Ca} (vector of numericals, dimensionless)
#'
#'@references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'@seealso \code{\link{calcActualDisp}}
#'
#'@family Kristensen Calculations
#'@family Resistance Calculations
#'
#'@examples
#'calcKristCa(c("bulk.carrier","container.ship"),c(73663.27,216726.45))
#'
#'@export


calcKristCa<-function(shipType,actualDisplacement,
                      tankerBulkCarrierGCargoShipTypes=c("tanker","general.cargo","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier"),
                      containerShipTypes=c("container.ship")){

Ca<- ifelse(shipType%in%containerShipTypes,
            (0.5*log10(actualDisplacement)-0.1*(log10(actualDisplacement))^2)/1000,
            ifelse(shipType%in%tankerBulkCarrierGCargoShipTypes,
  (pmax(-0.1, 0.5*log10(actualDisplacement)-0.1*(log10(actualDisplacement))^2))/1000,
  NA
)
)
return(Ca)
}
