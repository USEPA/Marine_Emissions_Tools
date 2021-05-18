#'@title calclwl
#'
#'@description Estimate waterline length (\code{lwl}) (m) from length between
#'perpendiculars (\code{lbp}) (m).
#'
#'@param lbp Length between perpendiculars (vector of numericals, m)
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}}). 
#' Must align with
#'\code{tankerBulkCarrierGCargoShipTypes} and \code{roroPaxShipTypes} groupings
#'@param tankerBulkCarrierGCargoShipTypes Ship types specified in input
#' \code{shipTypes} to be modeled as tankers, bulk carriers and general cargo
#' vessels (vector of strings)
#'@param roroPaxShipTypes Ship types specified in input \code{shipTypes} to be
#'modeled as RORO and passenger ships (vector of strings)
#'
#'@details
#'This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierGCargoShipTypes} and \code{roroPaxShipTypes} grouping
#' parameters to provide these ship type groupings. Any ship types not included
#' in these groupings will be considered as miscellaneous vessels.
#'
#'@return \code{lwl} (vector of numericals, m)
#'
#'@references
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'@examples
#'calclwl(c("bulk.carrier","container.ship","cruise"), c(214.5,396,140))
#'
#'@export

calclwl<-function(shipType, lbp,
                  tankerBulkCarrierGCargoShipTypes=c("tanker","general.cargo","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier"),
                  roroPaxShipTypes=c("ro.ro","passenger","ferry.pax","ferry.ro.pax","cruise","yacht","cruise.ed")
                  ){
  lwl<- ifelse(shipType %in% tankerBulkCarrierGCargoShipTypes,#case 1
               1.02*lbp,
               ifelse(shipType%in%roroPaxShipTypes,#case 2
                      1.035*lbp,
                      1.01*lbp #not in tanker, bulk carrier, general cargo, or passenger
               )#end case1
  )#end case2

  return(lwl)
}
