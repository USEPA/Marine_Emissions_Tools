#' @title calcPropDia
#'
#' @description Calculate propeller diameter (\code{propDiam}) (m).
#'
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}}).
#' Must align with \code{tankerBulkCarrierGCargoShipTypes} and
#' \code{containerShipTypes} groupings
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param tankerBulkCarrierGCargoShipTypes Ship types specified in input
#' \code{shipTypes} to be modeled as tankers, bulk carriers and general cargo
#' vessels (vector of strings)
#' @param containerShipTypes Ship types specified in input \code{shipTypes} to
#' be modeled as container ships (vector of strings)
#'
#'@details
#'This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierGCargoShipTypes} and \code{containerShipTypes} grouping
#' parameters to provide these ship type groupings. Any ship types not included
#' in these groupings will be considered as miscellaneous vessels.
#'
#' @return \code{propDiam} (vector of numericals, m)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#' @examples
#' calcPropDia(c("bulk.carrier","container.ship"), c(13.6,15.6))
#' calcPropDia(c("other.tanker","container.ship"), c(13.6,15.6),
#'             tankerBulkCarrierGCargoShipTypes=c("other.tanker","bulk.carrier"))
#'
#' @export

calcPropDia <-function(shipType, maxDraft,
                       tankerBulkCarrierGCargoShipTypes=c("general.cargo","tanker","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier"),
                       containerShipTypes=c("container.ship")
                       ){


  propDiam<-ifelse( #case 1
    shipType %in% tankerBulkCarrierGCargoShipTypes,
    #if true return:
    0.395*maxDraft+1.3,
    #otherwise:
    ifelse( #case 2
      shipType%in%containerShipTypes,
      #if true return:
      0.623*maxDraft-0.16,
      #otherwise: (RoRo etc...)
      0.713*maxDraft-0.08
    )#end case 2
  )#end case 1

  return(propDiam)
}
