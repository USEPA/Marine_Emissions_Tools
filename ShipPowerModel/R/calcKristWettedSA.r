#' @title calcKristWettedSA
#'
#'@description Calculate hull wetted surface area (\code{wettedSA}) (m^2)
#' using the Kristensen method.
#'
#' @param shipType Ship type (vector of strings, see \code{\link{calcShipType}}).
#' Must align with \code{paxTugShipTypes},
#'\code{tankerBulkCarrierGCargoShipTypes}, and \code{containerShipTypes}
#'groupings
#'@param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#'@param maxDraft Maximum summer load line draft (vector of numericals, m)
#'@param actualDraft Actual draft (vector of numericals, m)
#'@param lwl Waterline length (vector of numericals, m) (see \code{\link{calclwl}})
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param Cbw Waterline block coefficient (vector of numericals, dimensionless)
#'(see \code{\link{calcCbw}})
#'@param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can
#' supply either a vector of numericals, a single number, or rely on the default
#'@param paxTugShipTypes Ship types specified in input \code{shipTypes} to be
#'modeled as passengers and tug vessels (vector of strings)
#'@param tankerBulkCarrierGCargoShipTypes Ship types specified in input
#'\code{shipTypes} to be modeled as tankers, bulk carriers and general cargo
#'vessels (vector of strings)
#'@param containerShipTypes Ship types specified in input \code{shipTypes}to be
#'modeled as container ships (vector of strings)
#'
#' @details
#'
#' This method this requires ship types to be grouped. Use the
#' \code{paxTugShipTypes}, \code{tankerBulkCarrierGCargoShipTypes}, and
#' \code{containerShipTypes} grouping  parameters to provide these ship
#' type groupings. Any ship types not included in this grouping will be
#' considered as miscellaneous vessels.
#'
#'
#' Container ship wetted surface area:
#'
#' = 0.995(disp./lloyds draft + 1.9 * Lwl * lloyds draft)-2.4(lloyds draft-actual draft)(lwl+breadth)
#'
#' Bulk carrier, tanker, and general cargo ship wetted surface area:
#'
#' = 0.99(disp./lloyds draft + 1.9 * Lwl * lloyds draft)-2(lloyds draft- actual draft)(lwl+breadth)
#'
#' Cruise ships and tugs treated as twin screw RORO in Kristensen formulas on
#' the basis of sample of vessels from Lloyds and their number of propellers.
#'
#' Twin Screw RORO wetted surface area:
#'
#' = 1.21(disp./lloyds draft +1.3*lwl*lloyds draft)*(1.2-0.34*Cbw)-2.5(lloyds draft-actual draft)*(lwl+breadth)
#'
#' Auto carriers, miscellaneous, reefer, and RORO ships are treated as single
#' skeg ROROs in Kristensen formulas on the basis of sample of vessels from
#' Lloyds and their number of propellers.
#'
#' Single screw RORO wetted surface area:
#'
#' 0.87(disp./lloyds draft +2.7*lwl*lloyds draft)*(1.2-0.34*Cbw)-3.0(lloyds draft-actual draft)*(lwl+breadth)
#'
#' Note: Actual draft is typically obtained from sources such as AIS messages or ship records.
#'
#' @return \code{wettedSA} (vector of numericals, m^2)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'@seealso \itemize{
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcCbw}}
#'\item \code{\link{calcShipType}}
#'}
#'
#' @family Kristensen Calculations
#'
#' @examples
#' calcKristWettedSA("bulk.carrier", 80097, 13.6, 12.5, 218, 32.25, 0.8099003, 1.025)
#' calcKristWettedSA("other.tanker", 238942, 15.6, 14, 400, 49, 0.7490287, 1.025,
#'                   tankerBulkCarrierGCargoShipTypes=c("other.tanker","bulk.carrier"))
#' calcKristWettedSA("container.ship", 238942, 15.6, 14, 400, 49, 0.7490287, 1.025)
#'
#' @export

calcKristWettedSA <- function(shipType, maxDisplacement, maxDraft, actualDraft, lwl, breadth, Cbw, seawaterDensity=1.025,
                              tankerBulkCarrierGCargoShipTypes=c("general.cargo","tanker","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier"),
                              containerShipTypes=c("container.ship"),
                              paxTugShipTypes=c("ferry.pax","ferry.ro.pax","cruise","cruise.ed","yacht","tug","passenger","ro.ro")
                              ){


  wettedSA<- ifelse(#case 1
    shipType %in% containerShipTypes,
                    0.995*( maxDisplacement/(seawaterDensity*maxDraft) + 1.9*lwl*maxDraft)
                    - 2.4*(maxDraft-actualDraft)*(lwl+breadth)
                    ,
                    ifelse(#case2
                      shipType %in% tankerBulkCarrierGCargoShipTypes,
                            0.99*( maxDisplacement/(seawaterDensity*maxDraft) + 1.9 *lwl*maxDraft)
                            - 2*(maxDraft-actualDraft)*(lwl+breadth)
                            ,
                            ifelse(#case3
                              shipType %in% paxTugShipTypes,
                                  1.21*( maxDisplacement/(seawaterDensity*maxDraft) + 1.3*lwl*maxDraft)*(1.2-0.34*Cbw)
                                  - 2.5*(maxDraft-actualDraft)*(lwl+breadth)
                                  ,
                                  #miscelaneous case: reefer, roro, etc...
                                  0.87*( maxDisplacement/(seawaterDensity*maxDraft) + 2.7*lwl*maxDraft)*(1.2-0.34*Cbw)
                                  - 3.0*(maxDraft-actualDraft)*(lwl+breadth)
                                  )#end case 3
                          )#end case 2
                    )#end case 1

  return(wettedSA)
}
