#' @title calcKristThrustFactor
#'
#' @description Calculate the thrust deduction factor (\code{thrustFactor})
#'(dimensionless) using the Kristensen method.
#'
#' @param shipType Ship type (vector of strings, see \code{\link{calcShipType}}),
#' determined by Stat 5 code, \itemize{
#' \item"container.ship"
#' \item"bulk.carrier"
#' \item"tanker"
#' \item"general.cargo"
#' \item"vehicle.carrier"
#' \item"reefer"
#' \item"ro.ro"
#' \item"passenger"
#' \item"tug"
#' \item"misc"
#'}
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param lwl Waterline Length (vector of numericals, m) (see
#'\code{\link{calclwl}})
#'@param Cbw Waterline block coefficient (vector of numericals, dimensionless)
#' (see \code{\link{calcCbw}})
#'@param propDiam Propeller diameter (vector of numericals, m) (see
#' \code{\link{calcPropDia}})
#'@param M Fineness/slenderness coefficient (vector of numericals,
#' dimensionless) (see \code{\link{calcShipM}})
#'@param nProp Number of propellers (vector of numericals, see
#' \code{\link{calcPropNum}})
#' @param tankerBulkCarrierShipTypes Ship types specified in input
#'\code{shipTypes} to be modeled as tankers and bulk carriers vessels
#' (vector of strings)
#'
#' @details
#'Thrust deduction factor is a component of hull efficiency as well as a
#'component of propeller efficiency. It describes the increase in resistance on
#'the hull from water getting sucked back towards the propeller.
#'
#' This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierShipTypes} grouping  parameters to provide these ship
#' type groupings. Any ship types not included in this grouping will be considered
#' as miscellaneous vessels.
#'
#' @return \code{thrustFactor} (vector of numericals, dimensionless)
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
#'\item \code{\link{calcPropDia}}
#'\item \code{\link{calcShipM}}
#'\item \code{\link{calcPropNum}}
#'\item \code{\link{calcShipType}}
#'}
#'
#' @family Kristensen Calculations
#'
#' @examples
#' calcKristThrustFactor(c("bulk.carrier","container.ship"),
#'                       c(32.25,49),
#'                       c(218.75,400),
#'                       c(0.8,0.75),
#'                       c(6.7,9.6),
#'                       c(5.2,6.7),
#'                       c(1,1))
#'
#' calcKristThrustFactor("bulk.carrier", 32.25, 218.75, 0.8, 6.7,5.2,1)
#'
#' @export

calcKristThrustFactor <- function(shipType, breadth, lwl, Cbw, propDiam,M,nProp,
                                  tankerBulkCarrierShipTypes=c("tanker","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier")
){


  thrustFactor<-ifelse(nProp==1,
                       ifelse(shipType%in%tankerBulkCarrierShipTypes,
                              (#single prop case
                                #t1
                                #d
                          (((0.625*breadth)/lwl)+0.08)+
                            #e
                            (0.165-((0.25*breadth)/lwl))/
                            (#f
                              (825-((8060*breadth)/lwl)+
                                 20300*((breadth/lwl)^2))*
                                (0.98-Cbw)^3+1)+
                            #t2=0 since we assume F_a=0
                            #t3
                            (2*((propDiam/lwl)-0.04))
                        )#thrust correction
                        -0.26+0.04*M,
                        (#single prop case for non bulk carriers non tankers
                          #t1
                          #d
                          (((0.625*breadth)/lwl)+0.08)+
                            #e
                            (0.165-((0.25*breadth)/lwl))/
                            (#f
                              (825-((8060*breadth)/lwl)+
                                 20300*((breadth/lwl)^2))*
                                (0.98-Cbw)^3+1)+
                            #t2=0 since we assume F_a=0
                            #t3
                            (2*((propDiam/lwl)-0.04))
                        )),
                         (#twin prop case
                          0.0665+0.62833*(1.133*(Cbw^2)-0.797*Cbw+0.215)
                        )#end of twin prop case

            )#end elseif

   return(thrustFactor)


}
