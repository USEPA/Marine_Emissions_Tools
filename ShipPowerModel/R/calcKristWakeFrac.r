#' @title calcKristWakeFrac
#'
#'@description Calculate wake fraction (\code{wakeFraction}) (dimensionless)
#'using the Kristensen method.
#'
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}})
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param lwl Waterline length (vector of numericals, m) (see
#'\code{\link{calclwl}})
#'@param Cbw Waterline block coefficient (vector of numericals, dimensionless)
#'(see \code{\link{calcCbw}})
#'@param propDiam Propeller diameter (vector of numericals, m) (see
#'\code{\link{calcPropDia}})
#'@param M Fineness/slenderness coefficient (vector of numericals,
#'dimensionless) (see \code{\link{calcShipM}})
#'@param nProp Number of propellers (vector of numericals, see
#'\code{\link{calcPropNum}})
#'@param tankerBulkCarrierShipTypes Ship types specified in input
#'\code{shipTypes} to be modeled as tankers and bulk carriers vessels (vector of
#' strings)
#'
#' @return \code{wakeFraction} (vector of numericals, dimensionless)
#'
#' @details
#' "The speed of advance of the propeller relative to the water in which it is working is
#'  lower than the observed speed of the vessel. This difference in speed,
#'  expressed as a percentage of the ship speed, is known as the wake fraction coefficient".
#'  \url{https://www.wartsila.com/encyclopedia/term/wake-fraction-coefficient}
#'
#'Wake fraction is a component of hull efficiency as well as a component of
#'propeller efficiency.
#'
#' This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierShipTypes} grouping  parameters to provide these ship
#' type groupings. Any ship types not included in this grouping will be considered as
#' miscellaneous vessels.
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
#'\item \code{\link{calcShipType}}}
#'
#' @family Kristensen Calculations
#'
#' @examples
#' calcKristWakeFrac("bulk.carrier",32.25,218.75, 0.8, 6.7,5.2, 1)
#' calcKristWakeFrac("oil.tanker",32.25,218.75, 0.8, 6.7,5.2, 1)
#' calcKristWakeFrac(c("bulk.carrier","container.ship"),
#'                   c(32.25,49),
#'                   c(218.75,400),
#'                   c(0.8,0.75),
#'                   c(6.7,9.6),
#'                   c(5.2,6.7),
#'                   c(1,1))
#'
#' @export

calcKristWakeFrac <- function(shipType, breadth,lwl, Cbw, propDiam,M, nProp,
                              tankerBulkCarrierShipTypes=c("tanker","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier")
){

  wakeFraction<-ifelse(nProp==1,
            #a
            ((((0.1*breadth)/
                 lwl)+0.149)+
               (
                 #b
                 (((0.05*breadth)/
                     lwl)+0.449)/
                   #c
                   (
                     (585-((5027*breadth)/
                             lwl)+
                        11700*((breadth/
                                  lwl)^2)
                     )*
                       ((0.98-Cbw)^3)+1)))+
              # Assume w2 = 0 because we assume F_a = 0 where
              #F_a = [-2,0,2] and F_a=0 represents a N-shaped hull form
              #take this assumption for lack of better information
              #w3
              pmin(0.1,-0.18+(0.00756/((propDiam/lwl)+
                                         0.002))),
            #nprop == 2 case
            1.133*(Cbw^2)-0.797*Cbw+0.215
            )

  #apply adjustments from Kristensen, 2016 (to correct Harvald's algorithms toward data in more recent model tests)
  wakeFraction<-ifelse( (nProp==1) & ( shipType %in% tankerBulkCarrierShipTypes), #End of case definition
             (0.7*wakeFraction-0.45+0.08*M),#second case= container ships
             0.7*wakeFraction
            )

  return(wakeFraction)

}# end of function: calcWakeFrac
