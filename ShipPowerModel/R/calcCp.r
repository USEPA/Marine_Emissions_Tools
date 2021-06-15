#' @title calcCp
#'
#'@description Calculates the prismatic coefficient (\code{Cp}) (dimensionless).
#'
#'@param Cm Midship section coefficient (vector of numericals, dimensionless) 
#'(see \code{\link{calcCm}})
#'@param Cbw Waterline block coefficient (vector of numericals, dimensionless) 
#'(see \code{\link{calcCbw}})
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}}). 
#' Must align with \code{roroPaxContainerShipTypes},
#'\code{gCargoShipTypes}, and \code{tankerBulkCarrierShipTypes} groupings
#'@param bounds Indicates which upper and lower bounds on \code{Cp} should be
#'applied:\itemize{
#'\item Pass "holtrop mennen" to use the bounds specified by Holtrop & Mennen
#'\item Pass "none" (default) to calculate \code{Cp} without upper or lower
#' bounds}
#' This argument is not vectorized. Either supply a single string or rely on the
#' default
#'@param roroPaxContainerShipTypes Ship types specified in input \code{shipTypes}
#'to be modeled as RORO, passenger and container ships (vector of strings)
#'@param gCargoShipTypes Ship types specified in input \code{shipTypes} to be
#'modeled as general cargo (vector of strings)
#'@param tankerBulkCarrierShipTypes Ship types specified in input
#'\code{shipTypes} to be modeled as tankers and bulk carriers (vector of strings)
#'
#' @details
#' \deqn{Cp = \frac{Cbw}{Cm}}{Cp=Cbw/Cm}
#'
#' This function can calculate \code{Cp} with or without upper and lower bounds.
#' If the Holtrop & Mennen bounds are applied, this requires ship types to be
#' grouped. Use the \code{roroPaxContainerShipTypes}, \code{gCargoShipTypes},
#' and  \code{tankerBulkCarrierShipTypes} parameters to provide these ship
#' type groupings. Any ship types not included in these groupings will be
#' considered as miscellaneous vessels.
#'
#' @return \code{Cp} (vector of numericals, dimensionless)
#'
#'@references
#'\href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#' Solutions. 2011. "Basic Principles of Propulsion."}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{calcCm}}
#'   \item \code{\link{calcCbw}}
#'   \item \code{\link{calcShipType}}
#' }
#'
#' @examples
#' calcCp(c(0.99,0.98), c(.8,.75),c("bulk.carrier","container.ship"),"none")
#'
#' @export

calcCp <-function(Cm, Cbw, shipType, bounds="none",
                  roroPaxContainerShipTypes=c("ro.ro","passenger","ferry.pax","ferry.ro.pax","cruise","cruise.ed","yacht","container.ship"),
                  gCargoShipTypes=c("general.cargo"),
                  tankerBulkCarrierShipTypes=c("tanker","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier")
){


  Cp <-if(bounds=="holtrop mennen"){
    ifelse(
      shipType%in%roroPaxContainerShipTypes,
      pmax(0.55,pmin(0.67,(Cbw/Cm))),
      ifelse(shipType%in%gCargoShipTypes,
             pmax(0.56,pmin(0.75,(Cbw/Cm))),
             ifelse(shipType%in%tankerBulkCarrierShipTypes,
                    pmax(0.85,pmin(0.73,(Cbw/Cm))),#case 3: bulk carrier & tanker,
                    Cbw/Cm
                    )
      )
    )
    }else{(Cbw/Cm)}

  return(Cp)
}
