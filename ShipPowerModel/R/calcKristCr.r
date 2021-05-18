#' @title calcKristCr
#'
#' @description Calculate the residual resistance coefficient (\code{Cr})
#' (dimensionless) using the Kristensen method with Harvald regressions.
#'
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}})
#'@param M Fineness/slenderness coefficient (vector of numericals,
#'dimensionless) (see \code{\link{calcShipM}})
#'@param froudeNum Froude number (vector of numericals, dimensionless) (see
#'\code{\link{calcFroudeNum}})
#'@param actualDraft Actual draft (vector of numericals, m)
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param Cp Prismatic coefficient (vector of numericals, dimensionless) (see
#'\code{\link{calcCp}})
#'@param tankerBulkCarrierGCargoShipTypes Ship types specified in input
#'\code{shipTypes} to be modeled as tankers, bulk carriers and general cargo
#'vessels (vector of strings)
#'
#' @details
#'Note: Uses Guldhammer Residual Resistance Calculation and adjusts for bulbous bow, and
#'breadth/draft ratio.
#'
#' This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierGCargoShipTypes} grouping
#' parameters to provide these ship type groupings. Any ship types not include
#' this grouping will be considered as miscellaneous vessels.
#'
#' Actual draft is typically obtained from sources such as AIS messages or ship
#' records.
#'
#' @return \code{Cr} (vector of numericals, dimensionless)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'@seealso \itemize{
#'\item \code{\link{calcShipM}}
#'\item \code{\link{calcFroudeNum}}
#'\item \code{\link{calcCp}}
#'\item \code{\link{calcShipType}}
#'}
#'
#' @family Kristensen Calculations
#' @family Resistance Calculations
#'
#' @examples
#' calcKristCr("bulk.carrier", 5.2, 0.1199268, 12.48, 32.25, 0.81)
#'
#' @export

calcKristCr <- function(shipType, M, froudeNum, actualDraft, breadth, Cp,
                        tankerBulkCarrierGCargoShipTypes=c("tanker","general.cargo","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier")
){


E<- (
  (1.35-(0.23*M)+0.012*(M^2))+
    (1.5*(froudeNum^1.8))+
    ( 0.0011*(M^9.1)*(froudeNum^(2*M-3.7)))
)*
  (0.98+(2.5/((M-2)^4)))+
  ((M-5)^4)*((froudeNum-0.1)^4)

G<- ((7-0.09*((M)^2))*(((5*Cp)-2.5)^2))/
  ((600*((froudeNum-0.315)^2)+1)^1.5)

H<-exp(
  80*(froudeNum-(0.04+(0.59*Cp))-0.015*(M-5))
)

  #Calculate Cr
  #Source: Kristensen(2016) using Guldhammer(1978) method
  Cr<-((
        (1.35-(0.23*M)+0.012*(M^2))+
        (1.5*(froudeNum^1.8))+
        ( 0.0011*(M^9.1)*(froudeNum^(2*M-3.7)))
        )*
        (0.98+(2.5/((M-2)^4)))+
        ((M-5)^4)*((froudeNum-0.1)^4)+
        (
          ((7-0.09*((M)^2))*(((5*Cp)-2.5)^2))/
          ((600*((froudeNum-0.315)^2)+1)^1.5)
        )+
        exp(
            80*(froudeNum-(0.04+(0.59*Cp))-0.015*(M-5))
            )+
        ( 180*(froudeNum^3.7)*exp((20*Cp)-16))+
        #correction for vessels where B/T!=2.5
        (.16*((breadth/actualDraft)-2.5))
      )/1000

  #Apply Cr Correction for bulbous bow shape according to ship type.
  #Bulbous bows have become more common since these regressions were
  #made, they cut down on a lot of resistance
  Cr<- ifelse( shipType %in%tankerBulkCarrierGCargoShipTypes,

               Cr+ pmax(-0.4, -0.1-1.6*froudeNum)/1000,

               Cr+ pmax(-50,250*froudeNum-90)* (Cr/100)

                )#end ifelse

  return(Cr)

}

