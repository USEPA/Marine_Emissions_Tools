#' @title calcCm
#'
#' @description  Calculates the midship section coefficient (\code{Cm})
#' (dimensionless).
#'
#' @param shipType Ship type (vector of strings, see \code{\link{calcShipType}}). 
#' Must align with \code{tankerBulkCarrierShipTypes}, \code{tugShipTypes}, and 
#' \code{roroPaxShipTypes} groupings 
#' @param Cbw Waterline block coefficient (vector of numericals, dimensionless) 
#' (see \code{\link{calcCbw}})
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param actualDraft Actual draft (vector of numericals, m)
#' @param CmEquationType Equation type: \itemize{
#' \item"kristensen"
#' \item"benford"
#' \item"schneekluth"}
#' This argument is not vectorized, as it takes only a single string
#'@param tankerBulkCarrierShipTypes Ship types specified in input \code{shipTypes}
#'to be modeled as tankers and bulk carriers (vector of strings)
#'@param tugShipTypes Ship types specified in input \code{shipTypes} to be
#'modeled as tugs (vector of strings)
#'@param roroPaxShipTypes Ship types specified in input \code{shipTypes} to be
#'modeled as RORO and passenger ships (vector of strings)
#'
#' @details
#' The midship section coefficient calculation depends on the ship type, in
#' addition to the actual draft. Actual draft is typically obtained from sources
#' such as AIS messages or ship records.
#'
#' This function can calculate \code{Cm} using three different methods:
#' Kristensen, Benford, and Schneekluth. The Kristensen method requires ship
#' types to be grouped. Use the \code{tankerBulkCarrierShipTypes},
#' \code{tugShipTypes}, and \code{roroPaxShipTypes} parameters to provide these
#' ship type groupings. Any ship types not included in these groupings will be
#' considered as miscellaneous vessels.
#'
#' Use the \code{CmEquationType} parameter to indicate which method to use:\itemize{
#'
#'  \item "kristensen" (see Kristensen 2013 & 2017): \itemize{
#'    \item Bulk Carriers and Tankers: \code{Cm} = 0.995
#'    \item Passenger Vessels: \code{Cm} = 0.95
#'    \item Tugboats: \code{Cm} = 0.92
#'    \item Miscellaneous Vessels: \code{Cm} = 0.98
#'  }
#'
#'  \item "benford" (see Rakke 2016):
#'  \deqn{Cm=0.977+0.085*(Cbw-0.6)}
#'
#'  \item "schneekluth" (see Schneekluth 1998):
#'  \deqn{Cm=1.006-0.0056*Cbw^-3.65}
#'
#' }
#'
#' @return \code{Cm} (vector of numericals, dimensionless)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'Schneekluth, H. and Bertram, V. 1998. "Ship Design for Efficiency and Economy."
#'2nd ed. Oxford, Boston: Butterworth-Heinemann.
#'
#' @seealso \itemize{
#'   \item \code{\link{calcCbw}}
#'   \item \code{\link{calcShipType}}
#'  }
#'
#' @examples
#' calcCm(c("chemical.tanker","container.ship"),c(0.8,0.75),c(13.6,15.6),c(12.5,14.1),"kristensen")
#' calcCm(c("chemical.tanker","container.ship"),c(0.8,0.75),c(13.6,15.6),c(12.5,14.1),"kristensen",
#'        tankerBulkCarrierShipTypes=c("other.tanker"))
#'
#' @export


calcCm <-function(shipType,Cbw,maxDraft,actualDraft,CmEquationType,
                  tankerBulkCarrierShipTypes=c("tanker","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier"),
                  tugShipTypes=c("service.tug","tug"),
                  roroPaxShipTypes=c("ferry.pax","ferry.ro.pax","cruise","cruise.ed","yacht","ro.ro","passenger")
                  ){

  if(grepl("kristensen",tolower(CmEquationType))==TRUE){
    Cm<- ifelse(#case 1
          shipType %in% tankerBulkCarrierShipTypes
        ,
        0.995,
        ifelse(#case 2
          shipType %in% tugShipTypes,
          0.92,
          ifelse(#case 3
            shipType %in% roroPaxShipTypes,
            0.95,# passenger case
            0.98 # misc type case
          )#end of case 3

        )#end of case 2
      )#end of case 1

    #adjust for actual draft
    Cm<-1-maxDraft/actualDraft*(1-Cm)

  }else if(grepl("benford",tolower(CmEquationType))==TRUE){

    Cm<-0.977+0.085*(Cbw-0.6)

  }else if(grepl("schneekluth",tolower(CmEquationType))==TRUE){

    Cm<-1.006-0.0056*Cbw^-3.56

  }
  return(Cm)
}
