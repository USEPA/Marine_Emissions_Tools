#'@title calcKristCaa
#'
#'@description Calculate air resistance (\code{Caa}) (dimensionless) using the
#'Kristensen method.
#'
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}}). 
#'Must align with \code{tankerBulkCarrierGCargoShipTypes} and
#' \code{containerShipTypes} groupings
#'@param dwt Ship maximum deadweight tonnage (vector of numericals, tonnage)
#'@param tankerBulkCarrierGCargoShipTypes Ship types specified in input
#'\code{shipTypes} to be modeled as tankers, bulk carriers and general cargo
#'vessels (vector of strings)
#'@param containerShipTypes Ship types specified in input \code{shipTypes} to be
#'modeled as container ships (vector of strings)
#'
#' @details
#' Models the effect of realistic hull roughness on resistance, which is not
#' captured in the frictional and residual resistance coefficients from tank
#' towing operations.
#'
#' This method this requires ship types to be grouped. Use the
#' \code{tankerBulkCarrierGCargoShipTypes}, \code{containerShipTypes} grouping
#' parameters to provide these ship type groupings. Any ship types not included
#' in these groupings will be considered as miscellaneous vessels.
#'
#' NOTE: within the container ship section of this calculation, estimations are
#' made of the ships TEU:\itemize{
#' \item Feeder: TEU = ((dwt/15.19)^(1/0.9814))
#' \item Panamax: TEU = ((dwt/28.81)^(1/0.902))
#' \item PostPanamax: TEU = ((dwt/37)^(1/0.875))
#' }
#' These come from: H.O. Kristensen (2016), "Revision of statistical analysis and determination of
#' regression formulas for main dimensions of container ships based on data from Clarkson".
#' Kristensen's SHIP DESMO model also uses another dwt/teu relation for post-panamax container ships
#' with breadth > 49m. This equation has no inverse to map dwt to TEU and thus all post panamaxs are
#' assumed to have the same dwt/teu relation for all breadths, described above.
#'
#' @return \code{Caa} (vector of numericals, dimensionless)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'Kristensen, H. O. 2016. "Revision of statistical analysis and determination of
#'regression formulas for main dimensions of container ships based on data from
#'Clarkson."
#'
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#'@family Kristensen Calculations
#'@family Resistance Calculations
#'
#' @examples
#' calcKristCaa(c("bulk.carrier","container.ship","other.tanker"),c(70000,191144,20000))
#' calcKristCaa(c("bulk.carrier","container.ship","other.tanker"),c(70000,191144,20000),
#'              tankerBulkCarrierGCargoShipTypes=c("bulk.carrier","other.tanker"))
#'
#' @export



calcKristCaa<-function(shipType,dwt,
                       tankerBulkCarrierGCargoShipTypes=c("tanker","general.cargo","chemical.tanker","liquified.gas.tanker","oil.tanker","other.tanker","bulk.carrier"),
                       containerShipTypes=c("container.ship")
){
  Caa<- ifelse(shipType%in%tankerBulkCarrierGCargoShipTypes,
               ifelse(#case 1:"Small","Handysize","Handymax",
                 dwt <= 55000,0.07,
                 ifelse(#case 2:"Panamax","Aframax","Suezmax"
                   dwt <= 200000,0.05,
                   #case 3: "VLCC","VLBC"
                   0.04
                 )#end case 3
               )# end case 2
               # end case 1
  #end non container ship group
  ,ifelse(shipType %in% containerShipTypes,#start container ship group
  ifelse( #case 1 Feeder
    dwt <= 35000, pmax(0.28*((dwt/15.19)^(1/0.9814))^-0.126,0.09),
    ifelse(#case 2 Panamax
      dwt <= 60000, pmax(0.28*((dwt/28.81)^(1/0.902))^-0.126,0.09),
      pmax(0.28*((dwt/37)^(1/0.875))^-0.126,0.09) #PostPanamax
    )# end case 2
  ),
  NA# end case 1
  #end container ship group
                            )
  )

  return(Caa/1000)
}
