#'@title calcTEU
#'
#' @description Estimate vessel size in twenty-foot equivalent units (TEUs) from
#' deadweight tonnage (\code{dwt}) for container ships.
#'
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}}),
#'determined by Stat 5 code, \itemize{
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
#' @param dwt Ship maximum deadweight tonnage (vector of numericals, tonnage)
#'
#' @return TEU (vector of numericals)
#'
#' @references
#'\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
#'"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
#'
#' @examples
#' calcTEU(c("bulk.carrier","container.ship"), c(70000,191144))
#'
#' @export

calcTEU<- function(shipType, dwt){
  TEU<-ifelse(shipType=="container.ship",
         ifelse( #case 1 Feeder
           dwt <= 35000,((dwt/15.19)^(1/0.9814)),
           ifelse(#case 2 Panamax
             dwt <= 60000,((dwt/28.81)^(1/0.902)),
             ((dwt/37)^(1/0.875))
           )
         ),0
  )
  return(TEU)
}
