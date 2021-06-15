#' @title calcAdmPwr
#'
#' @description Calculates ship power (kW) using the admiralty formula.
#'
#' @param totalInstalledPwr Total installed main engine power (vector of numericals,
#' kW) (maximum continuous rated power)
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see
#' \code{\link{calcSpeedUnitConversion}})
#' @param refSpeed Reference speed of the ship (service speed or maximum speed)
#' (vector of numericals, m/s)
#' @param actualDraft Actual draft (vector of numericals, m)
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param serviceMargin A service margin to account for weather and sea effects:
#' \itemize{\item At-sea operations = 15 (Default) \item Coastal operations = 10}
#' Can supply either a vector of numericals, a single number, or rely on the default.
#' @param n Exponential relationship applied to the ship speed ratio (dimensionless).
#' Default = 3. This argument is not vectorized, so supply a single number or rely
#' on the default
#' @param refSpeedType Indicates if the reference speed is service speed or
#' maximum speed: \itemize{
#' \item "serviceSpeed (Default)"
#' \item "maxSpeed"}
#' Can supply either a vector of strings, a single value, or rely on the default.
#'
#' @details
#' Ship speed and actual draft are typically obtained from sources such as AIS
#' messages or ship records.
#'
#' This implementation assumes the following default values:
#'
#' n = 3 (IMO greenhouse gas study)
#'
#' serviceMargin = 10 or 15, based on IMO (Prpic Orsic and Faltinsen, 2012).
#' 15 indicates 15\% increased resistance in at-sea water conditions, and 10
#' indicates 10\% increased resistance in coastal water conditions.
#'
#' Note that service margin is treated as an added resistance here
#' (instead of as a reduced efficiency) to maintain consistency with the other
#' power models used in this library. (See MAN, 2011).
#'
#' @return power (vector of numericals, kW)
#'
#' @references
#'International Maritime Organization. 2014. "Third IMO GHG study 2014 - Final
#'report." London: International Maritime Organization.
#'
#'\href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#' Solutions. 2011. "Basic Principles of Propulsion."}
#'
#'@seealso \itemize{
#'\item \code{\link{calcSpeedUnitConversion}}
#'\item \code{\link{calcPropPwr}}
#'\item \code{vignette("OverviewOfPowerModels", package="ShipPowerModel")}
#'\item \code{vignette("Admiralty.Formula.Example", package="ShipPowerModel")}
#'}
#'
#' @examples
#' calcAdmPwr(9363, seq(0,13,1), 15, 12.48, 13.57, serviceMargin = 15, n=3)
#'
#' @export

calcAdmPwr <- function(totalInstalledPwr, shipSpeed, refSpeed, actualDraft, maxDraft,
                           serviceMargin = 15, n=3,
                           refSpeedType = "serviceSpeed"){

  #The admiralty formula method requires the the reference speed to be maximum speed, to be consistent with the other terms.
  #Convert service speed to maximum speed by assuming service speed is 94% of maximum speed.
  refSpeed<-ifelse(tolower(refSpeedType) == "servicespeed",
                   refSpeed/0.94,
                   refSpeed
                  )#end ifelse

  power <- ifelse(shipSpeed==0,
                  0,
                  totalInstalledPwr*
                   pmin(1, #set maximum power to 100%
                        pmax(.02, #limit minimum power to 2%
                             ((actualDraft/maxDraft)^(2/3)*
                              (shipSpeed/refSpeed)^(n))*
                              ((100+serviceMargin)/100)
                            )
                       )
                 )#end ifelse
  return(power)

}#end of calcAdmPwr
