#'@title calcOperatingMode
#'
#' @description
#' Assigns an operating mode designation according to ship speed and engine load
#' fraction.
#'
#' @param shipSpeed Vessel speed (vector of numericals, knots)
#' @param loadFactor Fractional percentage (between 0 and 1) of main engine
#' required to propel vessel at given speed (vector of numericals) (see
#' ShipPowerModel library)
#'
#' @details
#' Calculated according to methods in IMO GHG 3.
#'
#' Note: This is a simplification of the methods described in Section 3.8.5 of
#' the Port Emissions Inventory Guidance.
#'
#' @return \code{opMode} (vector of strings). Valid values are: \itemize{
#' \item "Berth"
#' \item "Anchorage"
#' \item "Maneuvering"
#' \item "Transit"
#' }
#'
#' @references
#'International Maritime Organization. 2014. "Third IMO GHG study 2014 - Final
#'report." London: International Maritime Organization.
#'
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Port Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@seealso
#'ShipPowerModel library
#'
#' @examples
#' calcOperatingMode(shipSpeed=c(0,2.5,14,20), loadFactor=c(0.02,0.06,0.15,0.75))
#'
#' @export



calcOperatingMode<- function(shipSpeed,loadFactor){
opMode<-ifelse(shipSpeed<=1,
        "Berth",
          ifelse(shipSpeed<=3,
                 "Anchorage",
                    ifelse(loadFactor<=.2,
                      "Manuevering",
                      "Transit"
                    )
                 )
        )

return(opMode)
}
