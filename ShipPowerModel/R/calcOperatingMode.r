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
#' @param berthArea Vector of named berth areas (NA if not at berth) or vector
#'                  of booleans (TRUE if at berth, FALSE if not)
#' @param anchorageArea Vector of named anchorage areas (NA if not at anchorage) 
#'                      or vector of booleans (TRUE if at berth, FALSE if not)
#'
#' @details
#' Calculated as a simplification of the methods described in Section 3.8.5 of
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
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Port Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@seealso
#'ShipPowerModel library
#'
#' @examples
#' calcOperatingMode(shipSpeed = c(0, 2.5, 14, 20), 
#'                   loadFactor = c(0.01, 0.0, 0.15, 0.75),
#'                   berthArea = c(TRUE, FALSE, FALSE, FALSE),
#'                   anchorageArea = c(FALSE, TRUE, FALSE, FALSE)
#'                   )
#'
#' @export

calcOperatingMode <- function(shipSpeed,loadFactor,berthArea,anchorageArea){
  opMode <- ifelse(!is.na(anchorageArea) & anchorageArea != FALSE & shipSpeed <= 3, "Anchorage",
                   # not in anchorage area and moving slowly
                   ifelse(!is.na(berthArea) & berthArea != FALSE & shipSpeed <= 1, "Berth",
                          # not in berth area and moving slowly
                          ifelse(shipSpeed > 1 & loadFactor <= 0.20, "Maneuvering",
                                 # not maneuvering
                                 # assume anything with a load factor >20% is in transit
                                 ifelse(loadFactor > 0.20, "Transit",
                                        # not in transit
                                        # vessel is going <= 1 knots with a load factor <= 0.20; assume anchorage
                                        "Anchorage"
                                        ) # end ifelse for transit
                                 ) # end ifelse for maneuvering
                          ) # end ifelse for Berth
                   ) # end ifelse for Anchorage

  return(opMode)
}
