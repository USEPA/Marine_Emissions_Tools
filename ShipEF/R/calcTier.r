#' @title calcTier
#'
#' @description
#' Estimate engine NOx tier from the vessel's keel-laid year for C3 vessels.

#'@param engineType Engine type (vector of strings) (see
#'\code{\link{calcEngineType}}). Valid values are: \itemize{
#'\item "SSD"
#'\item "MSD"
#'\item "MSD-ED"
#'\item "GT"
#'\item "GT-ED"
#'\item "ST"
#'\item "LNG"
#'}
#' @param keelLaidYear Keel-laid year (vector of ints). If not available,
#' interpolate from build date
#' @param shipCategory Ship category (vector of ints) (see
#' \code{\link{calcShipCategory}}). Valid values are:
#' \itemize{
#'  \item 1
#'  \item 2
#'  \item 3
#' }
#'
#' @details
#' This function is designed to determine the engine NOx tier for C3 vessels.
#' Therefore, it will return "None" for C1 or C2 vessels.
#'
#' "Tier 0" is returned for steam turbine, gas turbine, and LNG engines, as well
#' as for diesel engines that were keel-laid prior to Tier 1 taking effect
#' (2000).
#'
#' "Tier 1", "Tier 2", and "Tier 3" refer to Tier I, Tier II, and Tier III,
#' respectively.
#'
#' @return
#' \code{tier}, a factorized vector with possible levels of: \itemize{
#'  \item "None" (used for C1/C2 vessels)
#'  \item "Tier 0"
#'  \item "Tier 1"
#'  \item "Tier 2"
#'  \item "Tier 3"
#' }
#
#' @references
#'\href{https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=P1005ZGH.TXT}{EPA. 2009.
#'"Regulatory impact analysis: Control of emissions air pollution from category
#'3 marine diesel engines." Ann Arbor, MI: Office of Transportation and Air
#'Quality. US Environmental Protection Agency. Pg 3-118:119}
#'
#'@seealso \itemize{
#' \item \code{\link{calcEngineType}}
#' \item \code{\link{calcShipCategory}}
#'}
#'
#' @examples
#' calcTier(c("MSD","GT","LNG","SSD"),c(1985, 2011, 2001,2004),c(3,3,3,2))
#'
#' @export


calcTier<-function(engineType, keelLaidYear,shipCategory){
  tier<- ifelse(shipCategory==3,
            ifelse(engineType%in%c("ST","GT","GT-ED","LNG") | keelLaidYear<2000,
                   "Tier 0",
                   ifelse(keelLaidYear<2011,
                          "Tier 1",
                          ifelse(keelLaidYear<2016,
                                 "Tier 2",
                                 "Tier 3")))
                ,
    "None"
  )
  tier<-as.factor(tier)
  return(tier)
}
