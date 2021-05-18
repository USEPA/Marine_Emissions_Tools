#' @title calcLoadBSFC
#'
#' @description Calculates Brake Specific Fuel Consumption (BSFC) or Specific Oil
#' Fuel Consumption (SFOC) using a parabolic relationship with the main engine
#' load factor.
#'
#' @param loadFactor Fraction of total installed propulsive power representing
#' the instantaneous or average propulsive power (unitless) (vector of
#' numericals) (see ShipPowerModel library). If \code{NA} is supplied, the
#' baseline BSFC value is returned (i.e., the resulting emission factor will be
#' independent of engine load).
#' @param BSFC_Baseline Baseline BSFC value (g/kWh) (vector of numericals)
#'
#' @return \code{BSFC} (g/kWh) (vector of numericals)
#'
#' @references
#'International Maritime Organization. 2014. "Third IMO GHG study 2014 - Final
#'report." London: International Maritime Organization.
#'
#' \href{https://doi.org/10.5194/acp-12-2641-2012}{Jalkanen, J.-P., Johansson
#' L., Kukkonen, J., Brink, A., Kalli, J., and Stipa , T. 2012. "Extension of an
#' assessment model of ship traffic exhaust emissions for particulate matter and
#' carbon monoxide. Atmospheric Chemistry and Physics 12, no. 5.}
#'
#' @examples
#' calcLoadBSFC(c(NA, 0.5, 0.7),c(185,195,300))
#'
#' @export

calcLoadBSFC<- function(loadFactor=NULL, BSFC_Baseline){
  BSFC<-ifelse(is.na(loadFactor),
             BSFC_Baseline,
             BSFC_Baseline*(0.455*((loadFactor)^2)-0.71*loadFactor+1.28)
             )
return(BSFC)
}
