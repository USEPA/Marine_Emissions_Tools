#' @title calcResistanceShipPwr
#'
#' @description Calculate ship power (kW) for resistance-based power models.
#'
#' @param Rtot Total ship resistance (vector of numericals, kN) (see
#' \code{\link{calcKristTotalRes}} or \code{\link{calcHMTotalRes}})
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see
#'  \code{\link{calcSpeedUnitConversion}})
#' @param hullEff Hull efficiency (vector of numericals, dimensionless) (see
#' \code{\link{calcHullEff}})
#' @param openWaterEff Open water efficiency (vector of numericals,
#' dimensionless) (see \code{\link{calcOpenWaterEff}})
#' @param totalInstalledPwr Total installed main engine power (vector of
#' numericals, kW) (maximum
#' continuous rated power)
#' @param shaftEff Shaft efficiency (dimensionless). Default = 0.98.
#'  Ratio of power delivered to the propeller and the brake power delivered by
#'  the engine. Can supply either a vector of numericals, a single
#'  number, or rely on the default
#' @param relRotationEff Relative rotational efficiency (dimensionless).
#'  Default = 1. Accounts for effect of rotational flow of water around propeller.
#'  Can supply either a vector of numericals, a single number, or rely on the default
#' @param pwrUpperBoundPercent Percent of total installed power at which
#' required power is capped (1 indicates required power cannot exceed
#' \code{totalInstalledPwr}). Can supply either a vector of numericals, a single
#'  number, or rely on the default
#' @param pwrLowerBoundPercent Percent of total installed power to act as lower
#' bound for required power (0.02 indicates required power cannot go below 2\%
#' of \code{totalInstalledPwr}). Can supply either a vector of numericals, a
#' single number, or rely on the default
#'
#' @return power (vector of numericals, kW)
#'
#' @details
#' This function is called by both Holtrop & Mennen (\code{\link{calcHMPwr}})
#' and Kristensen (\code{\link{calcKristPwr}}) power calculations, where each
#' uses has its own total resistance calculation (\code{\link{calcHMTotalRes}}
#' and \code{\link{calcKristTotalRes}}).
#'
#' Assumptions for shaft efficiency and relative rotational efficiency are based
#' on MAN.
#'
#' @references
#'
#'\href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#' Solutions. 2011. "Basic Principles of Propulsion."}
#'
#' @seealso
#' \itemize{
#' \item \code{\link{calcKristTotalRes}}
#' \item \code{\link{calcHMTotalRes}}
#' \item \code{\link{calcSpeedUnitConversion}}
#' \item \code{\link{calcHullEff}}
#' \item \code{\link{calcOpenWaterEff}}
#' \item \code{\link{calcHMPwr}}
#' \item \code{\link{calcKristPwr}}
#' }
#'
#' @examples
#' calcResistanceShipPwr(398.487,10.8, 1.191176, 0.721239,9363, shaftEff=0.98,relRotationEff=1)
#'
#' @export


calcResistanceShipPwr <- function(Rtot,shipSpeed, hullEff, openWaterEff,totalInstalledPwr,
                                  shaftEff=0.98,relRotationEff=1, pwrUpperBoundPercent=1, pwrLowerBoundPercent=0.02){

  power <-ifelse(shipSpeed==0,
                 0,
                 pmin(pwrUpperBoundPercent*totalInstalledPwr,
    pmax(pwrLowerBoundPercent*totalInstalledPwr,
         ((Rtot*shipSpeed)/
            (hullEff*openWaterEff*shaftEff*relRotationEff))
    )
  )
  )

  return(power)

}
