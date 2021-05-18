#' @title calcActualDisp
#'
#' @description Estimate actual loaded displacement (\code{actualDisplacement})
#' (m^3) using a vessel's actual draft.
#'
#' @param Cb Maximum block coefficient (vector of numericals, dimensionless) (see \code{\link{calcCb}})
#' @param Cbw Waterline block coefficient (vector of numericals, dimensionless) (see \code{\link{calcCbw}})
#' @param actualDraft Actual draft (vector of numericals, m)
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#' @param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#'
#' @details
#' Loaded displacement is estimated using actual draft. Actual draft is
#' typically obtained from sources such as AIS messages or ship records.
#' Uses Riddlesworth method (MAN, 2011).
#'
#' @return \code{actualDisplacement} (vector of numericals, m^3)
#'
#' @references
#' \href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#'  Solutions. 2011. "Basic Principles of Propulsion." pg. 9.}
#'
#' @seealso \code{\link{calcCb}}, \code{\link{calcCbw}}
#'
#' @examples
#' calcActualDisp(c(0.82,0.66), c(0.81,0.65), c(12.5,11.1), c(13.6,11.5), c(80097,52382))
#' calcActualDisp(0.82, 0.81, 12.5, 13.6, 80097)
#'
#' @export

calcActualDisp <- function(Cb, Cbw, actualDraft, maxDraft, maxDisplacement){

  actualDisplacement<- ((pmin(0.99,
                      Cb
  ))/Cbw)*(actualDraft/maxDraft)*maxDisplacement
  return(actualDisplacement)

}
