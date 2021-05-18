#' @title calcShipM
#'
#' @description Calculate the fineness/slenderness ratio (\code{M})
#' (dimensionless) of the ship.
#'
#' @param actualDisplacement Actual loaded displacement (vector of
#' numericals, m^3) (see \code{\link{calcActualDisp}})
#' @param lwl Waterline length (vector of numericals, m) (see
#' \code{\link{calclwl}})
#'
#' @return \code{M} (vector of numericals, dimensionless)
#'
#' @references
#'\href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#' Solutions. 2011. "Basic Principles of Propulsion."}
#'
#'@seealso
#' \itemize{
#' \item \code{\link{calcActualDisp}}
#' \item \code{\link{calclwl}}
#' }
#'
#' @examples
#' calcShipM(73663,218)
#' calcShipM(c(73663,216726),c(218,400))
#'
#' @export

calcShipM <-function(actualDisplacement, lwl){

  M <- lwl/(actualDisplacement^(1/3))

  return(M)
}
