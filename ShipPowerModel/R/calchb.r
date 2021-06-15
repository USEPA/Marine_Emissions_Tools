#' @title calchb
#'
#' @description Estimate the center of bulb area above keel line (\code{hb}) (m)
#' using the method in Rakke (2016).
#'
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#'
#' @return \code{hb} (vector of numericals, m)
#'
#' @references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'\href{http://hdl.handle.net/11250/2410741}{Rakke, S. G. 2016. "Ship Emissions
#'Calculation from AIS." NTNU.}
#'
#' @examples
#' calchb(c(13.57,11.49))
#' calchb(13.57)
#'
#' @export


calchb <-function(maxDraft){

  hb<-0.4*maxDraft

  return(hb)
}
