#'@title calcCb
#'
#'@description Calculates the maximum block coefficient (\code{Cb})
#'(dimensionless) based on a ship's maximum displacement and and maximum draft.
#'
#'@param maxDisplacement Maximum ship displacement (vector of numericals, m^3)
#'@param lwl Waterline length (vector of numericals, m) (see \code{\link{calclwl}})
#'@param breadth Moulded breadth (vector of numericals, m)
#'@param maxDraft Maximum summer load line draft (vector of numericals, m)
#'
#'@details
#'The block coefficient is the ratio of the vessel's displacement and its
#'volume, defined by its waterline length (\code{lwl}), breadth, and draft:
#'\deqn{Cb=\frac{displacement}{lwl*breadth*draft}}{Cb=displacement/(lwl*breadth*draft)}
#'
#'This function returns the maximum block coefficient (\code{Cb}), which is the
#'ratio of the vessel's maximum displacement and it's maximum volume, defined by
#'its waterline length (\code{lwl}), breadth, and maximum draft:
#'\deqn{Cb=\frac{maxDisplacement}{lwl*breadth*maxDraft}}{Cb=maxDisplacement/(lwl*breadth*maxDraft)}
#'
#'@return \code{Cb} (vector of numericals, dimensionless)
#'
#'@references
#'\href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#' Solutions. 2011. "Basic Principles of Propulsion."}
#'
#'@seealso \itemize{
#'\item \code{\link{calclwl}}
#'\item \code{\link{calcCbw}}
#'}
#'
#'@examples
#'calcCb(c(80097.00,52382.04), c(218.75,209.25), c(32.25,32.2), c(13.6,11.5))
#'
#'@export


calcCb<- function(maxDisplacement, lwl, breadth, maxDraft){

  Cb <- pmin(1,
             maxDisplacement/(lwl*breadth*maxDraft)
  )
  return(Cb)

}
