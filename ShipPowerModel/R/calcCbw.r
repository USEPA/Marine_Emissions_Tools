#' @title calcCbw
#'
#' @description Calculates the waterline block coefficient (\code{Cbw})
#' (dimensionless) using actual draft.
#'
#' @param Cb Maximum block coefficient (vector of numericals, dimensionless)
#' (see \code{\link{calcCb}})
#' @param actualDraft Actual draft (vector of numericals, m)
#' @param maxDraft Maximum summer load line draft (vector of numericals, m)
#'
#' @details
#' The block coefficient is the ratio of the vessel's displacement and its
#' volume, defined by its waterline length (\code{lwl}), breadth, and draft:
#' \deqn{Cb=\frac{displacement}{lwl*breadth*draft}}{Cb=displacement/(lwl*breadth*draft)}
#'
#' This function returns the  waterline block coefficient (\code{Cbw}), which
#' represents a loaded condition estimation using actual draft using the
#' Riddlesworth method. Actual draft is typically obtained from sources such as
#' AIS messages or ship records.
#'
#' NOTE: Technically, no block coefficient can be greater than 1. Even a
#' waterline block coefficient of 1 itself is extremely improbable and can cause
#' numerical errors elsewhere in the model. Therefore, the upper bound of the
#' waterline block coefficient is fixed at 0.99 in this function.
#'
#' @return \code{Cbw} (vector of numericals, dimensionless)
#'
#' @references
#'\href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
#' Solutions. 2011. "Basic Principles of Propulsion."}
#'
#' @seealso \code{\link{calcCb}}
#'
#' @examples
#' calcCbw(c(0.82,0.66),c(12.48,11.09),c(13.57,11.49))
#' calcCbw(0.82,12.48,13.57)
#'
#' @export

calcCbw <- function(Cb,actualDraft,maxDraft){


  #ensure that: actualDraft < maxDraft
  actualDraft<-pmin(actualDraft,
                    maxDraft)

  Cbw<- pmin(0.99,
             1-(1-pmin(0.99,
                        Cb
                       )
                )*((maxDraft/actualDraft)^(1/3))
  )
  return(Cbw)
}
