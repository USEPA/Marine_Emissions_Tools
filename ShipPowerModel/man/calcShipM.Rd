% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcShipM.r
\name{calcShipM}
\alias{calcShipM}
\title{calcShipM}
\usage{
calcShipM(actualDisplacement, lwl)
}
\arguments{
\item{actualDisplacement}{Actual loaded displacement (vector of
numericals, m^3) (see \code{\link{calcActualDisp}})}

\item{lwl}{Waterline length (vector of numericals, m) (see
\code{\link{calclwl}})}
}
\value{
\code{M} (vector of numericals, dimensionless)
}
\description{
Calculate the fineness/slenderness ratio (\code{M})
(dimensionless) of the ship.
}
\examples{
calcShipM(73663,218)
calcShipM(c(73663,216726),c(218,400))

}
\references{
\href{https://www.man-es.com/marine/products/propeller-aft-ship}{MAN Energy
Solutions. 2011. "Basic Principles of Propulsion."}
}
\seealso{
\itemize{
\item \code{\link{calcActualDisp}}
\item \code{\link{calclwl}}
}
}
