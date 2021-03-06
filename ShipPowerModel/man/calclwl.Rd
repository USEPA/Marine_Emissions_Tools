% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calclwl.r
\name{calclwl}
\alias{calclwl}
\title{calclwl}
\usage{
calclwl(
  shipType,
  lbp,
  tankerBulkCarrierGCargoShipTypes = c("tanker", "general.cargo", "chemical.tanker",
    "liquified.gas.tanker", "oil.tanker", "other.tanker", "bulk.carrier"),
  roroPaxShipTypes = c("ro.ro", "passenger", "ferry.pax", "ferry.ro.pax", "cruise",
    "yacht", "cruise.ed")
)
}
\arguments{
\item{shipType}{Ship type (vector of strings, see \code{\link{calcShipType}}). 
Must align with
\code{tankerBulkCarrierGCargoShipTypes} and \code{roroPaxShipTypes} groupings}

\item{lbp}{Length between perpendiculars (vector of numericals, m)}

\item{tankerBulkCarrierGCargoShipTypes}{Ship types specified in input
\code{shipTypes} to be modeled as tankers, bulk carriers and general cargo
vessels (vector of strings)}

\item{roroPaxShipTypes}{Ship types specified in input \code{shipTypes} to be
modeled as RORO and passenger ships (vector of strings)}
}
\value{
\code{lwl} (vector of numericals, m)
}
\description{
Estimate waterline length (\code{lwl}) (m) from length between
perpendiculars (\code{lbp}) (m).
}
\details{
This method this requires ship types to be grouped. Use the
\code{tankerBulkCarrierGCargoShipTypes} and \code{roroPaxShipTypes} grouping
parameters to provide these ship type groupings. Any ship types not included
in these groupings will be considered as miscellaneous vessels.
}
\examples{
calclwl(c("bulk.carrier","container.ship","cruise"), c(214.5,396,140))

}
\references{
\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
}
