% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{Boiler_HC_CO_EF}
\alias{Boiler_HC_CO_EF}
\title{CO and HC emissions factors (g/kWh) for boiler engines.}
\format{
A data frame with 1 row and 4 variables:
\describe{
\item{engineType}{boiler engine type signified by "Boiler"}
\item{co}{CO C3 marine vessel boiler engine emission factors (g/kWh)}
\item{hc}{Hydrocarbon C3 marine vessel boiler engine emission factors (g/kWh)}
\item{Source}{Character string describing source of specific emission factor for each engine type}
}
}
\usage{
data(Boiler_HC_CO_EF)
}
\description{
CO and HC emissions factors (g/kWh) for boiler engines.
}
\details{
This data is used in \code{calcEF_CO.r} \code{calcEF_HC.r}
}
\examples{
assign("EF",get(utils::data(Boiler_HC_CO_EF)))

}
\references{
Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
}
\keyword{datasets}
