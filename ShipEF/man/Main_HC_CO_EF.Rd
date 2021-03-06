% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{Main_HC_CO_EF}
\alias{Main_HC_CO_EF}
\title{CO and HC emissions factors (g/kWh) for main propulsive engines, by engine type.}
\format{
A data frame with 7 rows and 4 variables:
\describe{
\item{engineType}{main engine type, SSD=Slow Speed Diesel,MSD=Medium Speed Diesel,LNG=Liquid Natural Gas,ST=Steam turbine,GT=Gas turbine,MSD-ED=Medium Speed Electric Diesel,GT-ED=Gas Turbine Electric Diesel}
\item{co}{CO C3 marine vessel main engine emission factors (g/kWh)}
\item{hc}{Hydrocarbon C3 marine vessel main engine emission factors (g/kWh)}
\item{Source}{Character string describing source of specific emission factor for each engine type}
}
}
\usage{
data(Main_HC_CO_EF)
}
\description{
CO and HC emissions factors (g/kWh) for main propulsive engines, by engine type.
}
\details{
This data is used in \code{calcEF_CO.r} \code{calcEF_HC.r}
}
\examples{
assign("EF",get(utils::data(Main_HC_CO_EF)))

}
\references{
International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
 Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
 EPA. Regulatory Impact Analysis: Control of Emissions of Air Pollution from Category 3 Marine Deisel Engines, 2009.
 Whall, C, D Cooper, K Archer, L Twigger, N Thurston, D Ockwell, A McIntyre, and A Ritchie. Quantification of Emissions from Ships Associated with Ship Movements between Ports in the European Community (Part 2). Cheshire: Entec UK Limited, 2002.
}
\keyword{datasets}
