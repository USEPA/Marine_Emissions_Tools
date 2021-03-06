% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{LLAFCoeff}
\alias{LLAFCoeff}
\title{Coefficients required to calculate low load adjustment per pollutant when main engine load is below 20 percent its total installed power.}
\format{
A data frame with 7 rows and 4 variables:
\describe{
\item{pollutant}{Pollutant for which emissions are being calculated at low loads.}
\item{a}{Coefficient required to calculate low load adjustment for each pollutant.}
\item{x}{Coefficient required to calculate low load adjustment for each pollutant.}
\item{b}{Coefficient required to calculate low load adjustment for each pollutant.}
}
}
\usage{
data(LLAFCoeff)
}
\description{
Coefficients required to calculate low load adjustment per pollutant when main engine load is below 20 percent its total installed power.
}
\details{
This data is used in \code{calcEF.r}
}
\examples{
data(LLAFCoeff)
}
\references{
EPA. Regulatory Impact Analysis: Control of Emissions of Air Pollution from Category 3 Marine Deisel Engines, 2009.
}
\keyword{datasets}
