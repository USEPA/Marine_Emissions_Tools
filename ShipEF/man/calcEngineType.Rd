% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcEngineType.r
\name{calcEngineType}
\alias{calcEngineType}
\title{calcEngineType}
\usage{
calcEngineType(
  propulsionType,
  mainEngineStrokeType = NULL,
  mainEngineRPM = NULL,
  MSD_SSD_RPM_CutOff = 500,
  main_aux_boiler = "main"
)
}
\arguments{
\item{propulsionType}{Descriptor of engine propulsion type (from IHS) (vector
of strings)}

\item{mainEngineStrokeType}{Engine stroke type (vector of ints). Valid values
are: \itemize{
\item 4
\item 2
\item \code{NA}
}}

\item{mainEngineRPM}{Engine revolutions per minute (vector of numericals)}

\item{MSD_SSD_RPM_CutOff}{Cutoff rpm value between medium and slow speed
diesel engines. Default = 500 rpm.}

\item{main_aux_boiler}{Is this calculation for a propulsive (main), auxiliary
(aux), or boiler engine? Options: \itemize{
\item "main" (Default)
\item "aux"
\item "boiler"
}}
}
\value{
\code{engineType} (vector of strings). Valid values are: \itemize{
\item "SSD" = Slow-speed diesel
\item "MSD" = Medium-speed diesel
\item "GT" = Gas turbine
\item "ST" = Steam turbine
\item "MSD-ED" = Electric drive MSD
\item "GT-ED" = Electric drive GT
\item "LNG" = Liquified natural gas
\item "HSD" = High-speed diesel (auxiliary engines only)
\item "Boiler" = Boiler engine (boilers only)
}
}
\description{
Assigns a standardized engine type descriptor using engine stroke and engine
rpm data.
}
\details{
For more information about assigning engine speed, see Section 3.3.2.2 of the
Port Emissions Inventory Guidance.
}
\examples{
calcEngineType(propulsionType=c("Oil Engine(s), Geared Drive",
                               "Oil Engine(s), Geared Drive",
                               "Sail, Aux Oil Eng(s), Geared"),
              mainEngineStrokeType = c(2,4,4),
              mainEngineRPM = c(NA,1515,1800))

calcEngineType(propulsionType=c("Oil Engine(s), Geared Drive",
                               "Oil Engine(s), Geared Drive",
                               "Sail, Aux Oil Eng(s), Geared"),
              mainEngineStrokeType = c(2,4,4),
              mainEngineRPM = c(NA,1515,1800),
              main_aux_boiler = "aux")

}
\references{
\href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
"Ports Emissions Inventory Guidance: Methodologies for Estimating
Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
Office of Transportation and Air Quality. US Environmental Protection Agency.}
}
