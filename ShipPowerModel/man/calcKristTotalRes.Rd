% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcKristTotalRes.r
\name{calcKristTotalRes}
\alias{calcKristTotalRes}
\title{calcKristTotalRes}
\usage{
calcKristTotalRes(
  wettedSA,
  Cf,
  Cr,
  Ca,
  Caa,
  seawaterDensity,
  shipSpeed,
  serviceMargin
)
}
\arguments{
\item{wettedSA}{Wetted hull surface area (vector of numericals, m^2) (see
\code{\link{calcKristWettedSA}})}

\item{Cf}{Frictional resistance coefficient (vector of numericals,
dimensionless) (see \code{\link{calcCf}})}

\item{Cr}{Residual resistance coefficient (vector of numericals,
dimensionless) (see \code{\link{calcKristCr}})}

\item{Ca}{Incremental hull (roughness) resistance coefficient (vector of
numericals, dimensionless) (see \code{\link{calcKristCa}})}

\item{Caa}{Air resistance coefficient (vector of numericals, dimensionless)
(see \code{\link{calcKristCaa}})}

\item{seawaterDensity}{Sea water density. Default = 1.025 (g/cm^3). Can
supply either a vector of numericals, a single number, or rely on the default}

\item{shipSpeed}{Ship actual speed (vector of numericals, m/s) (see
\code{\link{calcSpeedUnitConversion}})}

\item{serviceMargin}{A service margin to account for weather and sea effects:
\itemize{\item Coastal operations = 10 \item At-sea operations = 15} Can
supply either a vector of numericals, a single number, or rely on the default}
}
\value{
\code{Rtot} (vector of numericals, kN)
}
\description{
Calculate total ship resistance (\code{Rtot}) (kN) using the
Kristensen method.
}
\details{
Note that service margin is included here as a resistance term.
}
\examples{
calcKristTotalRes(10714.62,0.0015,0.00043,6.46e-05,5e-05,1.025,10.8,15)

}
\references{
Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
Power of Ships."

\href{https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}{Kristensen, H. O.
"Ship-Desmo-Tool." https://gitlab.gbar.dtu.dk/oceanwave3d/Ship-Desmo}
}
\seealso{
\itemize{
\item \code{\link{calcCf}}
\item \code{\link{calcSpeedUnitConversion}}
}

Other Kristensen Calculations: 
\code{\link{calcKristCaa}()},
\code{\link{calcKristCa}()},
\code{\link{calcKristCr}()},
\code{\link{calcKristPwr}()},
\code{\link{calcKristThrustFactor}()},
\code{\link{calcKristWakeFrac}()},
\code{\link{calcKristWettedSA}()}

Other Resistance Calculations: 
\code{\link{calcCf}()},
\code{\link{calcHMAppendageRes}()},
\code{\link{calcHMBulbousBowRes}()},
\code{\link{calcHMCa}()},
\code{\link{calcHMImmersedTransomRes}()},
\code{\link{calcHMTotalRes}()},
\code{\link{calcHMWaveMakingRes}()},
\code{\link{calcKristCaa}()},
\code{\link{calcKristCa}()},
\code{\link{calcKristCr}()}
}
\concept{Kristensen Calculations}
\concept{Resistance Calculations}
