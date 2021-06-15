#' ShipPowerModel: A package for estimating the propulsive power of commercial
#' marine vessels
#'
#' The ShipPowerModel package by the U.S. Environmental Protection Agency
#' provides functions for estimating the propulsive power of commercial marine
#' vessels (CMV). It also provides functions for estimating auxiliary and boiler
#' engine loads. This package can be used in conjunction with vessel
#' characteristics and emission factors (see the \code{ShipEF} package) to
#' estimate CMV air emissions.
#'
#' @section Primary ShipPowerModel functions:
#' The four functions below represent different methods for estimating vessel
#' power: \itemize{
#' \item \code{\link{calcPropPwr}} - Propeller law, which was used as the basis
#'  for load factor estimates in the U.S. EPA's 2014 National Emissions Inventory
#' \item \code{\link{calcAdmPwr}} - Admiralty formula, which was used as the
#'  basis for load factor estimates in the Third IMO Greenhouse Gas Study
#' \item \code{\link{calcHMPwr}} - Implementation of the Holtrop & Mennen model
#'  as used in the U.S. EPA's 2017 National Emissions Inventory
#' \item \code{\link{calcKristPwr}} - Implementation of Kristensen's SHIP-DESMO
#'  vessel power model
#' }
#'
#' In addition, \code{\link{calcAuxBoilerLoad}} is another primary function,
#' which assigns auxiliary and boiler engine loads based on ship type, ship
#' subtype, and operating mode.
#'
#' @section Vectorization:
#' Every function in this package has been optimized to work on vectors of data.
#' This is because the input datasets required to calculate instantaneous ship
#' power are very large. For example, the resolution of the ship activity data
#' may be as fine as every 5 minutes--or even more. Multiply this across the
#' hundreds or thousands of vessels that may be included in the analysis, and
#' the data become to large to handle via individual function calls per row of
#' data.
#'
#' Therefore, it is recommended that users store their data in \code{data.table}
#' objects and pass columns from the data.table as arguments to the functions in
#' this package. Other forms of vectorization work, as well, such as using
#' \code{data.frame}. The function documentation specifies which arguments can
#' be vectorized, and the vignettes introduced below provide examples on how to
#' do simple vectorization using \code{data.frame}.
#'
#' @section Vignettes:
#' An overview of each model can be found in the following vignette:
#' \code{vignette("OverviewOfPowerModels", package="ShipPowerModel")}
#'
#' Each model also has its own vignette: \itemize{
#' \item \code{vignette("Propeller.Law.Example", package="ShipPowerModel")}
#' \item \code{vignette("Admiralty.Formula.Example", package="ShipPowerModel")}
#' \item \code{vignette("HoltropMennen.Example", package="ShipPowerModel")}
#' \item \code{vignette("Kristensen.Example", package="ShipPowerModel")}
#' }
#'
#' @docType package
#' @name ShipPowerModel
NULL
