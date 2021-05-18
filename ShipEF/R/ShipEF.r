#' ShipEF: A package containing functions to assign marine vessel emission
#' factors
#'
#' The ShipEF package by the U.S. Environmental Protection Agency provides
#' functions for assigning emission factors to commercial marine vessels (CMV).
#' For various pollutants, emission factors depend on vessel characteristics,
#' vessel operating modes, and/or vessel location. This package can be used in
#' conjunction with ship power estimates (see the \code{ShipPowerModel} package)
#' and ship activity data to estimate CMV air emissions.
#'
#' @section Primary ShipEF functions:
#' Each pollutant included in this package has its own function to determine
#' the appropriate emissions factor. There is also a low load adjustment factor,
#' which applies to the emission factors for each pollutant in certain operating
#' conditions.
#' \itemize{
#' \item \code{\link{calcEF_CO}} - Carbon monoxide emissions factor
#' \item \code{\link{calcEF_CO2}} - Carbon dioxide emissions factor
#' \item \code{\link{calcEF_HC}} - Hydrocarbons emissions factor
#' \item \code{\link{calcEF_NOx}} - Oxides of nitrogen emissions factor
#' \item \code{\link{calcEF_PM}} - Particulate matter emissions factor
#' \item \code{\link{calcEF_SO2}} - Sulfur dioxide emissions factor
#' \item \code{\link{calcLLAF}} - Low load adjustment factor
#' \item \code{\link{calcEF}} - Calculates emission factors for all pollutants
#' and applies the LLAF
#' }
#'
#' @section Vignettes:
#' An example of how to use this package can be found in the following vignette:
#' \code{vignette("OverviewOfShipEF", package="ShipEF")}
#'
#' @docType package
#' @name ShipEF
NULL
