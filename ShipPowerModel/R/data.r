#' Unit test data for Holtrop-Mennen Calculations
#'
#' @docType data
#'
#' @usage data(TestShipsHM)
#'
#' @details This data is used for testing the Holtrop-Mennen power calculations
#'
#' @format A data frame with 1 row and 34 variables
#'
#' @keywords unit test
"TestShipsHM"

#' Unit test data for Kristensen Calculations
#'
#' @docType data
#'
#' @usage data(TestShipsKrist)
#'
#' @details This data is used for testing the Kristensen (SHIP-DESMO) power calculations
#'
#' @format A data frame with 6 row and 29 variables
#'
#' @keywords unit test
"TestShipsKrist"

#' Auxiliary engine loads per operating mode, ship type and sub type using IMO GHG 3 methodology. Also used to define sub types.
#'
#' @details
#' IMO GHG 3 uses Starcrest 2014 auxiliary loads from multiple vessel boarding programs(VBP) around the US in addition to FMI international auxiliary load data.
#' @docType data
#'
#' @usage data(IMOGHG3_AuxLoadFactor)
#'
#' @details This data is used in \code{calcAuxBoilerLoad.r} \code{calcSubType.r}
#'
#' @format A data frame with 56 rows and 9 variables:
#' \describe{
#' \item{shipType}{Generalizable ship type. Define by \code{calcShipType.r}}
#' \item{sizeMin}{Minimum size value for each subtype and their given size unit. Used to define subtype.}
#' \item{sizeMax}{Maximum size value for each subtype and their given size unit. Used to define subtype.}
#' \item{sizeUnits}{Size units used to define subtype size ranges.}
#' \item{subType}{Sub type of each ship type which are defined by size ranges.}
#' \item{Transit}{Auxiliary engine loads (kW) assigned to each vessel by subtype when transiting. Transiting defined by \code{calcOperatingMode.r}}
#' \item{Maneuvering}{Auxiliary engine loads (kW) assigned to each vessel by subtype when Maneuvering. Maneuvering defined by \code{calcOperatingMode.r}}
#' \item{Berth}{Auxiliary engine loads (kW) assigned to each vessel by subtype when berth. Berthing defined by \code{calcOperatingMode.r}}
#' \item{Anchorage}{Auxiliary engine loads (kW) assigned to each vessel by subtype when anchoring. Anchorage defined by \code{calcOperatingMode.r}}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#'
#' @examples
#' assign("Loads",get(utils::data(IMOGHG3_AuxLoadFactor)))
"IMOGHG3_AuxLoadFactor"

#' Boiler engine loads (kW) per operating mode, ship type and sub type using IMO GHG 3 methodology.
#'
#' @docType data
#'
#' @details
#' IMO GHG 3 uses Starcrest 2014 auxiliary loads from multiple vessel boarding programs(VBP) around the US in addition to FMI international auxiliary load data.
#'
#' @usage data(IMOGHG3_BoilerLoadFactor)
#'
#' @details This data is used in \code{calcAuxBoilerLoad.r}
#'
#' @format A data frame with 56 rows and 9 variables:
#' \describe{
#' \item{shipType}{Generalizable ship type. Define by \code{calcShipType.r}}
#' \item{sizeMin}{Minimum size value for each subtype and their given size unit.}
#' \item{sizeMax}{Maximum size value for each subtype and their given size unit.}
#' \item{sizeUnits}{Size units used to define subtype size ranges.}
#' \item{subType}{Sub type of each ship type which are defined by size ranges.}
#' \item{Transit}{Boiler engine loads (kW) assigned to each vessel by subtype when transiting. Transiting defined by \code{calcOperatingMode.r}}
#' \item{Maneuvering}{Boiler engine loads(kW) assigned to each vessel by subtype when Maneuvering. Maneuvering defined by \code{calcOperatingMode.r}}
#' \item{Berth}{Boiler engine loads (kW) assigned to each vessel by subtype when berth. Berthing defined by \code{calcOperatingMode.r}}
#' \item{Anchorage}{Boiler engine loads (kW) assigned to each vessel by subtype when anchoring. Anchorage defined by \code{calcOperatingMode.r}}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#'
#' @examples
#' assign("Loads",get(utils::data(IMOGHG3_BoilerLoadFactor)))
"IMOGHG3_BoilerLoadFactor"

#' Auxiliary engine loads per operating mode, ship type and sub type using Starcrest 2016 methodology. Also used to define sub types.
#'
#' @docType data
#'
#' @usage data(Starcrest2016_AuxLoadFactor)
#'
#' @details This data is used in \code{calcAuxBoilerLoad.r} \code{calcSubType.r}
#'
#' @format A data frame with 34 rows and 9 variables:
#' \describe{
#' \item{shipType}{Generalizable ship type. Define by \code{calcShipType.r} using method="starcrest"}
#' \item{sizeMin}{Minimum size value for each subtype and their given size unit. Used to define subtype.}
#' \item{sizeMax}{Maximum size value for each subtype and their given size unit. Used to define subtype.}
#' \item{sizeUnits}{Size units used to define subtype size ranges.}
#' \item{subType}{Sub type of each ship type which are defined by size ranges.}
#' \item{Transit}{Auxiliary engine loads (kW) assigned to each vessel by subtype when transiting. Transiting defined by \code{calcOperatingMode.r}}
#' \item{Maneuvering}{Auxiliary engine loads (kW) assigned to each vessel by subtype when Maneuvering. Maneuvering defined by \code{calcOperatingMode.r}}
#' \item{Berth}{Auxiliary engine loads (kW) assigned to each vessel by subtype when berth. Berthing defined by \code{calcOperatingMode.r}}
#' \item{Anchorage}{Auxiliary engine loads (kW) assigned to each vessel by subtype when anchoring. Anchorage defined by \code{calcOperatingMode.r}}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
#'
#' @examples
#' assign("Loads",get(utils::data(Starcrest2016_AuxLoadFactor)))
"Starcrest2016_AuxLoadFactor"

#' Boiler engine loads (kW) per operating mode, ship type and sub type using Starcrest 2016 methodology.
#'
#' @docType data
#'
#' @usage data(Starcrest2016_BoilerLoadFactor)
#'
#' @details This data is used in \code{calcAuxBoilerLoad.r}
#'
#' @format A data frame with 34 rows and 9 variables:
#' \describe{
#' \item{shipType}{Generalizable ship type. Define by \code{calcShipType.r}}
#' \item{sizeMin}{Minimum size value for each subtype and their given size unit.}
#' \item{sizeMax}{Maximum size value for each subtype and their given size unit.}
#' \item{sizeUnits}{Size units used to define subtype size ranges.}
#' \item{subType}{Sub type of each ship type which are defined by size ranges.}
#' \item{Transit}{Boiler engine loads (kW) assigned to each vessel by subtype when transiting. Transiting defined by \code{calcOperatingMode.r}}
#' \item{Maneuvering}{Boiler engine loads(kW) assigned to each vessel by subtype when Maneuvering. Maneuvering defined by \code{calcOperatingMode.r}}
#' \item{Berth}{Boiler engine loads (kW) assigned to each vessel by subtype when berth. Berthing defined by \code{calcOperatingMode.r}}
#' \item{Anchorage}{Boiler engine loads (kW) assigned to each vessel by subtype when anchoring. Anchorage defined by \code{calcOperatingMode.r}}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
#'
#' @examples
#' assign("Loads",get(utils::data(Starcrest2016_BoilerLoadFactor)))
"Starcrest2016_BoilerLoadFactor"

#' Map between individual Clarkson's Vessel_Type and generalizable ship types used in IMO GHG 3 (imoShipType) and Starcrest (starcrestShipType) inventories.
#'
#' @docType data
#'
#' @usage data(shipMap)
#'
#' @details This is used in \code{calcShipType.r}
#'
#' @format A data frame with 341 rows and 3 variables:
#' \describe{
#' \item{Vessel_Type}{Specific Clarkson's Vessel_Type. }
#' \item{imoShipType}{Generalizable ship types alligning with IMO GHG 3 auxiliary and boiler loads.}
#' \item{starcrestShipType}{Generalizable ship types alligning with starcrest 2016 auxiliary and boiler loads.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
#'
#' @examples
#' data(shipMap)
"shipMap"
