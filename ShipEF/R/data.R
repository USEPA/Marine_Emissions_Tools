#' CO and HC emissions factors (g/kWh) for auxiliary engines, by engine type.
#'
#' @docType data
#'
#' @usage data(Aux_HC_CO_EF)
#'
#' @details This data is used in \code{calcEF_CO.r} \code{calcEF_HC.r}
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#' \item{engineType}{auxiliary engine type, HSD=High Speed Diesel,MSD=Medium Speed Diesel,LNG=Liquid Natural Gas}
#' \item{co}{CO C3 marine vessel auxiliary engine emission factors (g/kWh)}
#' \item{hc}{Hydrocarbon C3 marine vessel auxiliary engine emission factors (g/kWh)}
#' \item{Source}{Character string describing source of specific emission factor for each engine type}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
#'
#' @examples
#' assign("EF",get(utils::data(Aux_HC_CO_EF)))
#'
"Aux_HC_CO_EF"

#' CO and HC emissions factors (g/kWh) for main propulsive engines, by engine type.
#'
#' @docType data
#'
#' @usage data(Main_HC_CO_EF)
#'
#' @details This data is used in \code{calcEF_CO.r} \code{calcEF_HC.r}
#'
#' @format A data frame with 7 rows and 4 variables:
#' \describe{
#' \item{engineType}{main engine type, SSD=Slow Speed Diesel,MSD=Medium Speed Diesel,LNG=Liquid Natural Gas,ST=Steam turbine,GT=Gas turbine,MSD-ED=Medium Speed Electric Diesel,GT-ED=Gas Turbine Electric Diesel}
#' \item{co}{CO C3 marine vessel main engine emission factors (g/kWh)}
#' \item{hc}{Hydrocarbon C3 marine vessel main engine emission factors (g/kWh)}
#' \item{Source}{Character string describing source of specific emission factor for each engine type}
#' }
#'
#' @keywords datasets
#'
#' @references
#'  International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#'  Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
#'  EPA. Regulatory Impact Analysis: Control of Emissions of Air Pollution from Category 3 Marine Deisel Engines, 2009.
#'  Whall, C, D Cooper, K Archer, L Twigger, N Thurston, D Ockwell, A McIntyre, and A Ritchie. Quantification of Emissions from Ships Associated with Ship Movements between Ports in the European Community (Part 2). Cheshire: Entec UK Limited, 2002.
#'
#' @examples
#' assign("EF",get(utils::data(Main_HC_CO_EF)))
#'
"Main_HC_CO_EF"

#' CO and HC emissions factors (g/kWh) for boiler engines.
#'
#' @docType data
#'
#' @usage data(Boiler_HC_CO_EF)
#'
#' @details This data is used in \code{calcEF_CO.r} \code{calcEF_HC.r}
#'
#' @format A data frame with 1 row and 4 variables:
#' \describe{
#' \item{engineType}{boiler engine type signified by "Boiler"}
#' \item{co}{CO C3 marine vessel boiler engine emission factors (g/kWh)}
#' \item{hc}{Hydrocarbon C3 marine vessel boiler engine emission factors (g/kWh)}
#' \item{Source}{Character string describing source of specific emission factor for each engine type}
#' }
#'
#' @keywords datasets
#'
#' @references
#'  Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
#'
#' @examples
#' assign("EF",get(utils::data(Boiler_HC_CO_EF)))
#'
"Boiler_HC_CO_EF"

#' Brake specific fuel consumption (BSFC) (g/kWh) for auxiliary engines, by engine and fuel type.
#'
#' @docType data
#'
#' @usage data(AuxBSFC)
#'
#' @details This data is used in \code{calcEF_CO2.r} \code{calcEF_PM.r} \code{calcEF_SO2.r}
#'
#' @format A data frame with 4 rows and 4 variables:
#' \describe{
#' \item{engineType}{auxiliary engine type, HSD=High Speed Diesel,MSD=Medium Speed Diesel,LNG=Liquid Natural Gas}
#' \item{fuelType}{fuel type used by vessel: RM/HFO=Residual Marine/Heavy Fuel Oil,MGO/MDO=Marine Gas Oil/Marine Diesel Oil,LNG=Liquid Natural Gas}
#' \item{BSFC}{Brake specific fuel consumption rate for auxiliary engines (g/kWh)}
#' \item{Source}{Character string describing source of each bsfc rate for each engine and fuel type}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' IVL, David Cooper, and Tomas Gustafsson. Methodology for Calculating Emissions from Ships. 1. Update of Emission Factors. n.d., 47.
#' Wartsila (2014). Solutions for marine and oil and gas markets.
#'
#' @examples
#' assign("EF",get(utils::data(AuxBSFC)))
#'
"AuxBSFC"

#' Brake specific fuel consumption (BSFC) (g/kWh) for main engines, by engine and fuel type.
#'
#' @docType data
#'
#' @usage data(MainBSFC)
#'
#' @details This data is used in \code{calcEF_CO2.r} \code{calcEF_PM.r} \code{calcEF_SO2.r}
#'
#' @format A data frame with 13 rows and 4 variables:
#' \describe{
#' \item{engineType}{main engine type, SSD=Slow Speed Diesel,MSD=Medium Speed Diesel,LNG=Liquid Natural Gas,ST=Steam turbine,GT=Gas turbine,MSD-ED=Medium Speed Electric Diesel,GT-ED=Gas Turbine Electric Diesel}
#' \item{fuelType}{fuel type used by vessel: RM/HFO=Residual Marine/Heavy Fuel Oil,MGO/MDO=Marine Gas Oil/Marine Diesel Oil,LNG=Liquid Natural Gas}
#' \item{BSFC}{Brake specific fuel consumption rate for main engines (g/kWh)}
#' \item{Source}{Character string describing source of each bsfc rate for each engine and fuel type}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' IVL, David Cooper, and Tomas Gustafsson. Methodology for Calculating Emissions from Ships. 1. Update of Emission Factors, n.d., 47.
#' Wartsila (2014). Solutions for marine and oil and gas markets.
#'
#' @examples
#' assign("EFBSFC",get(utils::data(MainBSFC)))
#'
"MainBSFC"

#' Brake specific fuel consumption (BSFC) (g/kWh) for boiler engines, by fuel type.
#'
#' @docType data
#'
#' @usage data(BoilerBSFC)
#'
#' @details This data is used in \code{calcEF_CO2.r} \code{calcEF_PM.r} \code{calcEF_SO2.r}
#'
#' @format A data frame with 13 rows and 4 variables:
#' \describe{
#' \item{engineType}{boiler engine type,signified as "Boiler"}
#' \item{fuelType}{fuel type used by boiler engine: RM/HFO=Residual Marine/Heavy Fuel Oil,MGO/MDO=Marine Gas Oil/Marine Diesel Oil}
#' \item{BSFC}{Brake specific fuel consumption rate for boiler engines (g/kWh)}
#' \item{Source}{Character string describing source of each bsfc rate for each engine and fuel type}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' IVL, David Cooper, and Tomas Gustafsson. Methodology for Calculating Emissions from Ships. 1. Update of Emission Factors, n.d., 47.
#' Wartsila (2014). Solutions for marine and oil and gas markets.
#'
#' @examples
#' assign("EFBSFC",get(utils::data(BoilerBSFC)))
#'
"BoilerBSFC"

#' NOx emission factors (g/kWh) for auxiliary engines, by fuel type, engine type, and engine tier.
#'
#' @docType data
#'
#' @usage data(AuxNOxEF)
#'
#' @details This data is used in \code{calcEF_NOx.r}
#'
#' @format A data frame with 17 rows and 5 variables:
#' \describe{
#' \item{engineType}{auxiliary engine type, "HSD"="High Speed Diesel","MSD"="Medium Speed Diesel","LNG"="Liquid Natural Gas"}
#' \item{fuelType}{fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil","LNG="Liquid Natural Gas"}
#' \item{tier}{Engine regulatory tier (per IMO regulations). Defined by \code{calcTier.r}}
#' \item{nox}{NOx emission factors (g/kWh) per engine type, fuel type and tier}
#' \item{Source}{Character string describing source of each NOx emission factor for each engine, fuel type and tier.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' Starcrest Consulting Group, LLC. San Pedro Bay Ports Emissions Inventory Methodology Report Version 2, 2021.
#'
#'
#' @examples
#' assign("EF",get(utils::data(AuxNOxEF)))
#'
"AuxNOxEF"

#' NOx emission factors (g/kWh) for main engines, by fuel type, engine type, and engine tier.
#'
#' @docType data
#'
#' @usage data(MainNOxEF)
#'
#' @details This data is used in \code{calcEF_NOx.r}
#'
#' @format A data frame with 31 rows and 5 variables:
#' \describe{
#' \item{engineType}{main engine type, "HSD"="High Speed Diesel","MSD"="Medium Speed Diesel","LNG"="Liquid Natural Gas"}
#' \item{fuelType}{fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil","LNG="Liquid Natural Gas"}
#' \item{tier}{Engine regulatory tier (per IMO regulations). Defined by \code{calcTier.r}}
#' \item{nox}{NOx emission factors (g/kWh) per engine type, fuel type and tier}
#' \item{Source}{Character string describing source of each NOx emission factor for each engine, fuel type and tier.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' International Maritime Organization. Second IMO GHG Study. 4 Albert Embankment, London SE1 7SR: International Maritime Organization, 2009.
#' Starcrest Consulting Group, LLC. San Pedro Bay Ports Emissions Inventory Methodology Report Version 2, 2021.
#' EPA. Regulatory Impact Analysis: Control of Emissions of Air Pollution from Category 3 Marine Deisel Engines, 2009.
#' Whall, C, D Cooper, K Archer, L Twigger, N Thurston, D Ockwell, A McIntyre, and A Ritchie. Quantification of Emissions from Ships Associated with Ship Movements between Ports in the European Community (Part 2). Cheshire: Entec UK Limited, 2002.
#'
#'
#' @examples
#' assign("EF",get(utils::data(MainNOxEF)))
#'
"MainNOxEF"

#' NOx emission factors (g/kWh) for boilers, by fuel type, engine type, and engine tier.
#'
#' @docType data
#'
#' @usage data(BoilerNOxEF)
#'
#' @details This data is used in \code{calcEF_NOx.r}
#'
#' @format A data frame with 2 rows and 5 variables:
#' \describe{
#' \item{engineType}{boiler engine type, "Boiler"}
#' \item{fuelType}{fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil"}
#' \item{tier}{Engine regulatory tier (per IMO regulations). Defined by \code{calcTier.r}}
#' \item{nox}{NOx emission factors (g/kWh) per engine type, fuel type and tier}
#' \item{Source}{Character string describing source of each NOx emission factor for each engine, fuel type and tier.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Starcrest Consulting Group,LLC.Port of Los Angeles Inventory of Air Emissions-2016, 2017.
#' Starcrest Consulting Group,LLC.2014 Multi-Facility Emissions Inventory of Cargo Handling Equipment, Heavy-Duty Diesel Vehicles, Railroad Locomotives, and Commercial Marine Vessels. Starcrest Consulting Group,LLC, February 2016.
#'
#'
#' @examples
#' assign("EF",get(utils::data(BoilerNOxEF)))
#'
"BoilerNOxEF"

#' Fuel Mix Table for auxiliary engines, by fuel type, engine type, and location.
#'
#' @docType data
#'
#' @usage data(AuxFuelMixTable)
#'
#' @details This data is used in \code{calcEF_CO2.r}\code{calcEF_NOx.r}\code{calcEF_PM.r}\code{calcEF_SO2.r}
#'
#' @format A data frame with 15 rows and 4 variables:
#' \describe{
#' \item{Location}{Vessel location indicating fuel use patterns and fuel sulfur level per region. ECA indicates North American Emission Control Area}
#' \item{engineType}{Auxiliary engine type, "HSD"="High Speed Diesel","MSD"="Medium Speed Diesel","LNG"="Liquid Natural Gas"}
#' \item{fuelType}{Fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil","LNG="Liquid Natural Gas"}
#' \item{Proportion}{Indicates the proportion of vessels per engine type (from 0 to 1) which use each possible fuel within each region. Used to to calculate weighted average emission factors when vessel specific fuel information is not available. }
#' }
#'
#' @keywords datasets
#'
#'
#' @examples
#' assign("fuelMixTable",get(utils::data(AuxFuelMixTable)))
#'
"AuxFuelMixTable"

#' Fuel Mix Table for main propulsive engines, by fuel type, engine type, and location.
#'
#' @docType data
#'
#' @usage data(MainFuelMixTable)
#'
#' @details This data is used in \code{calcEF_CO2.r}\code{calcEF_NOx.r}\code{calcEF_PM.r}\code{calcEF_SO2.r}
#'
#' @format A data frame with 39 rows and 4 variables:
#' \describe{
#' \item{Location}{Vessel location indicating fuel use patterns and fuel sulfur level per region. ECA indicates North American Emission Control Area}
#' \item{engineType}{Main engine type, "HSD"="High Speed Diesel","MSD"="Medium Speed Diesel","LNG"="Liquid Natural Gas"}
#' \item{fuelType}{Fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil","LNG="Liquid Natural Gas"}
#' \item{Proportion}{Indicates the proportion of vessels per engine type (from 0 to 1) which use each possible fuel within each region. Used to to calculate weighted average emission factors when vessel specific fuel information is not available. }
#' }
#'
#' @keywords datasets
#'
#'
#' @examples
#' assign("fuelMixTable",get(utils::data(MainFuelMixTable)))
#'
"MainFuelMixTable"

#' Fuel Mix Table for boilers, by fuel type, engine type, and location.
#'
#' @docType data
#'
#' @usage data(BoilerFuelMixTable)
#'
#' @details This data is used in \code{calcEF_CO2.r}\code{calcEF_NOx.r}\code{calcEF_PM.r}\code{calcEF_SO2.r}
#'
#' @format A data frame with 6 rows and 4 variables:
#' \describe{
#' \item{Location}{Vessel location indicating fuel use patterns and fuel sulfur level per region. ECA indicates North American Emission Control Area}
#' \item{engineType}{Boiler engine type, "Boiler"}
#' \item{fuelType}{Fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil"}
#' \item{Proportion}{Indicates the proportion of vessels per engine type (from 0 to 1) which use each possible fuel within each region. Used to to calculate weighted average emission factors when vessel specific fuel information is not available. }
#' }
#'
#' @keywords datasets
#'
#'
#' @examples
#' assign("fuelMixTable",get(utils::data(BoilerFuelMixTable)))
#'
"BoilerFuelMixTable"

#' Conversion factors to produce CO2 emission factors from BSFC values by fuel type.
#'
#' @docType data
#'
#' @usage data(CO2ConversionFactor)
#'
#' @details This data is used in \code{calcEF_CO2.r}
#'
#' @format A data frame with 3 rows and 3 variables:
#' \describe{
#' \item{fuelType}{Fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil","LNG="Liquid Natural Gas"}
#' \item{conversionFactor}{Conversion factors to convert BSFC values to CO2 emission factors.}
#' \item{Source}{Character string describing source of each CO2 conversion factor by fuel type.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Marine Environmental Protection Committee. 2012 Guidelines on the Method of Calculation of the Attained Energy Efficiency Design Index (EEDI) for New Ships (MEPC 63/28 Annex 8), March 2012.
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#'
#' @examples
#'   data(CO2ConversionFactor)
#'
"CO2ConversionFactor"

#' Coefficients required to calculate PM emission factors (g/kWh) per fuel type for diesel engines.
#'
#' @docType data
#'
#' @usage data(EFSulfurEQCoefficients)
#'
#' @details This data is used in \code{calcEF_PM.r}
#'
#' @format A data frame with 2 rows and 5 variables:
#' \describe{
#'   \item{fuelType}{Fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil"}
#'   \item{MWR}{Molecular weight ratio of sulfate PM to sulfur (224/32=7).}
#'   \item{FSC}{Percantage of sulfur in fuel that is converted to direct sulfate PM.}
#'   \item{Snom}{Nominal fuel sulfur level.}
#'   \item{PMnom}{Base PM emission rate for a fuel sulfur level of 0\%.}
#'   \item{Source}{Character string describing source of each CO2 conversion factor by fuel type.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' EPA. Regulatory Impact Analysis: Control of Emissions of Air Pollution from Category 3 Marine Deisel Engines, 2009.
#'
#' @examples
#' data(EFSulfurEQCoefficients)
"EFSulfurEQCoefficients"

#' PM10 emission factors (g/kWh) for non diesel engines per fuel type and engine type.
#'
#' @docType data
#'
#' @usage data(EF_PM_ST_GT_LNG)
#'
#' @details This data is used in \code{calcEF_PM.r}
#'
#' @format A data frame with 7 rows and 4 variables:
#' \describe{
#' \item{engineType}{Non-diesel engine type: "ST"="Steam Turbine","GT"="Gas Turbine","GT-ED"="Gas Turbine Electic Diesel","LNG"="Liquid Natural Gas"}
#' \item{fuelType}{Fuel type used by vessel: "RM/HFO"="Residual Marine/Heavy Fuel Oil","MGO/MDO"="Marine Gas Oil/Marine Diesel Oil","LNG="Liquid Natural Gas"}
#' \item{pm10EF}{PM10 emission factors (g/kWh) per engine type and fuel type.}
#' \item{Source}{Character string describing source of each PM emission factors by fuel and engine type.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' International Maritime Organization.Third IMO GHG Study 2014 - Final Report. London: International Maritime Organization, 2014.
#' IVL, David Cooper, and Tomas Gustafsson. Methodology for Calculating Emissions from Ships. 1. Update of Emission Factors, n.d., 47.
#' Kristensen, Hans Otto. Energy Demand and Exhaust Gas Emissions of Marine Engines, 2012, 30.
#'
#' @examples
#' data(EF_PM_ST_GT_LNG)
"EF_PM_ST_GT_LNG"

#' Coefficients required to calculate low load adjustment per pollutant when main engine load is below 20 percent its total installed power.
#'
#' @docType data
#'
#' @usage data(LLAFCoeff)
#'
#' @details This data is used in \code{calcEF.r}
#'
#' @format A data frame with 7 rows and 4 variables:
#' \describe{
#' \item{pollutant}{Pollutant for which emissions are being calculated at low loads.}
#' \item{a}{Coefficient required to calculate low load adjustment for each pollutant.}
#' \item{x}{Coefficient required to calculate low load adjustment for each pollutant.}
#' \item{b}{Coefficient required to calculate low load adjustment for each pollutant.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' EPA. Regulatory Impact Analysis: Control of Emissions of Air Pollution from Category 3 Marine Deisel Engines, 2009.
#'
#' @examples
#' data(LLAFCoeff)
"LLAFCoeff"
