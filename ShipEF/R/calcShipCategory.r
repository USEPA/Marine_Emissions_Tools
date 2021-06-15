#' @title calcShipCategory
#'
#' @description
#' Assigns EPA regulatory ship category from engine displacement
#'
#' @param mainEngineBore Diameter of the main engine cylinder (mm) (vector of
#' numericals)
#' @param mainEngineStroke Main engine stroke length, the distance travelled by
#' the piston in each cycle (mm) (vector of numericals)
#'
#'@details
#'For more information, see Section 3.3.2.1 of the Port Emissions Inventory
#'Guidance.
#'
#' @return \code{shipCategory} (vector of ints). Valid values are: \itemize{
#' \item 1
#' \item 2
#' \item 3
#' }
#'
#' @references
#'\href{https://nepis.epa.gov/Exe/ZyPURL.cgi?Dockey=P1005ZGH.TXT}{EPA. 2009.
#'"Regulatory impact analysis: Control of emissions air pollution from category
#'3 marine diesel engines." Ann Arbor, MI: Office of Transportation and Air
#'Quality. US Environmental Protection Agency.}
#'
#'\href{https://nepis.epa.gov/Exe/ZyPDF.cgi/?Dockey=P10024CN.PDF}{EPA. 2008.
#'"Regulatory impact analysis: Control of emissions of air pollution from
#'locomotive engines and marine compression ignition engines less than 30 liters
#'per cylinder." Ann Arbor, MI: Office of Transportation and Air
#'Quality. US Environmental Protection Agency.}
#'
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Port Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#' @examples
#' calcShipCategory(400, 240)
#'
#' @export


calcShipCategory<- function(mainEngineBore, mainEngineStroke){

# Calculate displacement in L/cyl
  main.engine.displacement <- ((pi / 4) * (mainEngineBore/10) ^ 2 *
                                    (mainEngineStroke / 10)) / 1000

# Categorize Ships (C1/C2/C3)
shipCategory <- ifelse(main.engine.displacement>= 30, 3,
                                  ifelse(main.engine.displacement >= 7, 2,
                                         ifelse(main.engine.displacement > 0 , 1,
                                                NA)))
return(shipCategory)
}
