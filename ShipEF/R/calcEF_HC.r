#'@title calcEF_HC
#'
#'@description
#'Calculates the appropriate hydrocarbon (HC) emission factor (g/kWh) for
#'the given parameters.
#'
#'@param engineType Engine type (vector of strings) (see
#'\code{\link{calcEngineType}}). Valid values are: \itemize{
#'\item "SSD"
#'\item "MSD"
#'\item "MSD-ED"
#'\item "GT"
#'\item "GT-ED"
#'\item "ST"
#'\item "LNG"
#'\item "HSD" (auxiliary only)
#'\item "Boiler" (boiler only)
#'}
#'@param main_aux_boiler Is this calculation for a propulsive (main), auxiliary
#'(aux), or boiler engine? Options: \itemize{
#' \item "main" (Default)
#' \item "aux"
#' \item "boiler"
#'}
#'
#'@details
#'For more information about calculating HC emission factors, see Section 3.5.4
#'of the Port Emissions Inventory Guidance.
#'
#'@return \code{EF_HC} (g/kWh) (vector of numericals)
#'
#'@references
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Ports Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@examples
#'calcEF_HC(c("SSD","MSD","MSD-ED"))
#'calcEF_HC(c("HSD","MSD","LNG"),main_aux_boiler="aux")
#'calcEF_HC(c("Boiler"), main_aux_boiler="boiler")
#'
#' @import data.table
#' @importFrom utils data
#' @importFrom utils tail
#'@export

calcEF_HC<-function(engineType,
                     main_aux_boiler="main"
                    )
{
  #bind variables to make devtools::check() happy
  Main_HC_CO_EF<-Aux_HC_CO_EF<-Boiler_HC_CO_EF<-NULL

  #Read In Emission Factor DataFrames
  if(main_aux_boiler=="main"){
    EF<-ShipEF::Main_HC_CO_EF
  }else if(main_aux_boiler=="aux"){
    EF<-ShipEF::Aux_HC_CO_EF
  }else if(main_aux_boiler=="boiler"){
    EF<-ShipEF::Boiler_HC_CO_EF
  }
  #=================================================================
  if(main_aux_boiler=="boiler"){engineType<-rep("Boiler",length(engineType))}

  df<-data.table::as.data.table(engineType)
  hc<-EF[,c("engineType","hc")][df,on=c("engineType")][,c("hc")]

  return(hc)
}
