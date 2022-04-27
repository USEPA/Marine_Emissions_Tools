#' @title calcEF
#'
#' @description Creates a data table of emission factors (g/kWh) for the given
#' parameters.
#'
#'@param engineType Engine type (string or vector of strings) (see
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
#'@param location Location of vessel (string or vector of strings). Valid values are:
#'\itemize{
#'   \item "ECA"
#'   \item "OutsideECA"
#'   \item "GreatLakes"
#' }
#'@param tier NOx engine tier (string or vector of strings) (see \code{\link{calcTier}}).
#'Valid values are:
#'\itemize{
#'  \item "Tier 0"
#'  \item "Tier 1"
#'  \item "Tier 2"
#'  \item "Tier 3"
#'}
#'@param loadFactor Fractional percentage (between 0 and 1) of main engine
#' required to propel vessel at given speed (vector of numericals) (see
#' ShipPowerModel library). This parameter is necessary when \code{output} =
#' "EF_LLAF" (default) so that the correct low load adjustments are applied. It
#' is also necessary when \code{loadBasedBSFC} = "Y" or if "nox" is in the
#' pollutants list. However, if \code{output}
#' = "EF" \code{loadBasedBSFC} = "N", this argument will not be used and the
#' resulting emission factor is independent of engine load.
#'@param ECAfuelSulfurPercentage Fuel sulfur cap (percentage by weight) for the
#'Emissions Control Area (ECA). Default = 0.1\% (in effect Jan. 1, 2015)
#'@param GlobalfuelSulfurPercentage Fuel sulfur cap (percentage by weight) for
#'outside the Emissions Control Area (ECA). Default = 0.5\% (in effect Jan. 1,
#'2020)
#'@param pollutants Select pollutants (default = "ALL"). Options: \itemize{
#'\item "co"
#'\item "hc"
#'\item "co2"
#'\item "so2"
#'\item "pm10"
#'\item "pm2.5"
#'\item "nox"
#'}
#'@param loadBasedBSFC Determines if baseline BSFC values should be altered
#'according to main engine load (see \code{\link{calcLoadBSFC}}) \itemize{
#'\item"Y"
#'\item"N" (Default)
#'\item"lower load factor bound , upper load factor bound" (see Details)
#'}
#'@param output What output table should be produced? Options: \itemize{
#'\item "EF": table of corresponding emission factors
#'\item "EF_LLAF" (Default): table of emission factors multiplied by low load
#'adjustment factors
#'}
#'@param main_aux_boiler Is this calculation for a propulsive (main), auxiliary
#'(aux), or boiler engine? Options: \itemize{
#' \item "main" (Default)
#' \item "aux"
#' \item "boiler"
#'}
#' @param inputTableLocation File path (optional). Used to specify a
#' user-supplied set of low load adjustment factors. See details for formatting
#' requirements
#'
#'@details
#'Location is important for determining the fuel being used, as type of fuel
#'typically used varies by location.
#'
#'For more information about marine emission factors, see Section 3.5 of the
#'Port Emissions Inventory Guidance.
#'
#'If user-supplied djustment factors are used, the file should be
#'in .csv format with a header row. The headers should be "load" and one or more
#'pollutants listed in the \code{pollutants} argument. The load column should
#'contain numerical values in percentage space (i.e., it can range between 0 and
#'100). The pollutant columns should contain the specific adjustment factors for
#'that pollutant, corresponding to the load values. Note: The function will
#'linearly interpolate between the nearest values for any given load factor, and
#'will return an adjustment factor of 1 for load factors outside the range of
#'the table (i.e., it will not extrapolate).
#'
#'Note: If it is desired that baseline BSFC values should be altered according
#'to main engine load only within a specified set of bounds, and the baseline
#'value should be used outside of those bounds, pass the bounds as a string
#'using the \code{loadBasedBSFC} argument. The bounds should be formatted as
#'such: the lower bound first, then a comma, then the upper bound.
#'
#'@return
#' a data.table of emission factors or low load adjusted emission factors
#' (depending on the \code{output} argument) for the given parameters. Each
#' pollutant selected in the \code{pollutants} argument will be a column in the
#' table.
#'
#' @references
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Ports Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@examples
#'calcEF(engineType=c("SSD","MSD","MSD-ED","GT"),
#'            location=c("ECA","OutsideECA","GreatLakes","ECA"),
#'            loadFactor=c(0.8,0.5,0.14,0.03),
#'            loadBasedBSFC="0.2,1",
#'            output="EF_LLAF",
#'            pollutants="co2",
#'            tier=c("Tier 3","Tier 2","Tier 1", "Tier 0"))
#'calcEF(engineType=c("SSD","MSD","MSD-ED","GT"),
#'            location=c("ECA","OutsideECA","GreatLakes","ECA"),
#'            loadFactor=c(0.8,0.5,0.14,0.03),
#'            loadBasedBSFC="Y",
#'            output="EF_LLAF",
#'            main_aux_boiler="main",
#'            tier=c("Tier 3","Tier 2","Tier 1", "Tier 0"))
#'calcEF(engineType=c("SSD","SSD"),
#'            location=c("ECA","ECA"),
#'            loadFactor=c(0.8,0.03),
#'            output="EF",
#'            main_aux_boiler="main",
#'            tier=c("Tier 3","Tier 2"))
#'calcEF(engineType=c("SSD","SSD"),
#'            location=c("ECA","ECA"),
#'            loadFactor=c(0.8,0.8),
#'            output="EF",
#'            main_aux_boiler="main",
#'            tier=c("Tier 3","Tier 2"))
#'calcEF(engineType=c("HSD","MSD","LNG"),
#'            location=c("ECA","OutsideECA","ECA"),
#'            loadFactor=c(0.8,0.5,0.14),
#'            ECAfuelSulfurPercentage=0.1,
#'            GlobalfuelSulfurPercentage=2.7,
#'            pollutants="ALL",
#'            output="EF",
#'            main_aux_boiler="aux",
#'            tier=c("Tier 3","Tier 2", "Tier 0"))
#'calcEF(engineType=c("MSD","SSD"),
#'            location=c("ECA","OutsideECA"),
#'            loadFactor=c(0.8,0.5),
#'            ECAfuelSulfurPercentage=0.1,
#'            GlobalfuelSulfurPercentage=2.7,
#'            pollutants="ALL",
#'            output="EF",
#'            main_aux_boiler="boiler",
#'            tier=c("Tier 0", "Tier 0"))
#'
#'@import data.table
#'@importFrom utils data
#'@importFrom utils tail
#'@export


calcEF<- function(engineType, tier, location, loadFactor=NULL,
                        ECAfuelSulfurPercentage=0.1,GlobalfuelSulfurPercentage=0.5,
                        pollutants="ALL",loadBasedBSFC="N",output="EF_LLAF", main_aux_boiler="main",
                       inputTableLocation=NULL
){


if(is.element("ALL",pollutants)){
  pollutants<-c("hc",
                "co2",
                "co",
                "nox",
                "pm2.5",
                "so2",
                "pm10")
}
#initialize PollutantsEF List ====================================
lengthOfInputs <- max(length(engineType), length(tier), length(location), length(loadFactor))
PollutantsEF<- lapply(pollutants, function(x) data.table::data.table(matrix(nrow=lengthOfInputs)))
names(PollutantsEF)<-pollutants

#Turn on or off load factor based sfoc modeling =================
#No load based modeled BSFC
if(loadBasedBSFC=="N"){
  loadFactorEF<- NULL
#Load based modeled BSFC for main engine load factors 0-1
} else if(loadBasedBSFC=="Y"){
  loadFactorEF<- loadFactor
#Load based modeled BSFC for main engine load factors within specified range.
} else {
  lowerBoundLF<-as.numeric(gsub("^(.*?),.*", "\\1", loadBasedBSFC))
  upperBoundLF<-as.numeric(gsub("^.*,(.*?)", "\\1", loadBasedBSFC))

  loadFactorEF<-rep(NA,lengthOfInputs)
  loadFactorEF[which(loadFactor>=lowerBoundLF & loadFactor<=upperBoundLF)]<-loadFactor[
    which(loadFactor>=lowerBoundLF & loadFactor<=upperBoundLF)]
}

#================================================================
for(pol in pollutants){
 ifelse(pol=="hc",
        PollutantsEF[paste(pol)]<-calcEF_HC(engineType, main_aux_boiler=main_aux_boiler)
        ,
        ifelse(pol=="co2",
                      PollutantsEF[paste(pol)]<-calcEF_CO2(engineType=engineType, location=location,loadFactor = loadFactorEF, main_aux_boiler=main_aux_boiler)
                      ,
               ifelse(pol=="co",
                      PollutantsEF[paste(pol)]<-calcEF_CO(engineType=engineType, main_aux_boiler=main_aux_boiler)
                      ,
                      ifelse(pol=="nox",
                             # Note: using the user-supplied loadFactor argument here instead of loadFactorEF because we always need to pass loadFactors to calcEF_NOx
                             #       (loadFactorEF is NULL if loadBasedBSFC="N")
                             PollutantsEF[paste(pol)]<-calcEF_NOx(engineType=engineType, location=location, tier=tier, loadFactor=loadFactor, main_aux_boiler=main_aux_boiler)
                             ,
                             ifelse(pol=="pm2.5",
                                    PollutantsEF[paste(pol)]<-calcEF_PM(engineType=engineType, location=location, loadFactor=loadFactorEF, ECAfuelSulfurPercentage=ECAfuelSulfurPercentage,
                                                                         GlobalfuelSulfurPercentage=GlobalfuelSulfurPercentage,
                                                                         pmSize = "pm2.5", main_aux_boiler=main_aux_boiler)
                                        ,
                                    ifelse(pol=="so2",
                                            PollutantsEF[paste(pol)]<-calcEF_SO2(engineType=engineType, location=location, loadFactor=loadFactorEF, ECAfuelSulfurPercentage=ECAfuelSulfurPercentage,
                                                                                  GlobalfuelSulfurPercentage=GlobalfuelSulfurPercentage,
                                                                                   main_aux_boiler=main_aux_boiler)
                                                  ,
                                           ifelse(pol=="pm10",
                                            PollutantsEF[paste(pol)]<-calcEF_PM(engineType=engineType, location=location, loadFactor=loadFactorEF, ECAfuelSulfurPercentage=ECAfuelSulfurPercentage,
                                                                                 GlobalfuelSulfurPercentage=GlobalfuelSulfurPercentage,
                                                                                 pmSize = "pm10", main_aux_boiler=main_aux_boiler)
                                                  ,NA
                                                  )
                                           )
                                    )
                             )
                      )
               )
 )

}

#auxiliary emission factors (below) are not affected by low loads
if(main_aux_boiler=="main"){

  if(output=="EF"){
    Pollutants<- PollutantsEF
  }else{ # EF_LLAF

    PollutantsLLAF<-calcLLAF(engineType, location, loadFactor,
                             ECAfuelSulfurPercentage,GlobalfuelSulfurPercentage,
                             pollutants,
                             inputTableLocation)

    Pollutants<- Map('*', PollutantsEF, PollutantsLLAF)
  }
#auxiliary and boiler emission factors (below) are not affected by low loads
}else{
  Pollutants<- PollutantsEF
}

Pollutants<- data.table::as.data.table(do.call(cbind,Pollutants))

return(Pollutants)
}
