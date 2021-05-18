#' @title calcLLAF
#'
#' @description Creates a data table of low load adjustment factors (unitless)
#' for the given parameters.
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
#'@param location Location of vessel (vector of strings). Valid values are:
#'\itemize{
#'   \item "ECA"
#'   \item "OutsideECA"
#'   \item "GreatLakes"
#' }
#'@param loadFactor Fractional percentage (between 0 and 1) of main engine
#' required to propel vessel at given speed (vector of numericals) (see
#' ShipPowerModel library)
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
#' @param inputTableLocation File path (optional). Used to specify a
#' user-supplied set of adjustment factors. See details for formatting
#' requirements
#'
#'@details
#'Location is important for determining the fuel being used, as type of fuel
#'typically used varies by location. This impacts the calculation of LLAF
#'for SO2.
#'
#'For more information about the default low load adjustment factors, see
#'Section 3.7 of the Port Emissions Inventory Guidance.
#'
#'If user-supplied adjustment factors are used, the file should be
#'in .csv format with a header row. The headers should be "load" and one or more
#'pollutants listed in the \code{pollutants} argument. The load column should
#'contain numerical values in percentage space (i.e., it can range between 0 and
#'100). The pollutant columns should contain the specific adjustment factors for
#'that pollutant, corresponding to the load values. Note: This function will
#'linearly interpolate between the nearest values for any given load factor, and
#'will return an adjustment factor of 1 for load factors outside the range of
#'the table (i.e., it will not extrapolate).
#'
#'@return
#' a data.table of low load adjusted emission factors the given parameters. Each
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
#'calcLLAF(engineType=c("SSD","MSD","MSD-ED","GT"),
#'         location = "ECA",
#'         loadFactor=c(0.8,0.5,0.14,0.03),
#'         pollutants="co2")
#'calcLLAF(engineType=c("SSD","MSD","MSD-ED","GT"),
#'         location = c("ECA","GreatLakes","ECA","OutsideECA"),
#'         loadFactor=c(0.8,0.5,0.14,0.03))
#'calcLLAF(engineType=c("HSD","MSD","LNG"),
#'         location = c("ECA","GreatLakes","ECA","OutsideECA"),
#'         loadFactor=c(0.8,0.5,0.14),
#'         ECAfuelSulfurPercentage=0.1,
#'         GlobalfuelSulfurPercentage=2.7,
#'         pollutants="ALL")
#'
#'@import data.table
#'@importFrom stats spline
#'@export

calcLLAF <- function(engineType, location, loadFactor,
                     ECAfuelSulfurPercentage=0.1,GlobalfuelSulfurPercentage=0.5,
                     pollutants="ALL",
                     inputTableLocation=NULL)
{

  #bind variables to make devtools::check() happy
  so2<-.<-NULL

  if(is.element("ALL",pollutants)){
    pollutants<-c("hc",
                  "co2",
                  "co",
                  "nox",
                  "pm2.5",
                  "so2",
                  "pm10")
  }

  #get low-load adjustments if an input table is given
  if(is.null(inputTableLocation) == FALSE)
    {
     tbl<-fread(inputTableLocation)

     # convert load from percent space to fractional space to make it consistent with loadFactor
     tbl[,load:=load/100]

     # only include the selected pollutants
     tbl<-subset(tbl, select=c("load", pollutants) )

     # linear interpolate the table for each given load factor
     LLAF<-tbl[, lapply(.SD, function(y) spline(load, y, xout = loadFactor)[['y']])]

     # Excluding electric drive oil engines (MSD-ED & GT-ED) who can switch off engines instead of operating at low loads.
     LLAF[engineType %in% c("MSD-ED","GT-ED")]<-LLAF[, lapply(.SD, function(y) 1)]

     # Excluding load factors that fall outside the range of this table
     LLAF[is.na(load)]<-LLAF[, lapply(.SD, function(y) 1)]

     # drop load column so we are just returning the results
     LLAF[, load:=NULL]
     return(LLAF)
    }

  #calculate low-load adjustments
  LLAFCoeff<-ShipEF::LLAFCoeff
  #initialize PollutantsLLAF====================================
  LLAF<- lapply(pollutants, function(x) data.table::data.table(matrix(nrow=length(engineType))))

  names(LLAF)<-pollutants

  #assign FuelSulfurFraction according to ECA constraints=======

  for(pol in pollutants){

    #Equation 3-26 C3RIA pg.3-27
    #Normalized to 20% load
    if(pol=="so2") {
       # create data table of location and load factor so that we can calculate SO2 LLAF from loadFactor differently depending on location
       dt <- data.table::data.table(location, loadFactor)

       dt[location %in% c("ECA", "GreatLakes"),
          so2 := (LLAFCoeff$a[LLAFCoeff$pollutant=="so2"]*((14.1205*(1/loadFactor)+205.7169) * ECAfuelSulfurPercentage)
                             +LLAFCoeff$b[LLAFCoeff$pollutant=="so2"])/(LLAFCoeff$a[LLAFCoeff$pollutant=="so2"]*((14.1205*(1/0.2)+205.7169)
                                                                                                                 * ECAfuelSulfurPercentage)+LLAFCoeff$b[LLAFCoeff$pollutant=="so2"])]

       dt[!(location %in% c("ECA", "GreatLakes")),
          so2 := (LLAFCoeff$a[LLAFCoeff$pollutant=="so2"]*((14.1205*(1/loadFactor)+205.7169) * GlobalfuelSulfurPercentage)
                  +LLAFCoeff$b[LLAFCoeff$pollutant=="so2"])/(LLAFCoeff$a[LLAFCoeff$pollutant=="so2"]*((14.1205*(1/0.2)+205.7169)
                                                                                                      * GlobalfuelSulfurPercentage)+LLAFCoeff$b[LLAFCoeff$pollutant=="so2"])]
       # LLAF is a list containing data.table columns
       LLAF[paste(pol)] <- dt[, .(so2)]
       rm(dt)

    } else { # All other pollutants

       LLAF[paste(pol)]<-data.table::as.data.table(
         (LLAFCoeff$a[LLAFCoeff$pollutant==pol]*(loadFactor^-LLAFCoeff$x[LLAFCoeff$pollutant==pol])+LLAFCoeff$b[LLAFCoeff$pollutant==pol])/(
           LLAFCoeff$a[LLAFCoeff$pollutant==pol]*(0.2^-LLAFCoeff$x[LLAFCoeff$pollutant==pol])+LLAFCoeff$b[LLAFCoeff$pollutant==pol])
       )

    } #end SO2 vs other pollutants if/else
  }

  #Excluding electric drive oil engines (MSD-ED & GT-ED) who can switch off engines instead of operating at low loads.
  #Excluding Auxiliary Engines
  #Excluding Non-Low Load Operations (>20% or 0% (Hotelling))
  LLAF<-lapply(LLAF, function(x){
    x[which(loadFactor>0.2|loadFactor==0)]<-1
    x
  })

  LLAF<-lapply(LLAF, function(x){
    x[which(engineType%in%c("MSD-ED","GT-ED"))]<-1
    x
  })

  LLAF<-data.table::as.data.table(do.call(cbind,LLAF))

  return(LLAF)
}
