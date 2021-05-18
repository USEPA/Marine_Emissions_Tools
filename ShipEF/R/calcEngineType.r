#'@title calcEngineType
#'
#'@description
#'Assigns a standardized engine type descriptor using engine stroke and engine
#'rpm data.
#'
#'@param propulsionType Descriptor of engine propulsion type (from IHS) (vector
#'of strings)
#'@param mainEngineStrokeType Engine stroke type (vector of ints). Valid values
#'are: \itemize{
#' \item 4
#' \item 2
#' \item \code{NA}
#'}
#'@param mainEngineRPM Engine revolutions per minute (vector of numericals)
#'@param MSD_SSD_RPM_CutOff Cutoff rpm value between medium and slow speed
#'diesel engines. Default = 500 rpm.
#'@param main_aux_boiler Is this calculation for a propulsive (main), auxiliary
#'(aux), or boiler engine? Options: \itemize{
#' \item "main" (Default)
#' \item "aux"
#' \item "boiler"
#'}
#'
#'@details
#'For more information about assigning engine speed, see Section 3.3.2.2 of the
#'Port Emissions Inventory Guidance.
#'
#'@return \code{engineType} (vector of strings). Valid values are: \itemize{
#'\item "SSD" = Slow-speed diesel
#'\item "MSD" = Medium-speed diesel
#'\item "GT" = Gas turbine
#'\item "ST" = Steam turbine
#'\item "MSD-ED" = Electric drive MSD
#'\item "GT-ED" = Electric drive GT
#'\item "LNG" = Liquified natural gas
#'\item "HSD" = High-speed diesel (auxiliary engines only)
#'\item "Boiler" = Boiler engine (boilers only)
#'}
#'
#'@references
#' \href{https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P10102U0.pdf}{EPA. 2020.
#' "Ports Emissions Inventory Guidance: Methodologies for Estimating
#' Port-Related and Goods Movement Mobile Source Emissions." Ann Arbor, MI:
#' Office of Transportation and Air Quality. US Environmental Protection Agency.}
#'
#'@examples
#'calcEngineType(propulsionType=c("Oil Engine(s), Geared Drive",
#'                                "Oil Engine(s), Geared Drive",
#'                                "Sail, Aux Oil Eng(s), Geared"),
#'               mainEngineStrokeType = c(2,4,4),
#'               mainEngineRPM = c(NA,1515,1800))
#'
#'calcEngineType(propulsionType=c("Oil Engine(s), Geared Drive",
#'                                "Oil Engine(s), Geared Drive",
#'                                "Sail, Aux Oil Eng(s), Geared"),
#'               mainEngineStrokeType = c(2,4,4),
#'               mainEngineRPM = c(NA,1515,1800),
#'               main_aux_boiler = "aux")
#'
#'@import data.table
#'@export


calcEngineType<- function(propulsionType,mainEngineStrokeType=NULL,mainEngineRPM=NULL,MSD_SSD_RPM_CutOff=500,main_aux_boiler="main"){

#sets up main_aux_boiler as vector so it can be analyzed in ifelse
if(length(main_aux_boiler)==1){
  main_aux_boiler<-rep(main_aux_boiler, length(propulsionType))
}

engineType<-ifelse(grepl("aux", main_aux_boiler, ignore.case=TRUE),
                   "MSD",
                   ifelse(grepl("boiler", main_aux_boiler, ignore.case=TRUE),
                          "Boiler",
                          ifelse(grepl("Sail|Non-Propelled", propulsionType)==TRUE,
                                 "Non-Propelled",
                                 ifelse(grepl("Gas", propulsionType)==TRUE,
                                        "GT",
                                        ifelse(grepl("Steam|St.", propulsionType)==TRUE,
                                               "ST",
                                               ifelse(mainEngineStrokeType==4 & is.na(mainEngineStrokeType)==FALSE,
                                                      "MSD",
                                                      ifelse(mainEngineStrokeType==2 & is.na(mainEngineStrokeType)==FALSE,
                                                             "SSD",
                                                             ifelse(mainEngineRPM>MSD_SSD_RPM_CutOff & is.na(mainEngineRPM)==FALSE ,
                                                                    "MSD",
                                                                    ifelse(mainEngineRPM<=MSD_SSD_RPM_CutOff& is.na(mainEngineRPM)==FALSE,
                                                                           "SSD",
                                                                            NA
                                                                          )
                                                                    )
                                                             )
                                                      )
                                               )
                                        )
                                 )
                          )
                  )

engineType[engineType==""]<-NA

#Don't assign electric diesel designation to auxiliary engines
engineType[main_aux_boiler == "main" &
           grepl("El",propulsionType)==TRUE &
           grepl("ST|NA|Non-Propelled|SSD", engineType)==FALSE
          ]<- paste(
                     engineType[main_aux_boiler == "main" &
                                (grepl("El",propulsionType)==TRUE &
                                grepl("ST|NA|Non-Propelled|SSD", engineType)==FALSE)
                               ],
                     "-ED",
                     sep=""
                   )

engineType[engineType=="NA-ED"]<-NA


return(engineType)
}
