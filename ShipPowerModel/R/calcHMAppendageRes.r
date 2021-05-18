#'@title calcHMAppendageRes
#'
#'@description Calculate appendage resistance (\code{Rapp}) (kN) from the
#'Holtrop & Mennen method.
#'
#'@param shipSpeed Ship actual speed (vector of numericals, m/s) (see 
#' \code{\link{calcSpeedUnitConversion}})
#'@param Cf Frictional resistance coefficient (vector of numericals, 
#' dimensionless) (see \code{\link{calcCf}})
#'@param appendagesList List of appendages on ship (vector of strings) \itemize{
#'\item"rudder behind skeg"
#'\item"rudder behind stern"
#'\item"twin-screw balance rudders"
#'\item"shaft brackets"
#'\item"skeg"
#'\item"strut bossings"
#'\item"hull bossings"
#'\item"shafts"
#'\item"stabilizer fins"
#'\item"dome"
#'\item"bilge keels"
#'}
#'@param wettedAppSAList List of wetted surface areas corresponding to list of appendages
#' (vector of numericals, m^2)
#'@param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can 
#' supply either a vector of numericals corresponding to the ship speed, provide a single
#' number, or rely on the default
#'
#'@return \code{Rapp} (vector of numericals, kN)
#'
#'@references
#'Holtrop, J. and Mennen, G. G. J. 1982. "An approximate power prediction
#'method." International Shipbuilding Progress 29.
#'
#'@seealso \itemize{
#'\item \code{\link{calcSpeedUnitConversion}}
#'\item \code{\link{calcCf}} }
#'
#'@family Holtrop-Mennen Calculations
#'@family Resistance Calculations
#'
#'@examples calcHMAppendageRes(seq(1,5,1),0.0015,"rudder behind skeg",50,seawaterDensity=1.025)
#' calcHMAppendageRes(seq(1,5,1),0.0015,NA,0,seawaterDensity=1.025)
#'
#'@export

calcHMAppendageRes<- function(shipSpeed,Cf,
       appendagesList,wettedAppSAList,seawaterDensity=1.025){


AppResFactor <- data.frame(appendage=c("rudder behind skeg","rudder behind stern",
"twin-screw balance rudders","shaft brackets","skeg","strut bossings","hull bossings",
"shafts","stabilizer fins","dome","bilge keels"),
resistance.factor=c(1.5,1.4,2.8,3,1.75,3,2,3,2.8,2.7,1.4))

wettedAppSAList[is.na(wettedAppSAList)==TRUE]<-0
appendagesList<- tolower(appendagesList)

FormFactorEqu=0

for(i in 1:length(appendagesList)){
  if(length(AppResFactor$resistance.factor[AppResFactor$appendage==paste(appendagesList[i])]*
      wettedAppSAList[i])>0){
 j<- AppResFactor$resistance.factor[grepl(appendagesList[i],AppResFactor$appendage)==TRUE]*
   wettedAppSAList[i]}else{j<-0}
  FormFactorEqu=FormFactorEqu+j
}

FormFactorEqu<-FormFactorEqu/sum(wettedAppSAList)
FormFactorEqu[is.na(FormFactorEqu)]<-0
Rapp<-0.5*seawaterDensity*(shipSpeed^2)*wettedAppSAList*FormFactorEqu*Cf

return(Rapp)
}

