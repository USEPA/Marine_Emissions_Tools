#' @title calcCf
#'
#' @description  Calculates the frictional resistance coefficient (\code{Cf})
#' (dimensionless).
#'
#' @param shipSpeed Ship actual speed (vector of numericals, m/s) (see
#' \code{\link{calcSpeedUnitConversion}})
#' @param lwl Waterline length (vector of numericals, m) (see 
#' \code{\link{calclwl}})
#' @param seawaterTemp Sea water temperature. Default = 15 (degrees Celsius).
#' Can supply either a vector of numericals, a single number, or rely on the 
#' default
#' @param seawaterDensity Sea water density. Default = 1.025 (g/cm^3). Can 
#' supply either a vector of numericals, a single number, or rely on the default
#'
#' @details
#' Calculates frictional resistance coefficient using the International Tank
#' Towing Committee (ITTC) 1957 method where:
#' \deqn{Cf = \frac{0.075}{(log(V*Lwl/visc)-2)^2}}{Cf = 0.075/((log(V*Lwl/visc)-2)^2)}
#'
#' @return \code{Cf} (vector of numericals, dimensionless)
#'
#' @references
#'Kristensen, H. O. and Lutzen, M. 2013. "Prediction of Resistance and Propulsion
#'Power of Ships."
#'
#'@seealso \itemize{
#'\item \code{\link{calcSpeedUnitConversion}}
#'\item \code{\link{calclwl}}
#'}
#'
#' @family Resistance Calculations
#'
#' @examples
#' calcCf(seq(1,5,1), 218.75, 15, 1.025)
#'
#' @export


calcCf <- function(shipSpeed, lwl, seawaterTemp=15, seawaterDensity=1.025){

  Cf<-0.075/((log10(
    (shipSpeed*lwl)/
                      (((43.4233-31.38*seawaterDensity)*
                          ((seawaterTemp+20)^(1.72*seawaterDensity-2.202))+
                          4.7478-5.779*seawaterDensity)*(10^(-6)))

    )
    -2)^(2))


 return(Cf)
}


