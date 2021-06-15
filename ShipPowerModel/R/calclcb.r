#'@title calclcb
#'
#'@description Calculate longitudinal position of center of buoyancy
#'(\code{lcb}).
#'
#'@param lwl Waterline length (vector of numericals, m) (see \code{\link{calclwl}})
#'
#'@details
#'The longitudinal position of center of buoyancy describes center of buoyancy
#'forward of half of the waterline length (\code{lwl}) (decimal percentage of
#'waterline length).
#'
#'@return \code{lcb} (vector of numericals)
#'
#'@references
#'\href{http://hdl.handle.net/11250/2410741}{Rakke, S. G. 2016. "Ship Emissions
#'Calculation from AIS." NTNU.}
#'
#'@seealso
#'\code{\link{calclwl}}
#'
#'@examples
#'calclcb(c(218.75,400,262))
#'
#'@export


calclcb<- function(lwl){

  lcb<- -0.75*(lwl/2)/100

  return(lcb)
}
