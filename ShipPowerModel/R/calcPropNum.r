#' @title calcPropNum
#'
#' @description Calculate number of propellers (\code{nProp}) by ship type
#' trends.
#'
#'@param shipType Ship type (vector of strings, see \code{\link{calcShipType}})
#' @param paxTugShipTypes Ship types specified in input \code{shipTypes} to
#' be modeled as passenger and tug vessels. (vector of strings)
#'
#' @details
#' Based on a sample of 2,700 ships from Clarksons.
#'
#' This method this requires ship types to be grouped. Use the
#' \code{paxTugShipTypes} grouping parameter to provide these ship type
#' groupings. Any ship types not included in these groupings will be considered
#' as miscellaneous vessels.
#'
#' @return \code{nProp} (vector of numericals)
#'
#' @examples
#' calcPropNum(c("container.ship","bulk.carrier","tug"))
#'
#' @export

calcPropNum <-function(shipType, paxTugShipTypes=c(
  "cruise.ed","passenger","ferry.pax","ferry.ro.pax","cruise","yacht","tug","service.tug")){
  #2. Determine number of propellers

  nprop<-ifelse(#case
    (shipType %in% paxTugShipTypes),
    #if true return:
    2,
    #otherwise return:
    1
  )#end ifelse
  return(nprop)
}
