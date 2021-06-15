#' @title calcTestIMO
#'
#' @description
#' Determines whether or not an IMO number is in the correct format. IMO numbers
#' should have 7 digits with a check digit at the end.
#'
#' @param IMO IMO number (vector of numericals or strings)
#' @param correctFlag Output indicator for rows where the IMO Number is in the
#' correct format
#' @param incorrectFlag Output indicator for rows where the IMO Number is not in
#' the correct format
#'
#' @return
#' \code{IMO_number_flag}, a factorized vector containing either the value of
#' \code{correctFlag} or \code{incorrectFlag}, depending on the correctness of
#' the IMO number
#'
#' @examples
#' calcTestIMO(IMO=c(899046900,0,9394935))
#'
#' @export

calcTestIMO<-function(IMO,correctFlag="Correct",incorrectFlag="Incorrect"){

#Correct Flag
IMO_NUMBER_flag<-rep(correctFlag,length(IMO))

#Test Right Legth:
IMO<-as.character(IMO)
imolist<-strsplit(unique(IMO), split="")
rightLength<-which(lapply(imolist, length)==7)
imolist<-imolist[rightLength]
rm(rightLength)


#For those of right length, test check digit:

imolist<-lapply(imolist, as.numeric)

checkDigit<-lapply(imolist, function(x) sapply(strsplit(
  as.character((x[1]*7)+(x[2]*6)+(x[3]*5)+(x[4]*4)+(x[5]*3)+(x[6]*2)),
  split=""), tail, 1))

test<-unlist(lapply(checkDigit, as.numeric))==unlist(lapply(imolist, function(x) x[7]))

rm(checkDigit)
imolist<-imolist[which(test==TRUE)]

imolist<- lapply(imolist, function(x)paste(x, collapse=""))

#Incorrect Flag:
IMO_NUMBER_flag[which(!IMO%in%imolist)]<-incorrectFlag

IMO_NUMBER_flag<-as.factor(IMO_NUMBER_flag)
return(IMO_NUMBER_flag)

}
