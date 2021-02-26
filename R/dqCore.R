# Last Change at 26.02.2021
# Kais Tahar
# this script provides core functions for data quality analysis

env <- new.env(parent=globalenv())
setGlobals <- function(medData, cdata) {
  env$medData <- medData
  env$cdata <- cdata
  env$dq <- medData
  env$dq$dq_msg<-""
}

getDQStatis <-function(bdata, col, row){
  tdata<- addTotalCount(bdata, col, row)
  tcdata <-addCompletness (tdata, col, row)
  sdata <-subset(tcdata, tcdata[,col]==row)
  sdata$N_Item <- NULL
  return <-sdata
}

addTotalCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$missing_value_no[index] <- sum(as.integer(as.character(bdata$missing_value_no[-index])))
  bdata$N[index]<-sum(bdata$N_Item[-index])
  mr <- (bdata$missing_value_no[index]/ bdata$N[index])* 100
  bdata$missing_value_rate[index] <- round (mr,2)
  bdata
}

addCompletness<- function (tdata, col, row) {
  index = which( tdata[,col]==row)
  tdata$completness_rate[index]<-round (100-tdata$missing_value_rate[index],2)
  return <- tdata
}

addMissing<- function (item, bdata, m, n) {
  index = which(bdata$basicItem==item)[1]
  if (is.null(index)) bdata$basicItem[1]=item
  if(n>0){
    bdata$N_Item[index] <-n
    bdata$missing_value_no[index] <- m
    mr <-(bdata$missing_value_no[index]/ bdata$N_Item[index]) * 100
    bdata$missing_value_rate[index] <- round (mr,1)
  }
  else {
    bdata$N_Item[index] <- 0
    bdata$missing_value_no[index] <- 0
    bdata$missing_value_rate[index] <-0
  }
  bdata
}
