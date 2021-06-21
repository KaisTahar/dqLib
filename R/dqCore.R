# Last Change at 21.06.2021
# Kais Tahar
# this script provides core functions for data quality analysis

env <- new.env(parent=globalenv())
setGlobals <- function(medData, repCol, cdata,tdata) {
  env$medData <- medData
  env$cdata <- cdata
  env$tdata <- tdata
  env$dq <- subset(medData, select = repCol)
  env$dq$dq_msg<-""
}

getFileExtension <- function(filePath){
  ext <- strsplit(basename(filePath), split="\\.")[[1]]
  return(ext[-1])
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
  bdata$missing_value_no[index] <- sum(as.integer(as.character(bdata$missing_value_no[-index])),na.rm=TRUE)
  bdata$N[index]<-sum(bdata$N_Item[-index],na.rm=TRUE)
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
  if(!"missing_value_no" %in% colnames(bdata)) bdata$missing_value_no <-NA
  if(!"missing_value_rate" %in% colnames(bdata)) bdata$missing_value_rate <-NA
  if(!"N_Item" %in% colnames(bdata)) bdata$N_Item <-NA
  if(n>0){
    bdata$N_Item[index] <-n
    bdata$missing_value_no[index] <- m
    mr <-(bdata$missing_value_no[index]/ bdata$N_Item[index]) * 100
    bdata$missing_value_rate[index] <- round (mr,1)
  }
  else if (item!="Total"){
    bdata$N_Item[index] <- 0
    bdata$missing_value_no[index] <- 0
    bdata$missing_value_rate[index] <-0
  }
  bdata
}

getMissing<-function (itemCol, outCol1, outCol2){
  if (!is.empty(itemCol))
  {
    for (i in itemCol) {
      cdata <-missingCheck(i,itemCol,outCol1, outCol2)$cdata
      dq <- missingCheck(i,itemCol,outCol1, outCol2)$dq
    }
  }
  out <- list()
  out[["dq"]] <- dq
  out[["cdata"]] <- cdata
  out
}

missingCheck<- function (item, itemCol, cl1, cl2) {
  dqItem.vec <- itemCol
  index <- which(dqItem.vec==item)[[1]]
  item.vec <- env$medData[[item]]
  if(!is.empty(item.vec)){
    nq <- which(item.vec =="" | is.na(item.vec))
      if (!is.empty (nq)) for(i in nq)
      {
        msg <- paste("Fehlendes ", item)
        if (index >1) env$dq[,cl1][i] <- paste(msg, env$dq[,cl1][i])
        else env$dq [,cl2] [i] <- msg
      }
    env$cdata <- addMissing(item, env$cdata,length(nq), length(item.vec))
  }
  out <- list()
  out[["dq"]] <- env$dq
  out[["cdata"]] <- env$cdata
  out
}
