# Kais Tahar
# this script provides core functions for data quality analysis

#env <- new.env(parent=globalenv())
#env <- new.env(parent = emptyenv())
env <- new.env()
setGlobals <- function(medData, repCol, cdata,ddata, tdata) {
  env$medData <- medData
  env$cdata <- cdata
  env$ddata <-ddata
  env$tdata <- tdata
  env$dq <- subset(medData, select = repCol)
  env$dq$dq_msg<-""
}

getFileExtension <- function(filePath){
  ext <- strsplit(basename(filePath), split="\\.")[[1]]
  return(ext[-1])
}

getTotalStatistic <- function(dqInd, col, row){
  env$cdata<- addStatistic(env$cdata, col, row)
  env$ddata<- addStatistic(env$ddata, col, row)
  bdata <- merge(env$cdata, env$ddata, by=intersect(names(env$cdata), names(env$ddata)), all = TRUE)
  bdata$Item_no<- 1
  index = which(bdata[,col]==row)
  bdata<-bdata[-index,]
  bdata[nrow(bdata) + 1, ] <- list ("Total",0,0,0,0,0, 0, nrow(bdata)-1)
  bdata<- addStatistic(bdata, col, row)
  tcdata <- addCompletness (bdata, col, row)
  total <- subset(tcdata, tcdata[,col]==row)
  env$tdata<- cbind(total,env$tdata)
  stotal <- subset(env$tdata, select= dqInd)
  stotal
}

getTotalStatisticx <- function(cdata, ddata,col, row){
  ddata<- addStatistic(ddata, col, row)
  cdata<- addStatistic(cdata, col, row)
  bdata <- merge(cdata, ddata, by=intersect(names(cdata), names(ddata)), all = TRUE)
  bdata$Item_no<- 1
  index = which(bdata[,col]==row)
  bdata<-bdata[-index,]
  bdata[nrow(bdata) + 1, ] <- list ("Total",0,0,0,0,0, 0, nrow(bdata)-1)
  bdata<- addStatistic(bdata, col, row)
  tcdata <-addCompletness (bdata, col, row)
  sdata <-subset(tcdata, tcdata[,col]==row)
  #sdata$N_Item <- NULL
  return <-sdata
}

addStatistic<- function (bdata, col, row) {
  bdata =addMissingCount(bdata,col,row)
  bdata = addOutlierCount(bdata,col,row)
  bdata
}

addMissingCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$N_Item[index]<-sum(bdata$N_Item[-index], na.rm=TRUE)
  bdata$missing_value_no[index] <- sum(as.integer(as.character(bdata$missing_value_no[-index])), na.rm=TRUE)
  mr <- (bdata$missing_value_no[index]/ bdata$N[index])* 100
  bdata$missing_value_rate[index] <- round (mr,2)
  bdata
}

addOutlierCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$N_Item[index]<-sum(bdata$N_Item[-index],na.rm=TRUE)
  bdata$outlier_no[index] <- sum (as.integer(as.character( bdata$outlier_no[-index] )), na.rm=TRUE)
  bdata$outlier_check_no[index] <- sum (as.integer(as.character( bdata$outlier_check_no[-index] )), na.rm=TRUE)
  if (bdata$outlier_no[index]>0) {
    or <- (bdata$outlier_no[index] / as.integer(bdata$outlier_check_no[index]))* 100
    bdata$outlier_rate[index] <- round (or,2)}
  else  bdata$outlier_rate[index] <- 0
  bdata
}

addOutlier<- function (item, bdata, m,n) {
  index = which(bdata$basicItem==item)[1]
  if (is.null(index)) bdata$basicItem[1]=item
  if(!"outlier_no" %in% colnames(bdata)) bdata$outlier_no <-0
  if(!"outlier_rate" %in% colnames(bdata)) bdata$outlier_rate <-0
  if(!"N_Item" %in% colnames(bdata)) bdata$N_Item <-0
  if(!"missing_value_no" %in% colnames(bdata)) bdata$missing_value_no <-0
  if(!"outlier_check_no" %in% colnames(bdata)) bdata$outlier_check_no <-0
  if(n>0){
    bdata$N_Item[index] <-n
    bdata$outlier_check_no[index] <- bdata$N_Item[index]-bdata$missing_value_no[index]
    if (!is.na(bdata$outlier_no[index]) && is.numeric(bdata$outlier_no[index]) ) bdata$outlier_no[index] <- bdata$outlier_no[index]+m
    else bdata$outlier_no[index] <- m
    #or <- (bdata$outlier_no[index]/ bdata$N_Item[index]) * 100
    if (bdata$outlier_no[index]>0) {
      or <- (bdata$outlier_no[index]/ bdata$outlier_check_no[index]) * 100
      bdata$outlier_rate[index] <- round (or,1)
    }else bdata$outlier_rate[index] <-0
  }
  else if (item!="Total"){
    bdata$N_Item[index]<- 0
    bdata$outlier_no[index] <- NA
    bdata$outlier_rate[index] <- NA
  }
  
  bdata
}

getDateOutlier<- function (dItem.vec){
  now<- as.Date(Sys.Date())
  out <-  vector()
  out <- which(isDate(dItem.vec) & (as.Date(dItem.vec)>now))
  out
}

getAgeMaxOutlier<- function ( dItem1.vec, dItem2.vec, n){
  diff <-  ifelse ((isDate(dItem1.vec) & isDate(dItem2.vec)), as.numeric(difftime(dItem1.vec, dItem2.vec),units="weeks")/52.25 , 0 )
  out <- which(abs(diff)>n)
  out
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
  bdata$outlier_no[index] <- sum (as.integer(as.character( bdata$outlier_no[-index] )), na.rm=TRUE)
  or <- (bdata$outlier_no[index] / bdata$N_Item[index] )* 100
  bdata$outlier_rate[index] <- round (or,2)
  bdata
}

addCompletness<- function (tdata, col, row) {
  index = which( tdata[,col]==row)
  tdata$completness_rate[index]<-round (100-tdata$missing_value_rate[index],2)
  return <- tdata
}

getMissingValue<-function (df, bItemCol, outCol1,outCol2){
  if(!outCol1 %in% colnames(env$dq)) env$dq[,outCol1]<-NA
  if(!outCol2 %in% colnames(env$dq)) env$dq[,outCol2] <-""
  bItems <-df[,bItemCol]
  if (!is.empty(bItems))
  {
    for (item in unique(bItems)) {
      df <-missingCheck(df, item, bItems,outCol1, outCol2)
    }
    if ( !is.empty(env$cdata) && "basicItem" %in% colnames(env$cdata))
    {
      x <- bItems %in%  env$cdata [, "basicItem"]
      if ( all(x)) {
        env$cdata <-df
      }
    }
  }
  df
  
}

missingCheck<- function (df, item, bItems, cl1, cl2) {
  index <- which(bItems==item)[[1]]
  item.vec <- env$medData[[item]]
  if(!is.empty(item.vec)){
    nq <- which(as.character(item.vec) =="" | is.na(item.vec))
    if (!is.empty (nq))
      for(i in nq)
      {
        msg <- paste("Fehlendes ", item)
        if (index >1) {
          if (!is.na(env$dq[,cl1][i])) env$dq[,cl1][i] <- paste(msg, "; ", env$dq[,cl1][i])
          else env$dq [,cl1] [i] <- msg
        }
        else env$dq [,cl1] [i] <- paste (msg, ".")
      }
    df <- addMissingValue(item, df,length(nq), length(item.vec))
  }
  else if (item!="Total"){
    df <- addMissingValue(item, df, 0,0)
    env$dq[,cl2]<- paste( item, " wurde nicht erhoben ", env$dq[,cl2])
  }
  
  df
}


addMissingValue<- function (item, bdata, m, n) {
  #index <- which(dqItem.vec==item)[[1]]
  index = which(bdata$basicItem==item)[1]
  if (is.null(index)) bdata$basicItem[1]=item
  if(!"missing_value_no" %in% colnames(bdata)) bdata$missing_value_no <-0
  if(!"missing_value_rate" %in% colnames(bdata)) bdata$missing_value_rate <-0
  if(!"N_Item" %in% colnames(bdata)) bdata$N_Item <-0
  if(n>0){
    bdata$N_Item[index] <-n
    if (!is.na(bdata$missing_value_no[index]) && is.numeric(bdata$missing_value_no[index]) ) bdata$missing_value_no[index] <- bdata$missing_value_no[index]+m
    else bdata$missing_value_no[index] <- m
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

getMissingItem<- function (basicItem) {
  diff <- setdiff (basicItem, names (env$medData))
  mItem <-""
  if (!is.empty (diff)){
    str<- paste (diff,collapse=" , " )
    mItem <- paste ("Folgende mandatorische Items fehlen: ", str) 
  }
  env$tdata$missing_item_no<- length(diff)
  env$tdata$missing_item_rate <- round(length(diff)/length(basicItem)*100 ,2)
  env$tdata
  mItem
}



