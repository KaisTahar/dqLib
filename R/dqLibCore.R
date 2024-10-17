#######################################################################################################
# Description: The data quality library (dqLib) is an R package for data quality (DQ) assessment and reporting.
# This script is part of the "dqLib" package and provides core functions for data quality assessment.
# Date Created: 2021-02-26
# Author: Kais Tahar, University Medical Center Göttingen
#######################################################################################################

#------------------------------------------------------------------------------------------------------
# functions to set package environment
#------------------------------------------------------------------------------------------------------

#' @title env
#' @description Package environment
#'
env <- new.env()

#' @title  setGlobals
#' @description Function to define global variables
#' @export
#'
setGlobals <- function(medData, repCol, cdata, ddata, tdata) {
  env$medData <- medData
  env$cdata <- cdata
  env$ddata <- ddata
  env$tdata <- tdata
  env$repMeta <-repCol
}

#' @title  setMissingCodes
#' @description Function to define missing values
#' @export
#'
setMissingCodes <- function(codeList) {
  env$missingCode <-codeList
}

#------------------------------------------------------------------------------------------------------
# functions to calculate DQ metrics for D1 completeness dimension
#------------------------------------------------------------------------------------------------------
#' @title itemCompletenessIndicator
#' @description This function adds generic DQ metrics to evaluate the completeness of mandatory data items
#'
itemCompletenessIndicator <- function(im, im_misg) {
  df <-data.frame( im = c(im), im_misg = c(im_misg))
  if (im>0) df$dqi_co_icr<- round (((im-im_misg)/im )*100,2)
  else df$dqi_co_icr<- NA
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  df
}

#' @title valueCompletenessIndicator
#' @description  This function adds generic DQ metrics to evaluate the completeness of mandatory data values
#'
valueCompletenessIndicator<- function(vm, vm_misg) {
  df <-data.frame( vm = c (vm), vm_misg = c(vm_misg))
  if (vm>0) df$dqi_co_vcr<- round (((vm-vm_misg)/vm )*100,2)
  else df$dqi_co_vcr<-NA
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  df
}

#' @title subjectCompletenessIndicator
#' @description  This function adds generic DQ metrics to evaluate the completeness of recorded subjects such as inpatient or outpatients
#'
subjectCompletenessIndicator <- function(s, s_inc) {
  df <-data.frame( s = c (s), s_inc= c(s_inc))
  if (s >0) df$dqi_co_scr<- round (((s-s_inc)/s )*100,2)
  else df$dqi_co_scr<-NA
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  df
  #env$report
}

#' @title caseCompletenessIndicator
#' @description  This function adds generic DQ metrics to evaluate the completeness of recorded case module
#'
caseCompletenessIndicator <- function(vm_case, vm_case_misg) {
  df <-data.frame(vm_case = c (vm_case), vm_case_misg= c(vm_case_misg))
  if (vm_case >0) df$dqi_co_ccr<- round (((vm_case-vm_case_misg)/vm_case )*100,2)
  else df$dqi_co_ccr<- NA
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  df
}

#' @title addMissingValueCount
#' @description This function adds the overall missing values
#'
addMissingValueCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$vm[index]<-sum(as.integer(as.character(bdata$vm[-index])), na.rm=TRUE)
  bdata$vm_misg[index] <- sum(as.integer(as.character(bdata$vm_misg[-index])), na.rm=TRUE)
  bdata
}
#' @title addMissingItemCount
#' @description This function adds the overall missing items
#'
addMissingItemCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$im_misg[index] <- sum(as.integer(as.character(bdata$im_misg[-index])), na.rm=TRUE)
  bdata
}

#' @title addCompleteness
#' @description This function to calculate the value completeness rate
#' @deprecated replaced by addValueCompleteness()
#'
addCompleteness<- function (tdata, col, row) {
  index = which( tdata[,col]==row)
  tdata$completness_rate[index]<-round (100-tdata$missing_value_rate[index],2)
  return <- tdata
}

#' @title getMissingValue
#' @description This function checks the loaded data for missing values
#'
getMissingValue<-function (df, bItemCol, outCol1,outCol2){
  if (!all(is.na(env$dq)))
  {
    if(!outCol1 %in% colnames(env$dq)) env$dq[,outCol1]<-""
    if(!outCol2 %in% colnames(env$dq)) env$dq[,outCol2] <-""
  }
  else
  {
    env$dq[nrow(env$dq)+1,] <- NA
    if(!outCol1 %in% colnames(env$dq)) env$dq[,outCol1]<-""
    if(!outCol2 %in% colnames(env$dq)) env$dq[,outCol2] <-""
  }
  bItems<-df[,bItemCol]
  bItems<-bItems[bItems!="Total"]
  if (!is.empty(bItems))
  {
    for (item in unique(bItems)) {
      df <-missingCheck(df, item, bItemCol, outCol1, outCol2)
    }
    if (!is.empty(env$cdata) && bItemCol %in% colnames(env$cdata))
    {
      x <- bItems %in%  env$cdata [,bItemCol]
      if ( all(x)) {
        env$cdata <-df
      }
    }
  }
  df
}

#' @title missingCheck
#' @description Function to check individual data items for missing values. The new version also supports coded missing values and data blanking rules. Data values hidden due to data blanking rules are not considered missing values
#' @export
#'
missingCheck<- function (df, item, itemCol, cl1, cl2, ...){
  vars <- list(...)
  if(length(vars)==2) {
    itemCond <- vars[[1]]
    cond <- vars[[2]]
    if (!all(is.na(env$dq))){
      if(!cl1 %in% colnames(env$dq)) env$dq[,cl1]<-""
      if(!cl2 %in% colnames(env$dq)) env$dq[,cl2] <-""
    } else {
      env$dq[nrow(env$dq)+1,] <- NA
      if(!cl1 %in% colnames(env$dq)) env$dq[,cl1]<-""
      if(!cl2 %in% colnames(env$dq)) env$dq[,cl2] <-""
    }
    dqItem.vec<- df[, itemCol]
    index <- which (df[, itemCol]==item)
    item.vec <- env$medData[[item]] 
    if(!is.empty(item.vec)){
      if(!is.empty(env$medData[[itemCond]])) {
        a <- which(env$medData[[itemCond]]!=cond)
        for (j in a) { 
          env$dq[,item][j] <- paste("hidden")
          env$medData[,item][j] <- paste("hidden")
        }
      }
      df$vm_misg[index] <-0
      m <- NULL
      if(!is.empty(env$medData[[itemCond]])) cond.vec <- env$medData[which(env$medData[[itemCond]]==cond), item]
      if(!is.empty(cond.vec)){
        if (!is.empty(env$missingCode)) {   
          for (code in env$missingCode) {
            if (is.na(code)) v <-  which(env$medData[[itemCond]]==cond & is.na(env$medData[[item]]))
            else if(is.null(code)) v <-  which(env$medData[[itemCond]]==cond & is.null(env$medData[[item]]))
            else  v <-  which(env$medData[[itemCond]]==cond & as.character(env$medData[[item]])==code)
            m <-c(m, v)
          }
        }
        else m <- which((env$medData[[itemCond]]==cond & as.character(env$medData[[item]])=="") |(env$medData[[itemCond]]==cond & is.na(env$medData[[item]])))
        if (!is.empty (m)) {
          for(i in m) {
            msg <- paste("Missing ", item, sep="")
            if (index >=1) { 
              if (!is.na(env$dq[,cl1][i])) env$dq[,cl1][i] <- paste(msg, ". ", env$dq[,cl1][i], sep="")
              else env$dq[,cl1][i] <- msg
            }
            else env$dq [,cl1][i] <- paste(msg, ".", sep="")
          }
          df <- addMissingValue(item,df,length(m),length(cond.vec),itemCol)
        }else df <- addMissingValue(item, df,0,length(cond.vec), itemCol)
      }else df <-addMissingValue(item, df,0, length(item.vec), itemCol)
    }
    else if (item!="Total"){
      df <- addMissingValue(item, df, 0,0, itemCol)
      env$dq[,cl2]<- paste( item," was not collected ", env$dq[,cl2])
    }
  } else {
      if (!all(is.na(env$dq))) {
        if(!cl1 %in% colnames(env$dq)) env$dq[,cl1]<-""
        if(!cl2 %in% colnames(env$dq)) env$dq[,cl2] <-""
      } else {
        env$dq[nrow(env$dq)+1,] <- NA
        if(!cl1 %in% colnames(env$dq)) env$dq[,cl1]<-""
        if(!cl2 %in% colnames(env$dq)) env$dq[,cl2] <-""
      }
      index <- which (df[, itemCol]==item)
      m <- NULL
      item.vec <- env$medData[[item]]
      if(!is.empty(item.vec)){
        if (!is.empty(env$missingCode)) {
          for (code in env$missingCode) {
            if (is.na(code)) v <-which(is.na(item.vec)) else if(is.null(code)) v <-which(is.null(item.vec))
            else  v <-which(as.character(item.vec) == code)
            m <-c(m, v)
          }
        }
        else m <- which(as.character(item.vec) =="" | is.na(item.vec))
        if (!is.empty (m)) {
          for(i in m)
          {
            msg <- paste("Missing ", item, sep="")
            if (index >=1) {
              if (!is.na(env$dq[,cl1][i])) env$dq[,cl1][i] <- paste(msg, ". ", env$dq[,cl1][i], sep="")
              else env$dq[,cl1][i] <- msg
            }
            else env$dq [,cl1][i] <- paste(msg, ".", sep="")
          }
          df <- addMissingValue(item, df,length(m), length(item.vec), itemCol)
        }
        else df <- addMissingValue(item, df,0, length(item.vec), itemCol)
      }
      else if (item!="Total"){
        df <- addMissingValue(item, df, 0,0, itemCol)
        env$dq[,cl2]<- paste( item, " was not collected ", env$dq[,cl2])
      }
  }
  
  df
}

#' @title checkSubjCompleteness
#' @description This function evaluates the completeness of recorded subjects such as inpatient or outpatients
#' @export
#'
checkSubjCompleteness <-function(subj, itemVec){
  df <-data.frame(s = 0, s_inc= 0)
  if (all(itemVec %in%  colnames(env$medData)==TRUE))
  {
    basicData <-subset(env$medData, select= itemVec)
    df$s <-length(unique(basicData[[subj]]))
    completeData <- na.omit(basicData)
    complete_subj_no_py <-length(unique(completeData[[subj]]))
    df$s_inc <-df$s-complete_subj_no_py
  } else {
    df$s <-length(unique(basicData[[subj]]))
    df$s_inc <-df$s
  }
  if (is.null(env$metrics)) env$metrics <-df
  else env$metrics <-cbind(env$metrics,df)
  df
}

#' @title checkCaseCompleteness
#' @description This function evaluates the completeness of case module
#'
checkCaseCompleteness<-function (caseItems, bItemCol){
  df <-data.frame(vm_case =0, vm_case_misg= 0)
  for (item in caseItems) {
    index1 = which(cdata[, bItemCol]==item)
    if (length(index1) >0) {
      if (env$cdata$vm[index1]==0) {
        df$vm_case_misg <- df$vm_case_misg+nrow(env$medData)
        df$vm_case <- df$vm_case+nrow(env$medData)
      }
      df$vm_case <- df$vm_case+env$cdata$vm[index1]
      df$vm_case_misg <- df$vm_case_misg+env$cdata$vm_misg[index1]
    }
    else{
      index2 = which(ddata[, bItemCol]==item)
      if (!is.null(index2) & !is.na(index2)){
        if (env$ddata$vm[index2]==0) {
          df$vm_case_misg <- df$vm_case_misg+nrow(env$medData)
          df$vm_case <- df$vm_case+nrow(env$medData)
        }
        df$vm_case <- df$vm_case+env$ddata$vm[index2]
        df$vm_case_misg <- df$vm_case_misg+env$ddata$vm_misg[index2]
      }
    }
  }
  if (is.null(env$metrics)) env$metrics <-df
  else env$metrics <-cbind(env$metrics,df)
  df
}

#' @title addMissingValue
#' @description Function to add missing value metrics for each data item
#' @export
#'
addMissingValue<- function (item, bdata, m, n, ...) {
  vars <- list(...)
  bItemCl = "basicItem"
  if(!(is.empty(vars))){
    bItemCl <- vars[[1]]
  }
  index = which(bdata[, bItemCl]==item)
  if ( length(index)>0){
    if(!"im_misg" %in% colnames(bdata)) bdata$im_misg <-0
    if(!"vm_misg" %in% colnames(bdata)) bdata$vm_misg <-0
    if(!"vm" %in% colnames(bdata)) bdata$vm <-0
    if(n>0){
      bdata$vm[index] <-n
      if (!is.na(bdata$vm_misg[index]) && is.numeric(bdata$vm_misg[index]) ) bdata$vm_misg[index] <- bdata$vm_misg[index]+m
      else bdata$vm_misg[index] <- m
    }
    else{
      bdata$im_misg[index] <- 1
      bdata$vm[index] <- 0
      bdata$vm_misg[index] <- 0
    }
    bdata
  }else bdata
}

#' @title addMissingCount
#' @description This function adds the overall metrics for missing values
#' @deprecated replaced by addMissingItemCount
#'
addMissingCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$N_Item[index]<-sum(bdata$N_Item[-index], na.rm=TRUE)
  bdata$missing_value_no[index] <- sum(as.integer(as.character(bdata$missing_value_no[-index])), na.rm=TRUE)
  mr <- (bdata$missing_value_no[index]/ bdata$N[index])* 100
  bdata$missing_value_rate[index] <- round (mr,2)
  bdata
}

#' @title getMissingItem
#' @description Function to check the loaded data for missing of mandatory data items
#' @export
#'
getMissingItem<- function (basicItem) {
  df <-data.frame( im = 0, im_misg = 0, im_misg_msg =0)
  diff <- setdiff(basicItem, names(env$medData))
  mItem <-""
  if (!is.empty (diff)){
    str<- paste (diff,collapse=" , " )
    mItem <- paste ("Following data items are missing: ", str)
  }
  df$im <- length(basicItem)
  df$im_misg<- length(diff)
  df$im_misg_msg<- mItem
  if (is.null(env$metrics)) env$metrics <-df
  else env$metrics <-cbind(env$metrics,df)
  
  mItem
}

#' @title is.empty
#' @description This function checks whether a vector (data item) is empty
#' @export
#'
is.empty <- function(x) return(length(x) ==0 )


#------------------------------------------------------------------------------------------------------
# functions to calculate DQ metrics for D2 plausibility dimension
#------------------------------------------------------------------------------------------------------

#' @title addOutlier
#' @description Function to add detected outliers for each data item
#' @export
#'
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

#' @title addOutlierCount
#' @description Funtion to count detected outliers and checked data
#'
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

# This Function checks the data item "birthdate" for implausible values
getAgeMaxOutlier<- function ( dItem1.vec, dItem2.vec, n){
  diff <-  ifelse ((isDate(dItem1.vec) & isDate(dItem2.vec)), as.numeric(difftime(dItem1.vec, dItem2.vec),units="weeks")/52.25 , 0 )
  out <- which(abs(diff)>n)
  out
}

# This function checks the loaded temporal data for outliers
getDateOutlier<- function (dItem.vec){
  now<- as.Date(Sys.Date())
  out <-  vector()
  out <- which(isDate(dItem.vec) & (as.Date(dItem.vec)>now))
  out
}



#------------------------------------------------------------------------------------------------------
# Utility functions
#------------------------------------------------------------------------------------------------------

#' @title isDate
#' @description This function checks whether a given data value has date format
#' @export
#'
isDate <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate,tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y","%Y.%m.%d","%d.%m.%Y","%m.%d.%Y"))),
           error = function(err) {FALSE})
}

#' @title getUserSelectedMetrics
#' @description This function enable users to select desired DQ metrics
#'
getUserSelectedMetrics <- function(dqInd, tdata){
  dqMetrics <- subset(tdata, select= dqInd)
  dqMetrics
}

#' @title getFileExtension
#' @description Function to get the file extension of a given file
#'
getFileExtension <- function(filePath){
  ext <- strsplit(basename(filePath), split="\\.")[[1]]
  return(ext[-1])
}

#' @title getPercentFormat
#' @description This function formats values as a percentage
#'
getPercentFormat <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}


#' @title addStatistic
#' @description Funtion to calculate the overall DQ metrics for the completeness and plausibility dimensions
#'
addStatistic<- function (bdata, col, row) {
  bdata = addMissingCount(bdata,col,row)
  bdata = addOutlierCount(bdata,col,row)
  bdata
}

#' @title getTotalStatistic
#' @description Function to calculate the overall DQ metrics
#' @export
#'
getTotalStatistic <- function(col, row){
  env$cdata<- addStatistic(env$cdata, col, row)
  if (is.null(env$ddata)) bdata <-env$cdata
  else bdata <- base::merge(env$cdata,  addStatistic(env$ddata, col, row) , by=intersect(names(env$cdata), names(env$ddata)), all = TRUE)
  if (!is.empty (bdata$engLabel)) bdata$engLabel <-NULL
  index = which(bdata[,col]==row)
  bdata<-bdata[-index,]
  bdata[nrow(bdata) + 1, ] <- list ("Total",0,0,0,0,0, 0, nrow(bdata)-1)
  bdata<- addStatistic(bdata, col, row)
  tcdata <- addCompleteness (bdata, col, row)
  total <- subset(tcdata, tcdata[,col]==row)
  env$tdata<- cbind(total,env$tdata)
  env$tdata
}

#' @title deprecatedMetrics
#' @description Function to handle deprecated DQ metrics that were renamed according to the abbreviations specified in the publication DOI:10.1055/a-2006-1018.
#' The aim of this function is to support legacy versions and migrate from deprecated code without introducing bugs.
#'
deprecatedMetrics<- function (df) {
  if (! is.empty(df))
  {
    if ("vm_misg" %in% names(df))  df$missing_value_no_py <- df$vm_misg
    if ("im_misg" %in% names(df))  df$missing_item_no_py <- df$im_misg
    if ("s_inc" %in% names (df))    df$incomplete_subject_no_py <- df$s_inc
    if ("dqi_co_icr" %in% names(df))  df$item_completeness_rate <- df$dqi_co_icr
    if ("dqi_co_vcr" %in% names(df))  df$value_completeness_rate <- df$dqi_co_vcr
    if ("dqi_co_scr" %in% names(df))  df$subject_completeness_rate <-df$dqi_co_scr
    if ("dqi_co_ccr" %in% names(df))  df$case_completeness_rate <-df$dqi_co_ccr
  }
  
  df
}