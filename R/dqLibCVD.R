#######################################################################################################
# Description: The data quality library (dqLib) is an R package for data quality (DQ) assessment and reporting.
# As part of the "dqLib" package this script includes functions to detect DQ issues that may arise in the context of common diseases like cardiovascular diseases (CVDs).
# Kais Tahar, University Medical Center Göttingen
# ######################################################################################################

#' @title cvdDqChecker
#' @description This function checks the quality of loaded data set regarding common DQ issues that may arise in the context of common diseases like cardiovascular diseases (CVDs).
#' @export
#'
cvdDqChecker <- function (rPath, spstMeta)
{
  # call constant values: Labels for potential DQ issues
  im_misg_lbl=base::get("im_misg_lbl", envir=env)
  vm_misg_lbl=base::get("vm_misg_lbl", envir=env)
  vo_lbl=base::get("vo_lbl", envir=env)
  vc_lbl=base::get("vc_lbl", envir=env)
  # call relevant meta data for CVDs
  numMeta=base::get("numMeta", envir=env)
  catMeta=base::get("catMeta", envir=env)
  tempMeta=base::get("tempMeta", envir=env)
  optMeta=base::get("optMeta", envir=env)
  metaCol=base::get("metaCol", envir=env)
  ovrQuality=base::get("ovrQuality", envir=env)
  # set specific ruleMeta for CVDs
  base::assign(x = "ruleMeta", value =spstMeta, envir = env)
  # D1-Completeness
  if (!(is.empty(numMeta) | is.empty(catMeta) | is.empty(tempMeta))) {
      im=getMandatoryItems(metaCol, optMeta, numMeta, catMeta, tempMeta)
      # add results of the completeness analysis for all data items in the meta data for CVDs
      base::assign(x ="mItem" , value=getMissingItem(im),envir=env)
      mRules <- getMissingRules(rPath)
      base::assign(x="numMeta", value= getMissingValue(numMeta, metaCol, vm_misg_lbl, im_misg_lbl, mRules), envir=env)
      base::assign(x="catMeta", value = getMissingValue(catMeta, metaCol, vm_misg_lbl, im_misg_lbl, mRules), envir=env)
      base::assign(x="tempMeta", value = getMissingValue(tempMeta, metaCol, vm_misg_lbl, im_misg_lbl, mRules), envir=env)
  } else stop(" The global Environment (env) does not contain any numerical or categorical or temporal data items.
  Please ensure the data type of mandatory data items is set correctly and then rerun the execution (For more details see global variables catMeta and numMeta).")
  # D2-Plausibility
  # get user-defined DQ rules form spreadsheets
  rRules <- getRangeRules(rPath)
  # add results of the plausibility analysis for all data items in the meta data for CVDs
  base::assign(x="numMeta", value = checkRangeRules(env$numMeta, rRules, metaCol, vo_lbl), envir=env)
  param <- getTotalStatistic(metaCol, ovrQuality)
  param
}

#' @title getMissingRules
#' @description Functions to import missing data rules from spreadsheets.
#' @export
#'
getMissingRules<-function (path) {
  ruleMeta <-base::get("ruleMeta", envir=env)
  misgRule=ruleMeta[["misgRule"]]
  rules <- read.xlsx(xlsxFile = path, sheet = misgRule, skipEmptyRows = FALSE)
  sapply(rules, class)
  rID=ruleMeta[["ruleID"]]
  vItem1=ruleMeta[["valueItem1"]]
  vItem2=ruleMeta[["valueItem2"]]
  ruleIDs<-na.omit(rules[[rID]])
  rdata <- data.frame(ruleID =character(), item1=character(), valueItem1=character(), item2=character(), valueItem2=character())
  if (!is.empty(ruleIDs)) {
    for (ID in ruleIDs) {
      index = which(ruleIDs==ID)
      #type = misgRule
      equation1 <- as.character(rules[[vItem1]][index])
      equation2 <- as.character(rules[[vItem2]][index])
      split1 =  unlist (strsplit(equation1, split ="="))
      item1 = split1[1]
      value1 = split1[2]
      split2 =  unlist(strsplit(equation2, split ="="))
      item2 = split2[1]
      value2 = split2[2]
      rule = list(ruleID =ID, item1=item1, valueItem1=value1, item2=item2, valueItem2=value2)
      rdata <- rbind(rdata, rule, stringsAsFactors=FALSE)
    }
  } else stop("No missing rules available. Please define the spreadsheet missingRules and set the required rules")
  rdata
}

#' @title checkMissingRules
#' @description Functions to detect missing data values using DQ rules
#' @export
#'
checkMissingRules<-function (df, mRules, metaCol, vm_misg_lbl, im_misg_lbl) {
  mRuleIDs<-mRules$ruleID
  if (!is.empty(mRuleIDs)) {
    for (i in  mRuleIDs) {
      index = which(mRules$ruleID==i)
      x <- mRules$item1[index] %in% df [,metaCol]
      if ( all(x)) {
        df <- missingCheck(df, mRules$item1[index], metaCol, vm_misg_lbl, im_misg_lbl, mRules$valueItem1[index], mRules$item2[index], mRules$valueItem2[index])
      }
    }
  }
  df
}

#' @title getRangRules
#' @description Functions to extract range rules from spreadsheets.
#' @export
#'
getRangeRules<-function (path) {
  ruleMeta <-base::get("ruleMeta", envir=env)
  rangeRule=ruleMeta[["rangeRule"]]
  rules <- read.xlsx(xlsxFile = path, sheet = rangeRule, skipEmptyRows = FALSE)
  sapply(rules, class)
  rID=ruleMeta[["ruleID"]]
  item1 =ruleMeta[["item1"]]
  maxItem1 =ruleMeta[["maxItem1"]]
  minItem1 =ruleMeta[["minItem1"]]
  vItem2=ruleMeta[["valueItem2"]]
  vItem3=ruleMeta[["valueItem3"]]
  ruleIDs<-na.omit(rules[[rID]])
  rdata <- data.frame(ruleID =character(), item1=character(),  minValue1=double(), maxValue1=double(), item2=character(), value2=character(), item3=character(), value3=character())
  if (!is.empty(ruleIDs)){
    for (ID in ruleIDs) {
      index = which(ruleIDs==ID)
      nItem1 <- as.character(rules[[item1]][index])
      minValue1 <- as.double(rules[[minItem1]][index])
      maxValue1 <- as.double(rules[[maxItem1]][index])
      equation1 <- as.character(rules[[vItem2]][index])
      equation2 <- as.character(rules[[vItem3]][index])
      if (!is.empty(equation1)){
        split1 =  unlist(strsplit( equation1 , split ="="))
        item2 = split1[1]
        value2 = split1[2]
      } else {
        item2 <-NA
        value2 <-NA
      }
      if (!is.empty(equation2)){
        split2 =  unlist(strsplit( equation2 , split ="="))
        item3 = split2[1]
        value3 = split2[2]
      } else {
        item3 <-NA
        value3 <-NA
      }
      rule = list(ruleID = ID, item1=nItem1,  minValue1=minValue1, maxValue1=maxValue1, item2 =item2, value2=value2, item3=item3, value3=value3)
      rdata <- rbind(rdata,rule, stringsAsFactors=FALSE)
    }
  } else stop("No range rules available. Please define the spreadsheet rangeRules and set the required rules")
  rdata
}

#' @title checkMissingRules
#' @description This function checks the range plausibly using predefined DQ rules.
#' @export
#'
checkRangeRules<-function (ndata, rRules, imCol, outlierCol) {
  if(!outlierCol %in% colnames(env$dq)) env$dq[,outlierCol]<-NA
  rRuleIDs<-rRules$ruleID
  if (!is.empty(rRuleIDs)) {
    for (ID in  rRuleIDs) {
      index = which(rRules$ruleID==ID)
      if ( is.na(rRules$value2[index])) ndata  <-checkRangeRule(ndata,rRules$ruleID[index], imCol, outlierCol, rRules$item1[index], rRules$minValue1[index], rRules$maxValue1[index])
      else ndata<-checkRangeRule(ndata, rRules$ruleID[index], imCol, outlierCol, rRules$item1[index], rRules$minValue1[index], rRules$maxValue1[index], rRules$item2[index], rRules$value2[index], rRules$item3[index], rRules$value3[index])
    }
  }
  ndata
}

