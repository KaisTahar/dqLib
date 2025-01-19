#######################################################################################################
# Description: The data quality library (dqLib) is an R package for traceable and explainable assessments of clinical data quality.
# As part of the "dqLib" package this script includes functions to detect data quality (DQ) issues that may arise in the context of common diseases like cardiovascular diseases (CVDs).
# Kais Tahar, University Medical Center Göttingen
# ######################################################################################################

#' @title cvdDqChecker
#' @description This function checks the quality of loaded data set regarding common DQ issues that may arise in the context of common diseases like cardiovascular diseases (CVDs).
#' @export
#'
cvdDqChecker <- function (rPath, ruleMeta)
{
  # Call global variables
  # Relevant meta data for CVDs
  numMeta=base::get("numMeta", envir=env)
  catMeta=base::get("catMeta", envir=env)
  tempMeta=base::get("tempMeta", envir=env)
  optMeta=base::get("optMeta", envir=env)
  metaCol=base::get("metaCol", envir=env)
  ovrQuality=base::get("ovrQuality", envir=env)
  # Labels for potential DQ issues
  im_misg_lbl=base::get("im_misg_lbl", envir=env)
  vm_misg_lbl=base::get("vm_misg_lbl", envir=env)
  vo_lbl=base::get("vo_lbl", envir=env)
  vc_lbl=base::get("vc_lbl", envir=env)
  # Set specific ruleMeta for CVDs
  base::assign(x = "ruleMeta", value =ruleMeta, envir=env)
  # D1-Completeness
  # Get user-defined missing rules from spreadsheet
  misgRules <- getMissingRules(rPath, ruleMeta[["missingRules"]])
  if (!(is.empty(numMeta) | is.empty(catMeta) | is.empty(tempMeta))) {
    im=getMandatoryItems(metaCol, optMeta, numMeta, catMeta, tempMeta)
    # Add results of the completeness analysis for all data items in the metadata for CVDs
    base::assign(x ="mItem" , value=getMissingItem(im),envir=env)
    base::assign(x="numMeta", value= getMissingValue(numMeta, metaCol, vm_misg_lbl, im_misg_lbl, misgRules), envir=env)
    base::assign(x="catMeta", value = getMissingValue(catMeta, metaCol, vm_misg_lbl, im_misg_lbl, misgRules), envir=env)
    base::assign(x="tempMeta", value = getMissingValue(tempMeta, metaCol, vm_misg_lbl, im_misg_lbl, misgRules), envir=env)
  } else stop(" The global Environment (env) does not contain any numerical or categorical or temporal data items.
  Please ensure the data type of mandatory data items is set correctly and then rerun the execution (For more details see global variables catMeta and numMeta).")
  # D2-Plausibility
  # Get user-defined DQ rules from spreadsheets
  rangeRules <- getRangeRules(rPath, ruleMeta[["rangeRules"]])
  mathRules <- getContradictionRules(rPath, ruleMeta[["mathRules"]])
  logicRules <- getContradictionRules(rPath, ruleMeta[["logicRules"]])
  #base::assign(x="dqRules", value=list (misgRules,rangeRules, logicRules), envir=env)
  base::assign(x="dqRules", value=list ("RangeRules"=rangeRules, "LogicalRules"= logicRules, "MathRules"=mathRules, "MissingRules"=misgRules), envir=env)
  # Add results of the plausibility analysis for all data items in the metadata for CVDs
  base::assign(x="numMeta", value = checkRangeRules(env$numMeta, rangeRules, metaCol, vo_lbl), envir=env)
  base::assign(x="contra", value = checkContradictionRules(env$contra, mathRules, logicRules, vc_lbl), envir=env)
  param <- getTotalStatistic(metaCol, ovrQuality)
  param
}

#' @title getMissingRules
#' @description Functions to import missing data rules from spreadsheets.
#' @export
#'
getMissingRules<-function (path, sheetName) {
  ruleMeta <-base::get("ruleMeta", envir=env)
  rules <- read.xlsx(xlsxFile = path, sheet = sheetName, skipEmptyRows = FALSE)
  sapply(rules, class)
  rID=ruleMeta[["ruleID"]]
  vItem1=ruleMeta[["valueItem1"]]
  vItem2=ruleMeta[["valueItem2"]]
  ruleIDs<-na.omit(rules[[rID]])
  rdata <- data.frame(ruleID =character(), item1=character(), valueItem1=character(), item2=character(), valueItem2=character())
  if (!is.empty(ruleIDs)) {
    for (ID in ruleIDs) {
      index = which(ruleIDs==ID)
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
        df <- missingCheck(df, mRules$item1[index], metaCol, vm_misg_lbl, im_misg_lbl, mRules$valueItem1[index], mRules$item2[index], mRules$valueItem2[index], mRules$ruleID[index])
      }
    }
  }
  df
}

#' @title getRangRules
#' @description Functions to extract range rules from spreadsheets.
#' @export
#'
getRangeRules<-function (path, sheetName) {
  ruleMeta <-base::get("ruleMeta", envir=env)
  rules <- read.xlsx(xlsxFile = path, sheet = sheetName, skipEmptyRows = FALSE)
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

#' @title getContradictionRules
#' @description Functions to extract contradiction rules from spreadsheets.
#' @export
#'
getContradictionRules<-function (path, sheetName) {
  ruleMeta <-base::get("ruleMeta", envir=env)
  mathRule =ruleMeta[["mathRules"]]
  logicRule =ruleMeta[["logicRules"]]
  rID=ruleMeta[["ruleID"]]
  item1 =ruleMeta[["item1"]]
  item2 =ruleMeta[["item2"]]
  operator =ruleMeta[["mathOpr"]]
  unit =ruleMeta[["unit"]]
  minRslt=ruleMeta[["minRslt"]]
  maxRslt=ruleMeta[["maxRslt"]]
  vItem1=ruleMeta[["valueItem1"]]
  vItem2=ruleMeta[["valueItem2"]]
  vItem3=ruleMeta[["valueItem3"]]
  rules <- read.xlsx(xlsxFile = path, sheet = sheetName, skipEmptyRows = FALSE)
  sapply(rules, class)
  ruleIDs<-na.omit(rules[[rID]])
  if (sheetName==mathRule) {
    rdata <- data.frame(ruleID =character(), item1=character(), operator=character(), item1=character(), item2=character(), unit=character(),min=double(), max=double())
    if (!is.empty(ruleIDs)) {
      for (ID in ruleIDs) {
        index = which(ruleIDs==ID)
        type = "mathRule"
        nItem1 <- as.character(rules[[item1]][index])
        nItem2 <- as.character(rules[[item2]][index])
        opr<- as.character(rules[[operator]][index])
        vUnit <- as.character(rules[[unit]][index])
        min <- as.double(rules[[minRslt]][index])
        max <- as.double(rules[[maxRslt]][index])
        rule = list(ruleID = ID, type=type, item1 =nItem1, operator = opr, item2 =nItem2, unit=vUnit, min=min, max=max)
        rdata <- rbind(rdata,rule, stringsAsFactors=FALSE)
      }
    }
  } else if (sheetName==logicRule) {
    rdata <- data.frame(ruleID =character(), tpye=character(), item1=character(), value1=character(),item2=character(),value2=character(),item3=character(),value3=character() )
    if (!is.empty(ruleIDs)) {
      for (ID in ruleIDs) {
        index = which(ruleIDs==ID)
        type = "logicalRule"
        itemDef1 <- as.character(rules[[vItem1]][index])
        split1 =  unlist (strsplit( itemDef1, split ="="))
        item1 = gsub(" ", "", split1[1])
        value1 = gsub(" ", "", split1[2])
        itemDef2 <- as.character(rules[[vItem2]][index])
        split2 =  unlist (strsplit( itemDef2, split ="="))
        item2 = gsub(" ", "", split2[1])
        value2 = gsub(" ", "", split2[2])
        itemDef3 <- as.character(rules[[vItem3]][index])
        split3 =  unlist (strsplit( itemDef3, split ="="))
        item3 = gsub(" ", "", split3[1])
        value3 = gsub(" ", "", split3[2])
        rule = list(ruleID = ID, type=type, item1=item1, value1=value1, item2=item2, value2=value2, item3=item3, value3=value3)
        rdata <- rbind(rdata,rule, stringsAsFactors=FALSE)
      }
    }
  } else stop("No contradiction rules available. Please define the spreadsheets for the required rules")
  rdata
}

#' @title checkContradictionRules
#' @description This function checks the semantic plausibly of loaded data set using predefined DQ rules.
#' @export
#'
checkContradictionRules<-function (df, mathRules, logicRules, contraCol) {
  if(!contraCol %in% colnames(env$dq)) env$dq[,contraCol]<-""
  if (!is.empty(mathRules)){
    mRuleIDs<-mathRules$ruleID
    if (!is.empty(mRuleIDs)) {
      for (ID in mRuleIDs) {
        index = which(mathRules$ruleID==ID)
        df<-checkMathRule(mathRules$ruleID[index], df, contraCol, mathRules$item1[index], mathRules$operator[index], mathRules$item2[index], mathRules$min[index],mathRules$max[index], mathRules$unit[index])
      }
    }
  }
  if (!is.empty(logicRules)){
    lRuleIDs<-logicRules$ruleID
    if (!is.empty(lRuleIDs)) {
      for (ID in  lRuleIDs) {
        index = which(logicRules$ruleID==ID)
        optParam <-c( logicRules$item3[index], logicRules$value3[index])
        if (!any(is.empty(optParam)) & !any(is.na(optParam))) df<-checkLogicalRule(logicRules$ruleID[index], df, contraCol, logicRules$item1[index], logicRules$value1[index], logicRules$item2[index], logicRules$value2[index], logicRules$item3[index], logicRules$value3[index])
        else df<-checkLogicalRule(logicRules$ruleID[index], df, contraCol, logicRules$item1[index], logicRules$value1[index], logicRules$item2[index], logicRules$value2[index])
      }
    }
  } else if (is.empty(mathRules)& is.empty(logicRules)) stop("No contradiction rules available. Please define the spreadsheets for the required rules and rerun the script")
  df
}
