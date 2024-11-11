
#This function checks the quality of loaded data regarding common DQ issues that may arise in the context of cardiovascular diseases (CVDs).

cvdDqChecker <- function (bItemCl, rPath)
{
  # D1-Completeness
  missingCode =c("", NULL, NA)
  setMissingCodes(missingCode)
  mRules <- getMissingRules(rPath, "MissingRules")
  if (!is.empty(env$cdata) & !is.empty (env$ddata) & !is.empty (env$ndata)) {
    imList <- getMandatoryItems("basicItem","Total", env$cdata, env$ddata, env$ndata)
    env$mItem <- getMissingItem(imList)
    env$cdata <- getMissingValue(env$cdata, bItemCl, "missing_value",  "missing_item", mRules)
    env$ddata <- getMissingValue(env$ddata, bItemCl, "missing_value",  "missing_item", mRules)
    env$ndata <- getMissingValue(env$ndata, bItemCl,  "missing_value",  "missing_item", mRules)
  } else if (is.null (env$cdata) & is.null(env$ndata)) stop(" The global Environment (env) does not contain any numerical or categorical data items. 
  Please ensure the data type of mandatory data items is set correctly and then rerun the execution (For more details see global variables env$ndata and env$cdata).")
  # D2-Plausibility
  rRules <- getRangeRules(rPath, "RangeRules")
  outliers <- checkRangeRules(env$ndata, rRules, bItemCl, "outlier")$ndata
  param <- getTotalStatistic(bItemCl,  env$sumRow)
  param
}

#------------------------------------------------------------------------------------------------------
# Functions to detect value completeness issues using missing data rules
#------------------------------------------------------------------------------------------------------

getMissingRules<-function (path, sheetName) {
  rules <- read.xlsx(xlsxFile = path, sheet = sheetName, skipEmptyRows = FALSE)
  sapply(rules, class)
  ruleIDs<-na.omit(rules$RuleID)
  if (!is.empty(ruleIDs))
  {
    if (sheetName=="MissingRules") rdata <- data.frame(ruleID =character(), type =character(), item1=character(),  item2=character(), valueItem2=character())
    else Stop("No missing rules available. Please define the spreadsheet name MissingRules and set the required rules")
    for (ID in ruleIDs) {
        index = which(rules$RuleID==ID)
        type = "MissingRule"
        item1 <- as.character( rules$Item1[index])
        item2 <- as.character( rules$Item2[index])
        value2 <- as.character( rules$`value(Item2)`[index])
        rule = list(ruleID =ID, type=type, item1=item1, item2=item2, valueItem2=value2)
        rdata <- rbind(rdata, rule, stringsAsFactors=FALSE)
      }
  }
  rdata
}

checkMissingRules<-function (df, mRules, itemCol, vmCol, imCol) {
  if(!vmCol %in% colnames(env$dq)) env$dq[,vmCol]<-""
  if(!imCol %in% colnames(env$dq)) env$dq[,imCol]<-""
  mRuleIDs<-mRules$ruleID
  if (!is.empty(mRuleIDs)) {
    for (i in  mRuleIDs) {
      index = which(mRules$ruleID==i)
      if (mRules$type [index]=="MissingRule") {
        x <- mRules$item1[index] %in% df [,itemCol]
        if ( all(x)) {
          df <- missingCheck(df, mRules$item1[index], itemCol, vmCol, imCol, mRules$item2[index], mRules$valueItem2[index])
        }
      }
    }
  }
  df
}

#------------------------------------------------------------------------------------------------------
# functions to support the detection of outliers using range rules
#------------------------------------------------------------------------------------------------------

getRangeRules<-function (path, sheetName) {
  rules <- read.xlsx(xlsxFile = path, sheet = sheetName, skipEmptyRows = FALSE)
  sapply(rules, class)
  itemIDs<-rules$RuleID
  if (!is.empty(itemIDs)){
    rdata <- data.frame(ruleID =character(), type=character(), item1=character(),  minValue1=double(), maxValue1=double(), item2=character(), value2=character(), item3=character(), value3=character())
    for (i in itemIDs) {
      index = which(rules$RuleID==i)
      ruleType =  as.character(rules$Type[index])
      if (ruleType =="Simple Range Rule") {
        type ="simpleRangeRule"
        item1 <- as.character(rules$Item1[index])
        minValue1 <-  as.double(rules$`min(Item1)`[index])
        maxValue1 <-  as.double( rules$`max(Item1)`[index])
        rule = list(ruleID = i, type=type, item1=item1,  minValue1=minValue1, maxValue1=maxValue1, item2 =NA, value2=NA, item3="", value3=NA)
      } else if (ruleType == "Complex Range Rule") {
        type ="complexRangeRule"
        item1 <- as.character( rules$Item1[index])
        minValue1 <-  as.double(rules$`min(Item1)`[index])
        maxValue1 <-  as.double( rules$`max(Item1)`[index])
        item2 <- as.character( rules$Item2[index])
        item3 <- as.character( rules$Item3[index])
        split1 =  unlist (strsplit( item2, split ="="))
        item2 = split1[1]
        value2 = split1[2]
        split2 =  unlist (strsplit( item3, split ="="))
        item3 = split2[1]
        value3 = split2[2]
        rule = list(ruleID = i, type=type, item1=item1,  minValue1=minValue1, maxValue1=maxValue1, item2 =item2, value2=value2, item3=item3, value3=value3)
      }
      rdata <- rbind(rdata,rule, stringsAsFactors=FALSE)
    }
  }
  rdata
}

checkRangeRules<-function (ndata, rRules, itemCol, outlierCol) {
  if(!outlierCol %in% colnames(env$dq)) env$dq[,outlierCol]<-NA
  rRuleIDs<-rRules$ruleID
  if (!is.empty(rRuleIDs)) {
    for (i in  rRuleIDs) {
      index = which(rRules$ruleID==i)
      if (rRules$type [index]=="simpleRangeRule") ndata <-checkRangeRule(ndata, itemCol, outlierCol, rRules$item1[index], rRules$minValue1[index], rRules$maxValue1[index])
      else if (rRules$type [index]=="complexRangeRule") ndata <-checkRangeRule(ndata, itemCol, outlierCol, rRules$item1[index], rRules$minValue1[index], rRules$maxValue1[index], rRules$item2[index], rRules$value2[index], rRules$item3[index], rRules$value3[index])
    }
  }
  env$ndata <-ndata
  out <- list()
  out[["dq"]] <- env$dq
  out[["ndata"]] <- ndata
  out
}