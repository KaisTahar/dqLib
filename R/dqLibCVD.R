
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
