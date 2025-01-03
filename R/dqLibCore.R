#######################################################################################################
# Description: The data quality library (dqLib) is an R package for data quality (DQ) assessment and reporting.
# This package provides multiple metrics to analyze different aspects of DQ. The developed functions enable users to select desired dimensions, indicators, and parameters as well as to generate specific DQ reports and visualizations.
# This script is part of the "dqLib" package and provides generic functions for data quality assessment.
# Date Created: 2021-02-26
# Author: Kais Tahar, University Medical Center Göttingen
#' @keywords internal
#' @name dqLib
"_PACKAGE"
#######################################################################################################

#------------------------------------------------------------------------------------------------------
# functions to set package environment
#------------------------------------------------------------------------------------------------------

#' @title env
#' @description Package environment
#'
env <- new.env()

#' @title setEnvironment
#' @description Function to set global variables.
#' @export
#'
setEnvironment <- function(data, numItems, catItems, tempItems, itemCol, optItems, ...) {
  vars <- list(...)
 if (!exists("env")) env <<- new.env()
 # if (!exists("env")) env <- new.env()
  # set study data
  env$studyData <- data
  if (is.null (env$studyData)) stop("No data available")
  env$dq <- data
  # set metadata
  env$metaCol<- itemCol
  env$numMeta <-numItems
  env$catMeta <-catItems
  env$tempMeta<-tempItems
  env$optMeta <- optItems
  env$im <- getMandatoryItems(itemCol, optItems, numItems, catItems, tempItems)
  if (is.null (env$numMeta)) warning("No numerical data items available")
  if (is.null (env$catMeta)) warning("No categorical data items available")
  if (is.null (env$tempMeta)) warning("No temporal data items available")
  if (is.null (env$numMeta) & is.null(env$catMeta) & is.null(env$tempMeta)) stop("The global Environment (env) does not contain any numerical or categorical or temporal data items.
  Please ensure the data type of mandatory data items is set correctly and then rerun the execution (For more details see global variables env$numMeta and env$catMeta).")
  # set code for missing data values
  if (!is.empty(vars)) env$vm_misg_code <-vars[[1]] else env$vm_misg_code <- c("", "NULL", NA)
  # set reporting labels for potential DQ issues
  env$im_misg_lbl <- "missing_items"
  env$vm_misg_lbl <- "missing_values"
  env$vo_lbl <-"outliers"
  env$vc_lbl <- "contradictions"
  # set default variables
  #env$ovrQuality <- "Overall DQ"
  env$ovrQuality <- "Total"
  env$ruleMeta <- NULL
  env$metrics <-NULL
  env$report <-NULL
  env$contra <-NULL
}

#' @title dqChecker
#' @description This function assesses the quality of loaded data using generic indicators and domain-specific DQ checks.
#' The DQ indicators were implemented based on the DQ concept published under the DOI:10.1055/a-2006-1018.
#' @export
#'
dqChecker <- function (studyData, coreDomain, numItems, catItems, tempItems, itemMandatory, itemOptional, missingCode, ...)
{
  vars <- list(...)
  # set global variables
  setEnvironment(studyData, numItems, catItems, tempItems, itemMandatory, itemOptional, missingCode)
  # initialize metrics
  metrics <- data.frame(indicators =NA, parameters =NA)
  indInstance <- data.frame(dqi_co_icr =NA, dqi_co_vcr =NA )
  #Domain-driven design of DQ checks
  if (coreDomain=="CVD") { # cardiovascular diseases (CVD)
    if (! is.empty (vars)) {
      # Semantic Rules for detecting missing data values and recognizing the intentionally hidden data values
      rulePath <- vars[[1]]
      ruleMeta  <- vars[[2]]
      paramInstance <- cvdDqChecker(rulePath, ruleMeta)
    }
  }
  if (coreDomain =="RD") { # rare diseases (RD)
    if(length(vars)==2){
      # tracer diagnoses list
      tracerRef <- vars[[1]]
      # standard terminology for semantic annotation of RDs
      rdStandard <- vars[[2]]
      paramInstance  <-rdDqChecker(tracerRef, rdStandard)
    }
  }
  # DQ indicators
  indInstance$dqi_co_icr <- itemCompletenessIndicator(paramInstance$im, paramInstance$im_misg)$value
  indInstance$dqi_co_vcr <- valueCompletenessIndicator(paramInstance$vm, paramInstance$vm_misg)$value
  indInstance$dqi_pl_rpr <- rangePlausibilityIndicator(paramInstance$vs_od, paramInstance$vo)$value
  indInstance$dqi_pl_spr <- semanticPlausibilityIndicator(paramInstance$vs_cd, paramInstance$vc)$value
  # results of DQ analysis (parameters and indicators)
  metrics$parameters <- paramInstance
  metrics$indicators <- indInstance
  assign(x="metrics", value=metrics, envir = env)
  metrics
}

#------------------------------------------------------------------------------------------------------
# functions to specify and calculate data quality (DQ) metrics for the completeness dimension (D1)
#------------------------------------------------------------------------------------------------------

#' @title itemCompletenessIndicator
#' @description This function calculates the Item Completeness Rate (dqi_cc_icr), a generic indicator that assesses the completeness of mandatory data items, and adds related metadata and DQ parameters.
#' @export
#'
itemCompletenessIndicator <- function(im, im_misg) {
  ind <-data.frame(
    Abbreviation= "dqi_co_icr",
    Label = "Item Completeness Rate",
    Dimension ="Completeness",
    Short_Description = "This indicator assesses the metadata completeness. Further details and examples are available under DOI:10.1055/a-2006-1018."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  if (is.numeric(im) & is.numeric(im_misg)) {
    if (im>0) ind$value <-round(((im - im_misg)/im)*100,2)
    else ind$value <- NA
  } else {
    ind$value <- NA
    if (!is.numeric(im)) im <-NA
    if (!is.numeric(im_misg)) im_misg <-NA
  }
  df <-data.frame( im = c(im), im_misg = c(im_misg), dqi_co_icr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title valueCompletenessIndicator
#' @description  This function determines the Value Completeness Rate (dqi_cc_vcr), a generic indicator for assessing the completeness of mandatory data values. It also adds related metadata and parameters in the DQ report.
#' @export
#'
valueCompletenessIndicator<- function(vm, vm_misg) {
  ind <-data.frame(
    Abbreviation= "dqi_co_vcr",
    Label = "Value Completeness Rate",
    Dimension ="Completeness",
    Short_Description = "This indicator assesses the data completeness of a given data set. The publication DOI:10.1055/a-2006-1018 provides more details and examples."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(vm) & is.numeric(vm_misg)) {
    if (vm>0) ind$value <-round (((vm-vm_misg)/vm )*100,2)
    else ind$value <- NA
  } else {
    ind$value <- NA
    if (!is.numeric(vm)) vm <-NA
    if (!is.numeric(vm_misg)) vm_misg <-NA
  }
  df <- data.frame( vm = c(vm), vm_misg= c(vm_misg), dqi_co_vcr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title subjectCompletenessIndicator
#' @description  Function to calculate the indicator for Subject Completeness (dqi_cc_scr), and adds related metadata and parameters in the DQ report.
#' @export
#'
subjectCompletenessIndicator <- function(s, s_inc) {
  ind <-data.frame(
    Abbreviation= "dqi_co_scr",
    Label = "Subject Completeness Rate",
    Dimension ="Completeness",
    Short_Description = "This indicator assesses the completeness of subject records. Further details and examples can be found under DOI:10.1055/a-2006-1018."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(s) & is.numeric(s_inc)) {
    if (s>0) ind$value <- round (((s-s_inc)/s )*100,2)
    else ind$value <- NA
  } else {
    ind$value <- NA
    if (!is.numeric(s)) s <-NA
    if (!is.numeric(s_inc)) s_inc <-NA
  }
  df <-data.frame( s = c (s), s_inc= c(s_inc), dqi_co_scr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title caseCompletenessIndicator
#' @description This function calculate the indicator for Case Completeness (dqi_cc_ccr), and adds related metadata and parameters.
#' @export
#'
caseCompletenessIndicator <- function(vm_case, vm_case_misg) {
  ind <-data.frame(
    Abbreviation= "dqi_co_ccr",
    Label = "Case Completeness Rate",
    Dimension ="Completeness",
    Short_Description = "This indicator assesses the completeness of data values required for recording the case module in a given data set. Further details and examples are available under DOI:10.1055/a-2006-1018."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(vm_case) & is.numeric(vm_case_misg)) {
    if (vm_case>0) ind$value <-  round (((vm_case-vm_case_misg)/vm_case )*100,2)
    else ind$value <- NA
  } else {
    ind$value <- NA
    if (!is.numeric(vm_case)) vm_case <-NA
    if (!is.numeric(vm_case_misg)) vm_case_misg <-NA
  }
  df <-data.frame(vm_case = c (vm_case), vm_case_misg= c(vm_case_misg), dqi_co_ccr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title addMissingValue
#' @description Function to add missing value metrics for each data item.
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
      bdata$im_misg[index] <- 0
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

#' @title addMissingValueCount
#' @description Function to count the overall missing data values.
#'
addMissingValueCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$vm[index]<-sum(as.integer(as.character(bdata$vm[-index])), na.rm=TRUE)
  bdata$vm_misg[index] <- sum(as.integer(as.character(bdata$vm_misg[-index])), na.rm=TRUE)
  bdata
}
#' @title addMissingItemCount
#' @description Function to count the overall missing data items.
#'
addMissingItemCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$im_misg[index] <- sum(as.integer(as.character(bdata$im_misg[-index])), na.rm=TRUE)
  bdata
}

#------------------------------------------------------------------------------------------------------
# functions to detect completeness issues
#------------------------------------------------------------------------------------------------------

#' @title getMissingValue
#' @description This function checks the loaded data for missing data values. It also supports missing rules.
#' @export
#'
getMissingValue<-function (df, bItemCol, outCol1, outCol2, ...){
  vars <- list(...)
  exp =NULL
  if (!all(is.na(env$dq)))
  {
    if(!outCol1 %in% colnames(env$dq)) env$dq[,outCol1]<-""
    if(!outCol2 %in% colnames(env$dq)) env$dq[,outCol2] <-""
  }
  else {
    env$dq[nrow(env$dq)+1,] <- NA
    if(!outCol1 %in% colnames(env$dq)) env$dq[,outCol1]<-""
    if(!outCol2 %in% colnames(env$dq)) env$dq[,outCol2] <-""
  }
  bItems<-df[,bItemCol]
  bItems<-bItems[bItems!=env$ovrQuality]
  if (!is.empty(bItems)) {
    if (!is.empty(vars)) {
      mRules <- vars[[1]]
      exp <- mRules$item1
      df <- checkMissingRules(df, mRules, bItemCol, outCol1, outCol2)
    }
    for (item in unique(bItems)) {
      if (!is.null(exp)){
        if (!item %in% exp) df <- missingCheck(df, item, bItemCol, outCol1, outCol2)
      } else df <-missingCheck(df, item, bItemCol, outCol1, outCol2)
    }
  }
  df
}

#' @title missingCheck
#' @description Function to check individual data items for missing values. The new version also supports coded missing values and data blanking rules. Data values hidden due to data blanking rules are not considered missing values.
#' @export
#'
missingCheck<- function (df, item, itemCol, cl1, cl2, ...){
  vars <- list(...)
  if(length(vars)==3) {
    if (vars[[1]]=="NA") missingCode = NA
    else if (vars[[1]]=="missingCode") missingCode <-env$vm_misg_code
    itemCond <- vars[[2]]
    cond <- vars[[3]]
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
    item.vec <- env$studyData[[item]]
    if(!is.empty(item.vec)){
      if(!is.empty(env$studyData[[itemCond]])) {
        a <- which(env$studyData[[itemCond]]!=cond)
        for (j in a) {
          env$dq[,item][j] <- paste("hidden")
          env$studyData[,item][j] <- paste("hidden")
        }
      }
      #df$vm_misg[index] <-0
      m <- NULL
      if(!is.empty(env$studyData[[itemCond]])) cond.vec <- env$studyData[which(env$studyData[[itemCond]]==cond), item]
      if(!is.empty(cond.vec)){
        if (!is.empty(missingCode)) {
          for (code in missingCode) {
            if (is.na(code)) v <-  which(env$studyData[[itemCond]]==cond & is.na(env$studyData[[item]]))
            else if(code=="NULL") v <-  which(env$studyData[[itemCond]]==cond & is.null(env$studyData[[item]]))
            else  v <-  which(env$studyData[[itemCond]]==cond & as.character(env$studyData[[item]])==code)
            m <-c(m, v)
          }
        }
        else m <- which((env$studyData[[itemCond]]==cond & as.character(env$studyData[[item]])=="") |(env$studyData[[itemCond]]==cond & is.na(env$studyData[[item]])))
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
    else if (item!=env$ovrQuality){
      df <- addMissingValue(item, df, 0,0, itemCol)
      env$dq[,cl2]<- paste( item," was not collected ", env$dq[,cl2])
    }
  } else {
    missingCode <-env$vm_misg_code
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
    item.vec <- env$studyData[[item]]
    if(!is.empty(item.vec)){
      if (!is.empty(missingCode)) {
        for (code in missingCode) {
          if (is.na(code)) v <-which(is.na(item.vec))
          else if(code=="NULL") v <-which(is.null(item.vec))
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
    else if (item!=env$ovrQuality){
      df <- addMissingValue(item, df, 0,0, itemCol)
      env$dq[,cl2]<- paste( item, " was not collected ", env$dq[,cl2])
    }
  }
  df
}

#' @title checkSubjCompleteness
#' @description Function to check the completeness of recorded subjects such as inpatient or outpatients.
#' @export
#'
checkSubjCompleteness <-function(subj, itemVec){
  df <-data.frame(s = 0, s_inc= 0)
  if (all(itemVec %in%  colnames(env$studyData)==TRUE))
  {
    basicData <-subset(env$studyData, select= itemVec)
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
#' @description Function to check the completeness of case module.
#' @export
#'
checkCaseCompleteness<-function (caseItems, bItemCol){
  df <-data.frame(vm_case =0, vm_case_misg= 0)
  for (item in caseItems) {
    index1 = which(env$catMeta[, bItemCol]==item)
    if (length(index1) >0) {
      if (env$catMeta$vm[index1]==0) {
        df$vm_case_misg <- df$vm_case_misg+nrow(env$studyData)
        df$vm_case <- df$vm_case+nrow(env$studyData)
      }
      df$vm_case <- df$vm_case+env$catMeta$vm[index1]
      df$vm_case_misg <- df$vm_case_misg+env$catMeta$vm_misg[index1]
    }
    else{
      index2 = which(env$tempMeta[, bItemCol]==item)
      if (!is.null(index2) & !is.na(index2)){
        if (env$tempMeta$vm[index2]==0) {
          df$vm_case_misg <- df$vm_case_misg+nrow(env$studyData)
          df$vm_case <- df$vm_case+nrow(env$studyData)
        }
        df$vm_case <- df$vm_case+env$tempMeta$vm[index2]
        df$vm_case_misg <- df$vm_case_misg+env$tempMeta$vm_misg[index2]
      }
    }
  }
  if (is.null(env$metrics)) env$metrics <-df
  else env$metrics <-cbind(env$metrics,df)
  df
}

#' @title getMissingItem
#' @description This function checks the loaded data items for completeness issues and returns the detected missing data items.
#' @export
#'
getMissingItem<- function (mandatoryItems) {
  df <-data.frame( im = 0, im_misg = 0, im_misg_msg =0)
  base::assign(x="im", value=mandatoryItems, envir=env)
  env$studyData<- env$studyData[!sapply(env$studyData, function(x) all( is.empty(x) | is.na(x)))]
  diff <- setdiff(mandatoryItems, names(env$studyData))
  mItem <-""
  if (!is.empty (diff)){
    str<- paste (diff,collapse=" , " )
    mItem <- paste ("Following data items are missing: ", str)
  }
  df$im <- length(mandatoryItems)
  df$im_misg<- length(diff)
  df$im_misg_msg<- mItem
  if (is.null(env$metrics)) env$metrics <-df
  else env$metrics <-cbind(env$metrics,df)
  mItem
}

#' @title is.empty
#' @description This function checks whether a vector (data item) is empty.
#' @export
#'
is.empty <- function(x) return(length(x) ==0 )

#------------------------------------------------------------------------------------------------------
# functions to specify and  calculate DQ metrics for the plausibility dimension (D2)
#------------------------------------------------------------------------------------------------------

#' @title rangePlausibilityIndicator
#' @description  This function calculates the Range Plausibility Indicator (dqi_pl_rpr), a generic metric to assess the range plausibility of selected data values. It also adds related metadata and parameters in the DQ report.
#' @export
#'
rangePlausibilityIndicator<- function(v_slc, v_ip) {
  ind <-data.frame(
    Abbreviation= "dqi_pl_rpr",
    Label = "Range Plausibility Rate",
    Dimension ="Plausibility",
    Short_Description = "This indicator assesses the plausibility of selected data values regarding outliers. The publication DOI:10.1055/a-2006-1018 provides more details and examples."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(v_slc) & is.numeric(v_ip)) {
    if (v_slc>0) ind$value <-round (((v_slc-v_ip)/v_slc )*100,2)
    else ind$value <- NA
  } else {
    ind$value <- NA
    if (!is.numeric(v_slc)) v_slc <-NA
    if (!is.numeric(v_ip)) v_ip <-NA
  }
  df <- data.frame(vs_od= c(v_slc), vo= c(v_ip), dqi_pl_rpr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title semanticPlausibilityIndicator
#' @description  This function calculates the Semantic Plausibility Rate (dqi_pl_spr), a generic indicator to assess the semantic plausibility of selected data values, and adds related metadata and DQ parameters.
#' @export
#'
semanticPlausibilityIndicator<- function(vs_cd, vc) {
  ind <-data.frame(
    Abbreviation= "dqi_pl_spr",
    Label = "Semantic Plausibility Rate",
    Dimension = "Plausibility",
    Short_Description = "This indicator assesses the plausibility of selected data values regarding semantic contradictions. The publication DOI:10.1055/a-2006-1018 provides more details and examples."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(vs_cd) & is.numeric(vc)){
    if (vs_cd>0) ind$value<- round (((vs_cd-vc)/vs_cd)*100,2)
    else ind$value<- NA
  } else {
    ind$value<- NA
    if (!is.numeric(vs_cd)) vs_cd <-NA
    if (!is.numeric(vc)) vc <-NA
  }
  df <-data.frame(vs_cd = c(vs_cd), vc= c(vc), dqi_pl_spr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title addOutlier
#' @description Function to add detected outliers for each data item.
#' @export
#'
addOutlier<- function (item, bdata, m, n, ...) {
  vars <- list(...)
  df <-data.frame(vm_case =0, vm_case_misg= 0)
  bItemCl = "basicItem"
  if(!(is.empty(vars))){
    bItemCl <- vars[[1]]
  }
  index = which(bdata[, bItemCl]==item)
  if (length(index)>0)
  {
    if(!"vo" %in% colnames(bdata)) bdata$vo <-0
    if(!"vm" %in% colnames(bdata)) bdata$vm <-0
    if(!"vm_misg" %in% colnames(bdata)) bdata$vm_misg <-0
    if(!"vs_od" %in% colnames(bdata)) bdata$vs_od <-0
    if(n>0){
      bdata$vm[index] <-n
      bdata$vs_od[index] <- bdata$vm[index]-bdata$vm_misg[index]
      if (!is.na(bdata$vo[index]) && is.numeric(bdata$vo[index]) ) bdata$vo[index] <- bdata$vo[index]+m
      else bdata$vo[index] <- m
    }
    else {
      bdata$vm[index]<- 0
      bdata$vo[index] <- NA
    }
    bdata
  }else bdata
}

#' @title addOutlierCount
#' @description Function to count and add detected outliers as well as performed checks.
#'
addOutlierCount<- function (bdata, col, row) {
  index = which( bdata[,col]==row)
  bdata$vo[index] <- sum (as.integer(as.character( bdata$vo[-index] )), na.rm=TRUE)
  bdata$vs_od[index] <- sum (as.integer(as.character( bdata$vs_od[-index] )), na.rm=TRUE)
  bdata
}

#' @title addContradictionCount
#' @description Function to count and add detected contradictions as well as performed checks.
#'
addContradictionCount <- function(metrics){
  contra <-base::get("contra", envir=env)
  if (is.empty(contra$rules)) {
    df <-data.frame(cont = 0, vc= 0, vs_cd =0)
  } else df <- subset (contra$rules, select =c("cont", "vc", "vs_cd"))
  vc = sum( as.integer(as.character(df$vc)), na.rm=TRUE)
  vs_cd = sum( as.integer(as.character(df$vs_cd)), na.rm=TRUE)
  cont <- sum(as.integer(as.character(df$cont)), na.rm=TRUE)
  total <- c(env$ovrQuality, cont, vc, vs_cd)
  if (is.empty(contra$rules)) contra$rules <-total
  else contra$rules[nrow(contra$rules) + 1,] <- total
  env$contra$rules <- contra$rules
  param <-data.frame(cont = cont, vc= vc, vs_cd = vs_cd)
  metrics <- cbind(metrics, param)
  metrics
}

#------------------------------------------------------------------------------------------------------
# functions to detect plausibility issues
#------------------------------------------------------------------------------------------------------

#' @title checkLogicalRule<
#' @description Function to detect contradictions using a predefined logical rule
#' @export
#'
checkLogicalRule<- function (rID, contra, contraCol, item1, value1, item2, value2, ...) {
  simpleConj <- c(value1, value2)
  vars <- list(...)
  if(length(vars)==2){
    item3 <- vars[[1]]
    value3 <- vars[[2]]
    complexConj <- c(value1, value2, value3)
    if (!any(is.empty(complexConj)) & !any(is.na(complexConj))){
      contraCols <- c(rID, item1, item2, item3,"cont", "vc")
      df<- data.frame(matrix( ncol= 6, nrow = 0))
      colnames(df) <- contraCols
      checkedItems <- c(item1, item2, item3)
      checkedValues <- na.omit (env$studyData[,checkedItems])
      if (!is.empty(checkedValues )) nValue <- nrow (checkedValues) * ncol (checkedValues)
      else  nValue = 0
      if (is.empty(contra$rules)) {
        rulesCols <- c("rID", "cont", "vc", "vs_cd")
        contra$rules <- data.frame(matrix( ncol= 4, nrow = 0))
        colnames(contra$rules) <- rulesCols
      }
      out <-checkSymbolicConjunctions(item1, value1, item2, value2, item3, value3)
      if (!is.empty(out)) {
        for(i in out){
          df[nrow(df) + 1,] <- c(rID, env$dq[[item1]][i], env$dq[[item2]][i], env$dq[[item3]][i], 1, 3)
          msg <- paste("Implausible combination according to the rule ", rID , ": ", item1, "=", env$dq[[item1]][i] ,  ", ",item2, "=", env$dq[[item2]][i]," ", item3, "=", env$dq[[item3]][i], ".", sep="")
          #print(msg)
          env$dq[,contraCol][i] <-paste (msg, env$dq[,contraCol][i])
        }
        df["cont"]  <-as.numeric(unlist(df["cont"]))
        df["vc"]  <-as.numeric(unlist(df["vc"] ))
        if (nrow(df)!=0) contra <- append(contra, list(df))
        rule <- c(rID, length(out), sum(df["vc"]), nValue)
        contra$rules[nrow(contra$rules) + 1,] <- rule
      }else {
        contra <- append(contra, list(df))
        rule <- c(rID, 0, 0, nValue)
        contra$rules[nrow(contra$rules) + 1,] <- rule
      }
    }
  } else if (!any(is.empty(simpleConj)) & !any(is.na(simpleConj))){
    contraCols <- c(rID, item1, item2, "cont", "vc")
    df<- data.frame(matrix( ncol= 5, nrow = 0))
    colnames(df) <- contraCols
    checkedItems <- c(item1, item2)
    checkedValues <- na.omit (env$studyData[,checkedItems])
    nValue<- nrow (checkedValues) * ncol (checkedValues)
    if (!is.empty(checkedValues )) nValue <- nrow (checkedValues) * ncol (checkedValues)
    else  nValue = 0
    if (is.empty( contra$rules)) {
      rulesCols <- c("rID", "cont", "vc", "vs_cd")
      contra$rules <- data.frame(matrix(ncol= 4, nrow = 0))
      colnames(contra$rules) <- rulesCols
    }
    out <-checkSymbolicConjunctions(item1, value1, item2, value2)

    if (!is.empty(out)) {
      for(i in out) {
        df[nrow(df) + 1,] <- c(rID, env$dq[[item1]][i], env$dq[[item2]][i], 1, 2)
        msg <- paste( "Implausible combination according to the rule ", rID , ": ", item1, "=", env$dq[[item1]][i] , ", ", item2, "=", env$dq[[item2]][i], ".", sep="")
        #print(msg)
        env$dq[,contraCol][i] <-paste (msg, env$dq[,contraCol][i])
      }
      df["cont"] <-as.numeric(unlist(df["cont"]))
      df["vc"]  <-as.numeric(unlist(df["vc"]))
      if (nrow(df)!=0) contra <- append(contra, list(df))
      rule <- c(rID, length(out), sum (df["vc"]), nValue)
      contra$rules[nrow(contra$rules) + 1,] <- rule

    }else {
      contra <- append(contra, list (df))
      rule <- c(rID, 0, 0, nValue)
      contra$rules[nrow(contra$rules) + 1,] <- rule
    }
  }
  contra
}

#' @title checkSymbolicConjunctions
#' @description Function to detect contradictions using predefined symbolic conjunctions
#' @import anytime
#' @export
#'
checkSymbolicConjunctions<- function (item1, value1, item2, value2, ...) {
  vars <- list(...)
  if(length(vars)==2) {
    item3 <- vars[[1]]
    value3 <- vars[[2]]
    contra <- which(as.character(env$studyData[,item1])==value1 & as.character(env$studyData[, item2])==value2 & as.character(env$studyData[, item3])==value3)
  }
  else contra <- which(as.character(env$studyData[,item1])==value1 & as.character(env$studyData[, item2])==value2)
  contra
}

#' @title checkRangeRule
#' @description Function to detect outliers using a predefined range rule
#' @export
#'
checkRangeRule<- function (ndata, rID, itemCol, outlierCol, item, min, max, ...) {
  vars <- list(...)
  item.vec <- getNumValue(env$dq[[item]])
  if (!is.empty(item.vec)){
    if (is.empty(vars)){
      out <- which(item.vec<min | item.vec>max)
      ndata <- addOutlier(item, ndata, length(out), length(item.vec), itemCol)
      for(i in out) {
        msg<- paste("Implausible", item , ":", item.vec[i], "according to the range rule:", rID)
        if (grepl("Implausible", env$dq[,outlierCol][i] , fixed = TRUE)) env$dq[,outlierCol][i] <- paste (msg,"; ", env$dq[,outlierCol][i])
        else env$dq [,outlierCol] [i] <- msg
      }
    } else if(length(vars)==4){
      item2 <- vars[[1]]
      value2 <- vars[[2]]
      item3 <- vars[[3]]
      value3 <- vars[[4]]
      item2.vec <- as.character(env$dq[[item2]])
      item3.vec <- as.character(env$dq[[item3]])
      out <- which(!is.na(item.vec) & (item.vec< min | item.vec > max) & item2.vec ==value2 & item3.vec==value3)
      if (!is.empty (out)) {
        for(i in out) {
          msg<- paste("Implausible", item , ":", item.vec[i], "according to the range rule:", rID)
          if (grepl("Implausible", env$dq[,outlierCol][i] , fixed = TRUE)) env$dq[,outlierCol][i] <- paste (msg, "; ", env$dq[,outlierCol][i])
          else env$dq [,outlierCol] [i] <- msg
        }
      }
      ndata <- addOutlier(item, ndata, length(out), length(item.vec), itemCol)
    }
  }
  ndata
}

#' @title checkAgePlausibility
#' @description This Function checks the data values related to the data item "birthdate" for implausible values and returns the detected plausibility issues.
#' @export
#'
checkAgePlausibility<- function (birthDate, currentData, ageMax){
  diff <-  ifelse ((isDate(birthDate) & isDate(currentData)), as.numeric(difftime(birthDate, currentData),units="weeks")/52.25 , 0 )
  out <- which(abs(diff)>ageMax)
  out
}

#' @title checkFutureDate
#' @description This function checks the loaded temporal data for implausible values and return the values dated to the future.
#' @export
#'
checkFutureDate<- function (dateValues){
  now<- as.Date(Sys.Date())
  out <-  vector()
  out <- which(isDate(dateValues) & (as.Date(dateValues)>now))
  out
}

#------------------------------------------------------------------------------------------------------
# functions to calculate overall DQ metrics
#------------------------------------------------------------------------------------------------------

#' @title getTotalStatistic
#' @description Function to aggregate the different types of analyzed data and calculate the overall DQ metrics.
#' @export
#'
getTotalStatistic <- function(col, row){
  if (!is.empty(env$catMeta) & !is.empty (env$tempMeta)) {
    allMeta <- base::merge(env$catMeta, env$tempMeta, by=intersect(names(env$catMeta), names(env$tempMeta)), all = TRUE)
    env$catMeta<- addStatistic(env$catMeta, col, row)
    env$tempMeta<- addStatistic(env$tempMeta, col, row)
  }
  else if (!is.empty(env$catMeta)) {
    allMeta <- env$catMeta
    env$catMeta<- addStatistic(env$catMeta, col, row)
  }
  else if (!is.empty(env$tempMeta)) {
    allMeta <- env$tempMeta
    env$tempMeta<- addStatistic(env$tempMeta, col, row)
  }
  if (!is.empty (env$numMeta)) {
    allMeta<- base::merge(allMeta, env$numMeta, by=intersect(names(allMeta), names(env$numMeta)), all = TRUE)
    env$numMeta<- addStatistic(env$numMeta, col, row)
  }
  if (!is.empty(allMeta$engLabel)) allMeta$engLabel <-NULL
  if (!is.empty(env$oItem)) {
    for (i in env$oItem){
      index = which(allMeta[,col]==i)
      allMeta$im_misg[index] <- 0
    }
  }
  allMeta<- addStatistic(allMeta, col, row)
  total <- subset(allMeta, allMeta[,col]==row)
  total[,col] <- NULL
  inter <-intersect(colnames(total), colnames(env$metrics))
  if (!is.null(inter)){
    df <-env$metrics[!colnames(env$metrics) %in% inter]
    env$metrics<-cbind(total, df)
  } else if (!is.empty(env$metrics)) env$metrics<-cbind(total, env$metrics) else env$metrics<-total
  env$allMeta <-allMeta
  env$metrics <- addContradictionCount(env$metrics)
  env$metrics
}

#' @title addStatistic
#' @description Function to count all detected DQ issues in the completeness and plausibility dimensions.
#'
addStatistic<- function (bdata, col, row) {
  index = which(bdata[,col]==row)
  if (length(index) >0) bdata<-bdata[-index,]
  bdata[nrow(bdata) + 1, ] <-  NA
  bdata[nrow(bdata), col] <-row
  bdata = addMissingValueCount(bdata,col,row)
  bdata = addMissingItemCount(bdata,col,row)
  bdata = addOutlierCount(bdata,col,row)
  bdata
}

#------------------------------------------------------------------------------------------------------
# Functions to generate data quality reports and visualizations
#------------------------------------------------------------------------------------------------------

#' @title visualizeOutliers
#' @description Function to visualize detected outliers using a bar chart.
#' @import ggplot2
#' @export
#'
visualizeOutliers <- function (itemCol, valueCol, sumRow, voPath, ...){
  index = which(env$numMeta[, itemCol]==sumRow)
  df <-subset(env$numMeta[-index,], valueCol >=0, select = c(itemCol, valueCol))
  dataItems <- df[, itemCol]
  outlierNumbers <- df[, valueCol]
  vars <- list(...)
  legend = TRUE
  labelSize = 10
  counterSize = 3
  if(!is.empty(vars)){
    if (!is.null(vars$a)) legend=vars[[1]]
    if (!is.null(vars$b)) labelSize=vars[[2]]
    if (!is.null(vars$c)) counterSize=vars[[3]]
  }
  ggplot(df, aes(x= reorder(dataItems, outlierNumbers), y=outlierNumbers, fill=dataItems)) +
    geom_bar(stat="identity", show.legend = legend) +
    geom_text(aes(label = round(outlierNumbers, digits = 0)), vjust = 0, size=counterSize)+
    labs(x="Data Items", y="Outliers", fill= "Data Items") +
    theme(axis.text.x=element_text(angle=45, size=labelSize, hjust=1), legend.text =element_text(size=labelSize))
  path<-paste (voPath, ".png")
  ggsave(path)
}

#' @title geReport
#' @description This function generates DQ reports on detected quality issues and user-selected metrics.
#' @import openxlsx utils
#' @export
#'
getReport <- function(repMeta, sheetName, df, path){
  if (is.list(df)) dfList <- df
  else dfList <- list ("DQ_Metrics"=df)
  if (is.list(sheetName)) sheetList <- sheetName
  else sheetList <- c ("DQ_Metrics", sheetName)
  if (grepl("csv", path, fixed=TRUE))  write.csv(dfList[[1]], path,  row.names = FALSE)
  else {
    if (!is.null(repMeta)) {
      if (is.data.frame(repMeta)) {
        repCol <- append(repMeta$repCol, sheetName)
        englCol <- append(repMeta$engLabel, sheetName)
      } else  repCol = append (repMeta, sheetName)
      repData <-subset(env$dq, select= repCol)
      dfq <-repData[ which(env$dq[,sheetName]!="")  ,]
      if (exists("englCol")) names(dfq)=englCol
      dfq[nrow(dfq)+1,5] <- env$mItem
      dfList <- list("DQ_Metrics" = df, "DQ_Issues"=dfq)
      write.csv(df, paste (path,".csv", sep =""), row.names = FALSE)
      path <-  paste (path,".xlsx", sep ="")
    }
    index=0
    wb <- createWorkbook()
    header_st <- createStyle(textDecoration = "Bold")
    for (df in dfList)
    {
      index=index+1
      addWorksheet(wb, sheetList[index])
      setColWidths(wb, sheet=sheetList[index], cols =1:30, widths = "auto")
      writeData(wb, sheet = sheetList[index], x = df,  headerStyle = header_st)
    }
    if (!is.null(env$semantics)) {
      addWorksheet(wb, "Semantics")
      setColWidths(wb, sheet="Semantics", cols =1:30, widths = "auto")
      writeData(wb, sheet = "Semantics", env$semantics,  headerStyle = header_st)
    }
    saveWorkbook(wb, path, overwrite = TRUE)
  }
}

#' @title addSemantics
#' @description This function adds semantic enrichment to resulting DQ metrics in the DQ report.
#' @export
#'
addSemantics <- function (dqRep, semData, ...) {
  vars <- list(...)
  if(!(is.empty(vars))){
    cl <- vars[[1]]
  }
  else cl <-semData$SymbolicName
  tRep <- as.data.frame(t(dqRep))
  Abbreviation  <- rownames (tRep)
  Label <-NA
  tRep <- cbind (Label, tRep)
  tRep <- cbind (Abbreviation, tRep)
  rownames(tRep) <- NULL
  colnames(tRep)[3] <-  "Value"
  for (item in tRep$Abbreviation)
  {
    if (item  %in%  cl){
      k<- which(tRep$Abbreviation==item)
      l<- which(cl==item)
      tRep$Label[k]=semData$Label[l]
      abr<-semData$Abbreviation[l]
      tRep$Abbreviation[k]=abr
      if ( grepl( "dqi", abr, fixed=TRUE))
      {
        if (abr !="dqi_cc_rvl") tRep$Value[k]<-paste0(gsub("\\.", ",", tRep$Value[k]), '%')
        #tRep$Value[k]<-paste0(gsub("\\.", ",", tRep$Value[k]), '%')
      }
    }
  }
  tRep
}

#------------------------------------------------------------------------------------------------------
# Utility: helper functions
#------------------------------------------------------------------------------------------------------

#' @title getMandatoryItems
#' @description This function returns the mandatory data items (im).
#
getMandatoryItems <- function (metaCol, opt, df, ...){
  vars <- list(...)
  diff <- setdiff(df[, metaCol], opt)
  im <- diff
  if(length(vars)==1){
    df1 <- vars[[1]]
    diff1 <- setdiff(df1[, metaCol], opt)
    im<- union(im, diff1)
  }
  if(length(vars)==2){
    df1 <- vars[[1]]
    df2 <- vars[[2]]
    diff1 <- setdiff(df1[, metaCol], opt)
    diff2 <- setdiff(df2[, metaCol], opt)
    im<- Reduce(union, list(im, diff1, diff2))
  }
  im
}

#' @title getNumValue
#' @description Function to transform data into numeric type.
#
getNumValue<- function (data) {
  if (is.character(data))
    return <-  as.double(as.character (data))
  else if (is.numeric(data))
    return <- data
  else if (is.factor(data))
    return <- as.double(as.character(data))
  else retun <- NULL
}

#' @title isDate
#' @description This function checks whether a given data value has date format.
#' @export
#'
isDate <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate,tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y","%Y.%m.%d","%d.%m.%Y","%m.%d.%Y"))),
           error = function(err) {FALSE})
}

#' @title getUserSelectedMetrics
#' @description This function enable users to select desired DQ metrics.
#'
getUserSelectedMetrics <- function(dqInd, df){
  for (m in dqInd){
    if (!(m  %in% names(df))) {
      stop ("undefined DQ metric: ", m)
      #print(paste(" undefined DQ metric:", m))
      #quit()
    }
  }
  dqMetrics <- subset(df, select= dqInd)
  dqMetrics
}

#' @title getFileExtension
#' @description Function to get the file extension of a given file.
#'
getFileExtension <- function(filePath){
  ext <- strsplit(basename(filePath), split="\\.")[[1]]
  return(ext[-1])
}

#' @title getPercentFormat
#' @description This function formats given values as a percentage
#'
getPercentFormat <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

#' @title getDateFormat
#' @description This function converts a given data vector in a date format
#' @import anytime
#' @export
#'
getDateFormat <- function (dataVec)
{
  if(!isDate(dataVec)) dataVec <-anytime(as.character(dataVec))
  #dataVec * 100) +1
  dataVec
}

#------------------------------------------------------------------------------------------------------
# Deprecated code
#------------------------------------------------------------------------------------------------------

#' @title addCompleteness
#' @description This function to calculate the value completeness rate.
#' @deprecated replaced by addValueCompleteness()
#'
addCompleteness<- function (tdata, col, row) {
  index = which( tdata[,col]==row)
  tdata$completness_rate[index]<-round (100-tdata$missing_value_rate[index],2)
  return <- tdata
}

#' @title addMissingCount
#' @description This function adds the overall metrics for missing values.
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

#' @title  setGlobals
#' @description Function to define global variables.
#' @deprecated replaced by setEnvironment
#' @export
#'
setGlobals <- function(medData, repCol, cdata, ddata, tdata) {
  env$studyData <- medData
  env$catMeta <- cdata
  env$tempMeta <- ddata
  env$metrics <- tdata
  env$repMeta <-repCol
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
    if ("s_inc" %in% names (df))   df$incomplete_subject_no_py <- df$s_inc
    if ("vo" %in% names(df))       df$outlier_no_py <- df$vo
    if ("dqi_co_icr" %in% names(df))  df$item_completeness_rate <- df$dqi_co_icr
    if ("dqi_co_vcr" %in% names(df))  df$value_completeness_rate <- df$dqi_co_vcr
    if ("dqi_co_scr" %in% names(df))  df$subject_completeness_rate <-df$dqi_co_scr
    if ("dqi_co_ccr" %in% names(df))  df$case_completeness_rate <-df$dqi_co_ccr
    if ("dqi_pl_rpr" %in% names(df))  df$range_plausibility_rate<-df$dqi_pl_rpr
    if ("dqi_co_ocr" %in% names(df))  df$orphaCoding_completeness_rate <-df$dqi_co_ocr
    if ("dqi_pl_opr" %in% names(df))  df$orphaCoding_plausibility_rate <-df$dqi_pl_opr
    if ("dqi_un_cur" %in% names(df))  df$rdCase_unambiguity_rate <-df$dqi_un_cur
    if ("dqi_un_cdr" %in% names(df))  df$rdCase_dissimilarity_rate <-df$dqi_un_cdr
    if ("rdCase_rel" %in% names(df))  df$rdCase_rel_py_ipat <-df$rdCase_rel
    if ("tracerCase_rel" %in% names(df))  df$tracerCase_rel_py_ipat <-df$tracerCase_rel
    if ("orphaCase_rel" %in% names(df))  df$orphaCase_rel_py_ipat <-df$orphaCase_rel
  }

  df
}
