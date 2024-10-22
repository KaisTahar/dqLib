#######################################################################################################
# Description: The data quality library (dqLib) is an R package for data quality (DQ) assessment and reporting.
# This package provides functions that enable users to select desired dimensions, indicators, and parameters as well as to define specific DQ reports.
# As part of the "dqLib" package this script includes functions for calculating DQ metrics and generating reports on detected DQ issues, especially in the field of Rare Diseases(RDs)
# Date Created: 2021-02-26
# Kais Tahar, University Medical Center Göttingen
# ######################################################################################################

#' @title checkCordDQ
#' @description This function checks the quality of loaded data regarding selected DQ metrics.
#' The default DQ dimensions are completeness, plausibility, uniqueness and concordance.
#' @import stringi
#' @export
#'
checkCordDQ <- function (instID, reportYear, inpatientCases, refData1, refData2, dqInd, repCol, cl, bItemCl, totalRow, oItem,...) {
  vars <- list(...)
  if (is.null (cl)) stop("No report design available")
  if (is.null (env$medData)) stop("No data available")
  if (is.null(env$medData$ICD_Primaerkode)) stop("Missing mandatory item: ICD_Primaerkode")
  if (is.null(env$medData$Orpha_Kode)) env$medData$Orpha_Kode <-NA
  else env$dq <- { subset(env$medData, select = repCol)
    env$dq[cl]<-""
  }
  env$tdata$report_year <-reportYear
  if(is.null(oItem)) mv <-totalRow
  env$oItem <-oItem
  mv <-c (totalRow, oItem)
  if (is.null(env$ddata)) basicItem <- setdiff(env$cdata[, bItemCl],mv)
  else basicItem <- setdiff (union(env$cdata[, bItemCl], env$ddata[, bItemCl]),mv)
  if ( !is.null(instID)){
    env$tdata$inst_id <- instID
    instData<- env$medData[which(env$medData$Institut_ID==instID),]
    if (nrow(instData)>0) env$medData <- instData
  }else {
    env$tdata$inst_id <- "ID fehlt"
  }
  rdDup_no =0
  inputData <-env$medData
  row_no = nrow(inputData)
  eList <-refData1[which(refData1$Unique_SE=="yes"),]
  
  if(!is.empty(env$medData$PatientIdentifikator) & !is.empty(env$medData$Aufnahmenummer) & !is.empty(env$medData$ICD_Primaerkode) & !is.empty(env$medData$Orpha_Kode))
  {
    env$medData<-env$medData[!duplicated(env$medData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode","Orpha_Kode")]),]
    env$dq <- subset(env$medData, select = repCol)
    env$dq[cl]<-""
    dup <-inputData[duplicated(inputData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode","Orpha_Kode")],fromLast=TRUE),]
    if (!dim(dup)[1]==0)
    { 
      dup$dupRdCase <-NA
      icdList <-which(!( dup$ICD_Primaerkode =="" | is.na(dup$ICD_Primaerkode) | is.empty(dup$ICD_Primaerkode)))
      for(i in icdList){
        iCode <- stri_trim(as.character(dup$ICD_Primaerkode[i]))
        oCode <- stri_trim(as.character(dup$Orpha_Kode[i]))
        if (is.element(iCode, stri_trim(as.character(eList$IcdCode))))
        {
          dup$dupRdCase[i] = "yes"
        }
        else if (!is.na(as.numeric(oCode))) {
          dup$dupRdCase[i] = "yes"
        }
      }
      dupRd <-dup[which(dup$dupRdCase=="yes"),]
      rdDup_no <- length (unique(dupRd$Aufnahmenummer))
      env$dup <-dup
      
    } else   rdDup_no =0
    
  }
  else if(!is.empty(env$medData$PatientIdentifikator) & !is.empty(env$medData$Aufnahmenummer) & !is.empty(env$medData$ICD_Primaerkode))
  {
    env$medData<-env$medData[!duplicated(env$medData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode")]),]
    env$dq <- subset(env$medData, select = repCol)
    env$dq[cl]<-""
    dup <-inputData[duplicated(inputData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode")], fromLast=TRUE),]
    if (!dim(dup)[1]==0)
    { 
      dup$dupRdCase <-NA
      icdList <-which(!( dup$ICD_Primaerkode =="" | is.na(dup$ICD_Primaerkode) | is.empty(dup$ICD_Primaerkode)))
      for(i in icdList){
        iCode <- stri_trim(as.character(dup$ICD_Primaerkode[i]))
        if (is.element(iCode, stri_trim(as.character(eList$IcdCode))))
        {
          dup$dupRdCase[i] = "yes"
        }
      }
      dupRd <-dup[which(dup$dupRdCase=="yes"),]
      rdDup_no <- rdDup_no +length (unique(dupRd$Aufnahmenummer))
      env$dup <-base::rbind(env$dup, dup)
    }else   rdDup_no =0
  }
  if(!is.empty(env$medData$PatientIdentifikator)) env$tdata$patient_no = length (unique(env$medData$PatientIdentifikator))
  if(!is.empty(env$medData$Aufnahmenummer)) env$tdata$case_no = length (env$medData$Aufnahmenummer[which(!duplicated(env$medData$Aufnahmenummer)& ! is.na(env$medData$Aufnahmenummer))])
  
  #D1 completeness
  keyD1 <- checkD1( refData1, cl, basicItem, bItemCl)
  env$mItem <- keyD1$mItem
  env$tdata <- addD1(env$tdata, keyD1$k2_orpha_no, keyD1$k2_orphaCheck_no)
  itemVec <- names (env$medData)
  inter <- intersect (basicItem, itemVec)
  env$tdata$s_inc <- checkSubjCompleteness("PatientIdentifikator", inter)$s_inc
  if(!is.empty(vars)) caseItems <- vars[[1]]
  else caseItems <- NULL
  if(!is.null(caseItems))  env$tdata$vm_case_misg<-checkCaseCompleteness(caseItems, bItemCl)$vm_case_misg
  #D2 plausibility
  keyD2 <- checkD2(refData2, bItemCl, cl)
  env$tdata <- addD2(env$tdata, keyD2$k1_rd_counter, keyD2$k1_check_counter)
  #D3 uniqueness
  env$tdata$case_dup = row_no - nrow(env$medData)
  env$tdata$rdCase_dup =rdDup_no
  keyD3 <- checkD3( refData1, refData2, cl)
  env$tdata <- addD3(env$tdata, keyD3$k3_unambiguous_rdDiag_no,  keyD3$k3_unambiguous_rdCase_no, keyD3$k3_checkedRdCase_no)
  #D4 concordance
  keyD4 <- checkD4(cl)
  env$tdata <- setAnnualVars(env$tdata)
  env$tdata <- addD4(env$tdata,  keyD4$k4_counter_orpha, keyD4$k4_counter_orphaCase, keyD3$k3_unambiguous_rdCase_no, inpatientCases)
  if(!is.empty(vars) & length(vars)>=2) concRef <- vars[[2]]
  else concRef <- NULL
  if(!is.null(concRef)) env$tdata$conc_with_refValues<-getConcWithRefValues(env$tdata$tracerCase_rel_py_ipat, concRef)
  orphaCases <- env$dq[which(env$dq$orphaCase=="yes"),]
  rdCases <- env$dq[which (env$dq$CheckedRdCase=="yes"),]
  env$tdata$orphaPatient_no_py = length(unique(orphaCases$PatientIdentifikator))
  env$tdata$rdPatient_no_py = length(unique(rdCases$PatientIdentifikator))
  # DQ metrics
  metrics<-getTotalStatistic(bItemCl, totalRow)
  metrics<-cbind(metrics, env$tdata)
  metrics <- cbind(metrics, env$tdata)
  metrics$range_plausibility_rate <-100-metrics$outlier_rate
  metrics$outlier_no_py <- metrics$outlier_no
  metrics$dqi_co_icr <- itemCompletenessIndicator(metrics$im, metrics$im_misg)$value
  metrics$dqi_co_vcr <- valueCompletenessIndicator(metrics$vm, metrics$vm_misg)$value
  metrics$dqi_co_scr <- subjectCompletenessIndicator(metrics$s, metrics$s_inc)$value
  metrics$dqi_co_ccr <- caseCompletenessIndicator(metrics$vm_case, metrics$vm_case_misg)$value
  env$report <- deprecatedMetrics(metrics)
  userMetrics<- getUserSelectedMetrics(dqInd, env$report)
  out <- list()
  out[["metric"]] <- userMetrics
  out[["mItem"]] <-env$mItem
  out
}

#------------------------------------------------------------------------------------------------------
# functions for the completeness dimension (D1)
#------------------------------------------------------------------------------------------------------

#' @title orphaCompletenessIndicator
#' @description This function calculates the Orphacoding Completeness Rate  (dqi_cc_ocr), a specific indicator for RD coding, and adds related metadata and DQ parameters.
#' @export
#'
orphaCompletenessIndicator <- function(icd_tracer, oc_misg) {
  ind <-data.frame(
    Abbreviation= "dqi_co_ocr",
    Label = "Orphacoding Completeness Rate",
    Dimension ="Completeness",
    Short_Description = "This indicator assesses whether all cases with tracer diagnoses (icd_tracer) are coded using Orphacodes. Further details and examples are available under DOI:10.1055/a-2006-1018."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(icd_tracer) & is.numeric(oc_misg)) {
    if (icd_tracer>0) ind$value<-round (((icd_tracer - oc_misg)/icd_tracer)*100,2)
    else ind$value <- NA
  } else {
    ind$value <- NA
    if (!is.numeric(icd_tracer)) icd_tracer <-NA
    if (!is.numeric(oc_misg)) oc_misg <-NA
  }
  df <-data.frame(icd_tracer = c(icd_tracer), oc_misg = c(oc_misg), dqi_co_ocr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title checkD1
#' @description This function checks the quality of loaded data regarding the completeness dimension (D1).
#'
checkD1 <- function ( refData, cl, basicItems,bItemCl){
  env$medData<- env$medData[!sapply(env$medData, function(x) all( is.empty(x) | is.na(x)))]
  mItem <- getMissingItem(basicItems)
  if (!is.null(env$cdata)) env$cdata <- getMissingValue(env$cdata, bItemCl, "missing_value", "missing_item")
  if (!is.null(env$ddata))env$ddata <- getMissingValue(env$ddata, bItemCl, "missing_value", "missing_item")
  if (!is.null(env$medData$Orpha_Kode)) dqList <- append(checkOrphaCodingCompleteness(refData, cl), list (mItem=mItem))
  else {
    dqList <-list(k2_orphaCheck_no =0,k2_orpha_no=0,mItem=mItem)
  }
  dqList
}

#' @title checkOrphaCodingCompleteness
#' @description This function checks the completeness of Orphacoding.
#' @import stringi
#'
checkOrphaCodingCompleteness <- function (refData, cl){
  env$dq$tracer <-NA
  k2_orpha_no =0
  k2_orphaCheck_no=0
  missing_counter1=0
  missing_counter2=0
  refData <-refData[which(refData$Complete=="yes"),]
  env$cdata <- addMissingValue("Orpha_Kode",env$cdata, 0,0)
  env$cdata <- addMissingValue("AlphaID_Kode",env$cdata, 0,0)
  if (!is.null(env$medData$ICD_Primaerkode))
  {
    iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
      rdRefList<- which(stri_trim(as.character(refData$IcdCode))==iCode)
      if (!is.empty(rdRefList)) {
        env$dq$tracer[i] <-"yes"
        if("Orpha_Kode" %in% colnames(env$medData)){
          code <-as.character(env$medData$Orpha_Kode[i])
          if(!(is.null(code) | is.na(code) | is.empty(code)))
          {
            oCode <-as.numeric(code)
            if (is.na(oCode)){
              k2_orphaCheck_no = k2_orphaCheck_no +1
              missing_counter1 =missing_counter1 +1
            }
            else {
              k2_orphaCheck_no = k2_orphaCheck_no +1
              k2_orpha_no =k2_orpha_no +1
            }
          }
          else{
            k2_orphaCheck_no = k2_orphaCheck_no +1
            env$dq[,cl][i] <- paste("Missing Orpha Code. ", env$dq[,cl][i])
            missing_counter1 =missing_counter1 +1
          }
        }
        if("AlphaID_Kode" %in% colnames(env$medData)){
          aCode <-as.character(env$medData$AlphaID_Kode[i])
          if (is.na(aCode) | is.empty(aCode)) {
            env$dq[,cl][i] <- paste("Missing AlphaID Code. ", env$dq[,cl][i])
            missing_counter2 =missing_counter2 +1
          }
        }
      }
    }
  }
  else {
    oList <-which(env$medData$Orpha_Kode !="" & !is.na(env$medData$Orpha_Kode)  & !is.empty(env$medData$Orpha_Kode))
    k2_orpha_no = length(oList)
    k2_orphaCheck_no = length(env$medData$Orpha_Kode)
    missing_counter1 = k2_orphaCheck_no - k2_orpha_no
    aList <-which(env$medData$AlphaID_Kode !="" & !is.na(env$medData$AlphaID_Kode)  & !is.empty(env$medData$AlphaID_Kode))
    k2_alpha_no = length(aList)
    k2_checkAlpha_no = length(env$medData$AlphaID_Kode)
    missing_counter2 = k2_checkAlpha_no -k2_alpha_no
  }
  
  tracer <-env$dq[ which (env$dq$tracer=="yes"),]
  env$tdata$tracerCase_no <- length (unique(tracer$Aufnahmenummer))
  env$cdata <- addMissingValue("Orpha_Kode", env$cdata, missing_counter1,k2_orphaCheck_no )
  env$cdata <- addMissingValue("AlphaID_Kode", env$cdata, missing_counter2 ,k2_orphaCheck_no )
  
  out <- list()
  out[["k2_orphaCheck_no"]] <-k2_orphaCheck_no
  out[["k2_orpha_no"]] <-k2_orpha_no
  out
}


#' @title addD1
#' @description This function adds DQ metrics for the completeness dimension (D1).
#'
addD1<- function (tdata,  orpha, icd_tracer) {
  if(icd_tracer>0){
    tdata$oc <- orpha
    tdata$oc_misg <-icd_tracer-orpha
    tdata$dqi_co_ocr <-orphaCompletenessIndicator(icd_tracer, tdata$oc_misg)$value
  }
  else {
    tdata$oc <- 0
    tdata$dqi_co_ocr <-0
    tdata$oc_misg <-NA
  }
  tdata
}

#------------------------------------------------------------------------------------------------------
# functions for the plausibility dimension (D2)
#------------------------------------------------------------------------------------------------------

#' @title orphaPlausibilityIndicator
#' @description This function calculates the Orphacoding Plausibility Rate (dqi_pl_opr), a specific indicator for assessing the semantic plausibility of RD diagnoses, and adds related metadata and DQ parameters.
#' @export
#'
orphaPlausibilityIndicator <- function(link, link_ip) {
  ind <-data.frame(
    Abbreviation= "dqi_pl_opr",
    Label = "Orphacoding Plausibility Rate",
    Dimension = "Plausibility",
    Short_Description = "This indicator assesses the semantic plausibility of coded RD diagnoses according to the standard terminology Alpha-ID-SE. The publication DOI:10.1055/a-2006-1018 provides more details and examples."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(link) & is.numeric(link_ip)){
    if (link>0) ind$value<- round (((link-link_ip)/link)*100,2)
    else ind$value<- NA
  } else {
    ind$value<- NA
    if (!is.numeric(link)) link <-NA
    if (!is.numeric(link_ip)) link_ip <-NA
  }
  df <-data.frame(link = c(link), link_ip= c(link_ip), dqi_pl_opr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}


#' @title checkD2
#' @description This function checks the quality of loaded data regarding the plausibility dimension (D2).
#'
checkD2 <- function (refData2, bItemCl, cl){
  # get outliers
  if (!is.null(env$ddata))
  {
    dItem <- env$ddata[, bItemCl]
    if (!is.empty(dItem)) {
      for (item in unique(dItem)) {
        env$ddata  <-checkOutlier(env$ddata, item, cl)
      }
    }
    
  }
  # check ICD10-Orpha
  if (!is.null(env$medData$Orpha_Kode)) out <-checkOrphaCoding(refData2, cl)
  else out <- list (k1_rd_counter=0,k1_check_counter=0 )
  out
}

#' @title checkOrphaCoding
#' @description This function checks the plausibility of ICD-Orpha links.
#' @import stringi
#'
checkOrphaCoding<- function (refData2, cl) {
  k1_check_counter =0
  k1_rd_counter=0
  if(!is.empty(env$medData$ICD_Primaerkode)){
    iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode) & !is.empty(env$medData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
      oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
      code <-as.character(env$medData$Orpha_Kode[i])
      if (is.na(oCode) & !is.na(code) ) {
        k1_check_counter =k1_check_counter+1
        msg<- paste("ICD10-Orpha combination:" , iCode,"-", code ,  "is implausible according to Alpha-ID-SE.",  env$dq[,cl][i])
        env$dq[,cl][i] <- msg
        
      }
      else if (!(is.null(oCode) | is.na(code) | is.empty(oCode))){
        iRefList<- which(stri_trim(as.character(refData2$ICD_Primaerkode1))==iCode)
        if (!is.empty (iRefList)){
          oRefList <- ""
          k1_check_counter =k1_check_counter+1
          for (j in iRefList){
            oRefCode <-as.integer(refData2$Orpha_Kode[j])
            oRefList <- append( oRefList,oRefCode)
          }
          if ( !is.element(oCode, oRefList))
          {
            msg<- paste("ICD10-Orpha combination:" , iCode,"-", oCode ,  "is implausible according to Alpha-ID-SE.",  env$dq[,cl][i])
            env$dq[,cl][i] <- msg
          }
          else k1_rd_counter=k1_rd_counter+1
        }
        else{
          if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
            k1_check_counter =k1_check_counter+1
            oRef<- which(as.character (refData2$Orpha_Kode)==oCode)
            if (!is.empty ( oRef)){
              msg<- paste("ICD10-Orpha combination:" , iCode,"-", oCode ,  "is implausible according to Alpha-ID-SE.",  env$dq[,cl][i])
              env$dq[,cl][i] <- msg
            }
          }
        }
      }
    }
  }
  out <- list()
  out[["k1_rd_counter"]] <- k1_rd_counter
  out[["k1_check_counter"]] <- k1_check_counter
  out
}

#' @title checkOutlier
#' @description This function checks the loaded data for outliers.
#'
checkOutlier<-function (ddata, item, cl) {
  item.vec <- env$medData[[item]]
  index = which(ddata$basicItem==item)[1]
  if (!is.empty (env$ddata$engLabel)) name <- env$ddata$engLabel[index]
  else name<- item
  if(!is.empty(item.vec)){
    item.vec <-  as.Date(ISOdate(env$medData[[item]], 1, 1))
    out <- getDateOutlier(item.vec)
    if (!is.empty(out)) {
      ddata<- addOutlier (item, ddata, length(out), length(item.vec))
      for(i in out) {
        env$dq[,cl][i] <- paste( "Implausible", name , item.vec[i], "date in the future.")
      }
    }   else ddata <- addOutlier(item, ddata, 0,length(item.vec))
    
    if(item == "Geburtsdatum")
    {
      item1.vec <-  as.Date(ISOdate(env$medData[["Geburtsdatum"]], 1, 1))
      now<- as.Date(Sys.Date())
      if (!exists("ageMax")) ageMax=105
      out<-getAgeMaxOutlier(item1.vec,  now, ageMax)
      if (!is.empty(out)) {
        ddata<- addOutlier (item, ddata, length(out), length(item1.vec) )
        maxAge <-paste( "maximal age ", ageMax, ".", sep = "")
        for(i in out) env$dq[,cl][i] <- paste( "Implausible birthdate", item1.vec[i] , maxAge,  env$dq[,cl][i])
      }
    }
  }
  else if (item!="Total"){
    ddata <- addOutlier(item, ddata, 0,0)
  }
  ddata
}


#' @title addD2
#' @description This function adds DQ indicators and parameters for the plausibility dimension (D2).
#'
addD2<- function (tdata, link_pl, link) {
  if(link_pl>0 & link >0){
    tdata$link <- link
    tdata$link_ip<- link-link_pl
    tdata$dqi_pl_opr <- orphaPlausibilityIndicator(tdata$link, tdata$link_ip)$value
  }
  else {
    tdata$link <- 0
    tdata$link_ip<- 0
    tdata$dqi_pl_opr<-NA
  }
  tdata
}

#------------------------------------------------------------------------------------------------------
# functions for D3 uniqueness dimension
#------------------------------------------------------------------------------------------------------

#' @title rdCaseUnambiguityIndicator
#' @description This function calculates the RD Case unambiguity Rate (dqi_un_cur), a specific indicator for assessing the semantic uniqueness of RD diagnoses, and adds related metadata and DQ parameters.
#' @export

rdCaseUnambiguityIndicator <- function(rdCase, rdCase_amb) {
  ind <-data.frame(
    Abbreviation= "dqi_un_cur",
    Label = "RD Case unambiguity Rate",
    Dimension = "Uniquness",
    Short_Description = "This indicator assesses the semantic uniqueness of RD diagnoses in a given data set. More details are available in the publication DOI:10.1055/a-2006-1018."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(rdCase) & is.numeric(rdCase_amb)) {
    if (rdCase>0) ind$value<- round (((rdCase-rdCase_amb)/rdCase)*100,2)
    else ind$value<- NA
  } else {
    ind$value<- NA
    if (!is.numeric(rdCase)) link <-NA
    if (!is.numeric(rdCase_amb)) rdCase_amb <-NA
  }
  df <-data.frame(rdCase= c(rdCase), rdCase_amb= c(rdCase_amb), dqi_un_cur = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title rdCaseDissimilarityIndicator
#' @description This function calculates the RD Case Dissimilarity Rate(dqi_un_cdr), a specific indicator for assessing the syntactic uniqueness of RD diagnoses, and adds related metadata and DQ parameters.
#'@export
#'
rdCaseDissimilarityIndicator <- function(rdCase, rdCase_dup) {
  ind <-data.frame(
    Abbreviation= "dqi_un_cdr",
    Label = "RD Case unambiguity Rate",
    Dimension = "Uniquness",
    Short_Description = "This indicator assesses the syntactic uniqueness of RD diagnoses in a given data set. Further details and examples are available in the publication DOI:10.1055/a-2006-1018."
  )
  if (is.null(env$semantics)) env$semantics <- ind
  else env$semantics <-rbind(env$semantics,ind)
  if (is.numeric(rdCase) & is.numeric(rdCase_dup)) {
    if (rdCase>0) ind$value<- round (((rdCase-rdCase_dup)/rdCase)*100,2)
    else ind$value<- NA
  } else {
    ind$value<- NA
    if (!is.numeric(rdCase)) rdCase <-NA
    if (!is.numeric(rdCase_dup)) rdCase_dup <-NA
  }
  df <-data.frame(rdCase= c(rdCase), rdCase_dup= c(rdCase_dup), dqi_un_cdr = c(ind$value))
  if (is.null(env$report)) env$report <-df
  else env$report <-cbind(env$report,df)
  ind
}

#' @title checkD3
#' @description This function checks the quality of loaded data regarding the uniqueness dimension (D3).
#'
checkD3 <- function (refData1, refData2, cl){
  if (is.null(env$medData$ICD_Primaerkode)) out <-checkUniqueOrphaCoding(cl)
  else if (!is.null(env$medData$Orpha_Kode)) out <-checkUniqueIcdOrphaCoding(refData1, refData2, cl)
  else out <- checkUniqueIcd(refData1, cl)
  out
}

#' @title checkUniqueIcd
#' @description This function checks the uniqueness of RD cases diagnosed using ICD-10 codes.
#' @import stringi
#'
checkUniqueIcd <- function (refData1, cl){
  env$dq$rdCase <-NA
  env$dq$CheckedRdCase <- NA
  env$dq$unambiguous_rdCase <-NA
  env$dq$ambiguous_tracer <-NA
  env$dq$tracer <-NA
  eList <-refData1[which(refData1$Unique_SE=="yes"),]
  k3_check_counter =0
  k3_rd_counter=0
  rd_counter=0
  if(!is.empty(env$medData$ICD_Primaerkode)){
    iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
      if (is.element(iCode, stri_trim(as.character(eList$IcdCode))))
      {
        k3_rd_counter=k3_rd_counter+1
        k3_check_counter =k3_check_counter+1
        env$dq$CheckedRdCase[i] <- "yes"
        env$dq$unambiguous_rdCase[i] = "yes"
        env$dq$rdCase[i] = "yes"
        env$dq$tracer[i] <-"yes"
      }
      else {
        mList <-refData1[(which(refData1$Unique_SE=="no")),]
        iRefList<- which(stri_trim(as.character (mList$IcdCode))==iCode)
        if (!is.empty (iRefList)){
          env$dq$rdCase[i] <-"yes"
          env$dq$tracer[i] <-"yes"
          env$dq$ambiguous_tracer[i] <-"yes"
          msg<- paste("Ambiguous ICD10 Code",iCode, ". Missing Orpha Code.",  env$dq[,cl][i])
          env$dq[,cl][i] <- msg
          k3_check_counter =k3_check_counter+1
          env$dq$CheckedRdCase[i] <- "yes"
          
        }
      }
    }
  }
  
  rd <-env$dq[ which (env$dq$rdCase=="yes"),]
  aRd <-env$dq[ which(env$dq$unambiguous_rdCase=="yes"),]
  checkedRd <-env$dq[ which (env$dq$CheckedRdCase=="yes"),]
  tracer <-env$dq[ which (env$dq$tracer=="yes"),]
  env$tdata$tracerCase_no <- length (unique(tracer$Aufnahmenummer))
  ambigTracer <-env$dq[ which (env$dq$ambiguous_tracer=="yes"),]
  env$tdata$ambiguous_tracerCase_no <- length (unique(ambigTracer$Aufnahmenummer))
  out <- list()
  out[["k3_unambiguous_rdDiag_no"]] <- length(aRd$Aufnahmenummer)
  out[["k3_unambiguous_rdCase_no"]] <- length (unique(aRd$Aufnahmenummer))
  out[["k3_checkedRdCase_no"]] <-  length (unique(checkedRd$Aufnahmenummer))
  out
}

#' @title checkUniqueOrphaCoding
#' @description This function checks the uniqueness of RD cases diagnosed with Orphacodes.
#'
checkUniqueOrphaCoding <- function (cl){
  oList <-which(env$medData$Orpha_Kode !="" & !is.na(env$medData$Orpha_Kode)  & !is.empty(env$medData$Orpha_Kode)& !is.null(env$medData$Orpha_Kode))
  for (i in oList)
  {
    code <-env$medData$Orpha_Kode[i]
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    if (!is.na(oCode)) {
      env$dq$CheckedRdCase[i] <- "yes"
      env$dq$unambiguous_rdCase[i] = "yes"
      env$dq$rdCase[i] = "yes"
    }
    else env$dq[,cl][i] <- paste("Ambiguous Case.",env$dq[,cl][i] )
    
  }
  
  out <- list()
  rd <-env$dq[ which (env$dq$rdCase=="yes"),]
  aRd <-env$dq[ which (env$dq$unambiguous_rdCase=="yes"),]
  checkedRd <-env$dq[ which (env$dq$CheckedRdCase=="yes"),]
  out <- list()
  out[["k3_unambiguous_rdDiag_no"]] <- length(aRd$Aufnahmenummer)
  out[["k3_unambiguous_rdCase_no"]] <- length (unique(aRd$Aufnahmenummer))
  out[["k3_checkedRdCase_no"]] <-  length (unique(checkedRd$Aufnahmenummer))
  
  out
}

#' @title checkUniqueIcdOrphaCoding
#' @description This function checks the uniqueness of RD cases coded with ICD-Orpha links.
#' @import stringi
#'
checkUniqueIcdOrphaCoding <- function (refData1, refData2, cl){
  env$dq$rdCase <-NA
  env$dq$CheckedRdCase <- NA
  env$dq$unambiguous_rdCase <-NA
  env$dq$ambiguous_tracer <-NA
  k3_check_counter =0
  k3_rd_counter=0
  
  if(!is.empty(env$medData$ICD_Primaerkode)){
    cq <- which(env$medData$ICD_Primaerkode=="" | is.na(env$medData$ICD_Primaerkode) | is.empty(env$medData$ICD_Primaerkode))
    if (!is.empty (cq)) for(i in cq) {
      env$dq[,cl][i]<- paste("Missing ICD-Code. ", env$dq[,cl][i])
      code <- env$medData$Orpha_Kode[i]
      if (! (is.na(code) || is.null(code) || is.empty(code))){
        oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
        if (!is.na(oCode)) {
          k3_rd_counter=k3_rd_counter+1
          env$dq$rdCase[i] <- "yes"
          env$dq$unambiguous_rdCase [i] = "yes"
          k3_check_counter =k3_check_counter+1
          env$dq$CheckedRdCase[i] <- "yes"
        }
        else{
          env$dq[,cl][i] <- paste("Ambiguous Case.",env$dq[,cl][i] )
        }
      }
    }
    iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
      numIcd <-as.numeric(iCode)
      if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
        if ( !is.na(numIcd))
        {
          msg<- paste("Invalid ICD code.",  numIcd, env$dq[,cl][i])
          env$dq[,cl][i] <- msg
        }
        else {
          oCode <-env$medData$Orpha_Kode[i]
          numCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
          if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode))){
            if ( is.na(numCode))
            {
              msg<- paste("Ambiguous Orphacoding.",  env$dq[,cl][i])
              env$dq[,cl][i] <- msg
              
            }
            else {
              
              iRefList<- which(stri_trim(as.character(refData2$ICD_Primaerkode1))==iCode)
              if (!is.empty (iRefList)){
                oRefList <- ""
                k3_check_counter =k3_check_counter+1
                env$dq$CheckedRdCase[i] <- "yes"
                
                for (j in iRefList){
                  oRefCode <-as.integer(refData2$Orpha_Kode[j])
                  oRefList <- append( oRefList,oRefCode)
                }
                if ( !is.element(numCode, oRefList))
                {
                  msg<- paste("Ambiguous Orphacoding.",  env$dq[,cl][i])
                  env$dq[,cl][i] <- msg
                }
                else { k3_rd_counter=k3_rd_counter+1
                env$dq$rdCase[i] <- "yes"
                env$dq$unambiguous_rdCase [i] = "yes"
                }
              }
              else{
                if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
                  k3_check_counter =k3_check_counter+1
                  env$dq$CheckedRdCase[i] <- "yes"
                  oRef<- which(as.numeric(as.character(refData2$Orpha_Kode))==numCode)
                  if (!is.empty ( oRef)){
                    msg<- paste("Ambiguous Coding.",  env$dq[,cl][i])
                    env$dq[,cl][i] <- msg
                  }
                }
              }
              
            }
            
          }
          else{
            eList <-refData1[(which(refData1$Unique_SE=="yes")),]
            if (is.element(iCode, stri_trim(as.character(eList$IcdCode))))
            {
              k3_rd_counter=k3_rd_counter+1
              k3_check_counter =k3_check_counter+1
              env$dq$CheckedRdCase[i] <- "yes"
              env$dq$rdCase[i] = "yes"
              env$dq$unambiguous_rdCase [i] = "yes"
            }
            else {
              mList <-refData1[(which(refData1$Unique_SE=="no")),]
              iRefList<- which(stri_trim(as.character (mList$IcdCode))==iCode)
              if (!is.empty (iRefList)){
                env$dq$rdCase[i] = "yes"
                k3_check_counter =k3_check_counter+1
                env$dq$CheckedRdCase[i] <- "yes"
                env$dq$ambiguous_tracer[i] <-"yes"
                msg<- paste("Ambiguous ICD10 Code",iCode, ".",  env$dq[,cl][i])
                env$dq[,cl][i] <- msg
              }
            }
          }
          
        }
      }
      
    }
  }
  
  rd <-env$dq[ which (env$dq$rdCase=="yes"),]
  aRd <-env$dq[ which (env$dq$unambiguous_rdCase=="yes"),]
  checkedRd <-env$dq[ which (env$dq$CheckedRdCase=="yes"),]
  ambigTracer <-env$dq[ which (env$dq$ambiguous_tracer=="yes"),]
  env$tdata$ambiguous_tracerCase_no <- length (unique(ambigTracer$Aufnahmenummer))
  out <- list()
  out[["k3_unambiguous_rdDiag_no"]] <- length(aRd$Aufnahmenummer)
  out[["k3_unambiguous_rdCase_no"]] <- length(unique(aRd$Aufnahmenummer))
  out[["k3_checkedRdCase_no"]] <-  length(unique(checkedRd$Aufnahmenummer))
  out
  
}


#' @title addD3
#' @description This function adds DQ indicators and parameters for uniqueness dimension (D3).
#'
addD3<- function (tdata, rdDiag_unamb, rdCase_unamb, rdCase) {
  if(rdCase >0){
    tdata$rdCase <- rdCase
    tdata$rdCase_amb <-rdCase-rdCase_unamb
    tdata$rdDiagnosis_unamb<-rdDiag_unamb
    tdata$dqi_un_cur <- rdCaseUnambiguityIndicator(tdata$rdCase, tdata$rdCase_amb)$value
    tdata$dqi_un_cdr <- rdCaseDissimilarityIndicator(tdata$rdCase, tdata$rdCase_dup)$value
  }
  else {
    tdata$rdCase <- 0
    tdata$dqi_un_cur<- 0
    tdata$dqi_un_cdr <- NA
    tdata$rdCase_amb <- NA
    tdata$rdDiagnosis_unamb<- 0
  }
  tdata
}

#------------------------------------------------------------------------------------------------------
# functions for concordance dimension (D4)
#------------------------------------------------------------------------------------------------------

#' @title checkD4
#' @description This function checks the quality of loaded data regarding the concordance dimension (D4).
#'
checkD4 <- function (cl) {
  iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
  k4_counter_icd= length(iList)
  if (!is.null(env$medData$Orpha_Kode)){
    k4_counter_orpha = getOrphaCodeNo (cl)
    k4_counter_orphaCase =getOrphaCaseNo(cl)
  }
  else { k4_counter_orpha=0
  k4_counter_orphaCase =0
  }
  out <- list()
  out[["k4_counter_icd"]] <- k4_counter_icd
  out[["k4_counter_orpha"]] <- k4_counter_orpha
  out[["k4_counter_orphaCase"]] <- k4_counter_orphaCase
  out
}

#' @title getOrphaCaseNo
#' @description This function calculates the number of Orpha cases.
#'
getOrphaCaseNo<- function (cl){
  env$dq$orphaCase <- NA
  orphaCaseNo =0
  oList <-which(env$medData$Orpha_Kode !="" & !is.na(env$medData$Orpha_Kode)  & !is.empty(env$medData$Orpha_Kode)& !is.null(env$medData$Orpha_Kode))
  for (i in oList)
  {
    code <-env$medData$Orpha_Kode[i]
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    if (!is.na(oCode)) env$dq$orphaCase[i] = "yes"
    
  }
  oc <-env$dq[ which (env$dq$orphaCase=="yes"),]
  orphaCaseNo <- length (unique(oc$Aufnahmenummer))
  orphaCaseNo
}

#' @title getOrphaCodeNo
#' @description This function calculates the number of Orphacodes.
#'
getOrphaCodeNo <- function (cl) {
  k4_counter_orpha =0
  oList <-which(env$medData$Orpha_Kode !="" & !is.na(env$medData$Orpha_Kode)  & !is.empty(env$medData$Orpha_Kode) & !is.null(env$medData$Orpha_Kode))
  if (!is.empty (oList)) for(i in oList) {
    code <-env$medData$Orpha_Kode[i]
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    if (!is.na(oCode)) k4_counter_orpha = k4_counter_orpha +1
    else env$dq[,cl][i] <- paste("Invalid Orpha code.",code, env$dq[,cl][i] )
  }
  k4_counter_orpha
}

#' @title addD4
#' @description This function adds DQ metrics for the concordance dimension (D4).
#'
addD4<- function (tdata,orpha,orphaCase, uRd, inPtCase) {
  if (! (is.empty(tdata$report_year) | is.na(tdata$report_year)))
  {
    tdata$case_no_py_ipat <-inPtCase
    tdata$orphaCoding_no_py <- orpha
    rd <- (tdata$rdCase_no_py/inPtCase) * 100000
    tdata$rdCase_rel_py_ipat  <-  round (rd,0)
    tracer <- (tdata$tracerCase_no_py/inPtCase) * 100000
    tdata$tracerCase_rel_py_ipat  <-  round (tracer,0)
    
    if(orphaCase>0){
      tdata$orphaCase_no_py <-orphaCase
      or <- ( orphaCase/inPtCase) * 100000
      tdata$orphaCase_rel_py_ipat   <- round (or,0)
      
    }
    else {
      tdata$orphaCase_rel_py_ipat  <- 0
      tdata$orphaCase_no_py <- 0
    }
    if(uRd>0){
      tdata$unambiguous_rdCase_no_py <-uRd
      rf <- ( uRd/inPtCase) * 100
      tdata$unambiguous_rdCase_rel_py_ipat <- round (rf,2)
    }
    else {
      tdata$unambiguous_rdCase_no_py <- 0
      tdata$unambiguous_rdCase_rel_py_ipat <- 0
    }
  }
  
  tdata
}

#' @title getConcWithRefValues
#' @description This function evaluates the concordance of tracer cases with reference values from the literature of national references.
#'
getConcWithRefValues <- function(tracerCase_rel_py_ipat, concRef){
  conc =NA
  if (is.integer(tracerCase_rel_py_ipat) | is.double(tracerCase_rel_py_ipat))
  {
    if (concRef[["min"]] <= tracerCase_rel_py_ipat && tracerCase_rel_py_ipat<=concRef[["max"]] ) conc=1
    else conc =0
  }
  
  conc
}

#' @title getConcIndicator
#' @description This function calculates the z-score value to measure concordance indicators such as the concordance of RD cases or the concordance of tracer cases.
#' @import stats
#'
getConcIndicator <- function(dist, index){
  concInd <-round (((dist[index]- mean(dist))/sd(dist)),2)
  concInd
}

#' @title setAnnualVars
#' @description Function to set annual parameters.
#'
setAnnualVars<- function (tdata) {
  if (! (is.empty(tdata$report_year) | is.na(tdata$report_year)))
  {
    tdata$rdCase_no_py <- tdata$rdCase
    tdata$orphaMissing_no_py <- tdata$oc_misg
    tdata$implausible_codeLink_no_py <- tdata$link_ip
    tdata$duplicateCase_no_py <-tdata$case_dup
    tdata$duplicateRdCase_no_py <-tdata$rdCase_dup
    tdata$ambiguous_rdCase_no_py <- tdata$rdCase_amb
    tdata$case_no_py <- tdata$case_no
    tdata$patient_no_py <-tdata$patient_no
    tdata$tracerCase_no_py <- tdata$tracerCase_no
  }
  
  tdata
}