#######################################################################################################
# Description: The data quality library (dqLib) is an R package for explainable and traceable assessments of clinical data quality.
# As part of the "dqLib" package this script includes functions for calculating specific metrics and reporting on detected data quality (DQ) issues, especially in the field of Rare Diseases(RDs)
# Date Created: 2021-02-26
# Kais Tahar, University Medical Center Göttingen
# ######################################################################################################

#' @title rdDqChecker
#' @description This function checks the quality of loaded data regarding DQ issues that may arise in context of rare diseases (RDs).
#' The default DQ dimensions are completeness and plausibility.
#' @export
rdDqChecker <- function (tracerRef, rdStandard) {
  # Call global variables
  # Relevant meta data for RDs
  catMeta=base::get("catMeta", envir=env)
  tempMeta=base::get("tempMeta", envir=env)
  metaCol=base::get("metaCol", envir=env)
  optMeta=base::get("optMeta", envir=env)
  ovrQuality=base::get("ovrQuality", envir=env)
  # Labels for potential DQ issues
  im_misg_lbl= base::get("im_misg_lbl", envir=env)
  vm_misg_lbl=base::get("vm_misg_lbl", envir=env)
  dq_lbl =base::get("dq_lbl", envir=env)
  #D1 completeness
  if (!(is.empty(catMeta) | is.empty(tempMeta))) {
      im <-getMandatoryItems(metaCol, c(optMeta,ovrQuality), catMeta, tempMeta)
      # Add results of the completeness analysis for all data items in the meta data for RDs
      base::assign(x ="mItem" , value= getMissingItem(im), envir=env)
      base::assign(x="catMeta", value = getMissingValue(catMeta, metaCol, vm_misg_lbl, im_misg_lbl), envir=env)
      base::assign(x="tempMeta", value = getMissingValue(tempMeta, metaCol, vm_misg_lbl, im_misg_lbl), envir=env)
  } else  stop(" The global Environment (env) does not contain any categorical or temporal data items.
  Please ensure the data type of the loaded data items is set correctly and then rerun the execution (see global variables catMeta and tempMeta).")
  param <- checkOrphaCodingCompleteness(tracerRef, dq_lbl)
  #D2 plausibility
  base::assign(x="tempMeta", value = checkTemporalPlausibility(env$tempMeta, metaCol, dq_lbl), envir=env)
  param <- cbind (checkOrphaCodingPlausibility(rdStandard, dq_lbl), param)
  param<-cbind(getTotalStatistic(metaCol,ovrQuality), param)
  param
}

#' @title checkCordDQ
#' @description This function checks the quality of loaded data regarding selected DQ metrics for the CORD use case.
#' The default DQ dimensions are completeness, plausibility, uniqueness and concordance.
#' @import stringi
#' @export
#'
checkCordDQ <- function (instID, reportYear, inpatientCases, refData1, refData2, dqInd, repCol, cl, itemCol, totalRow, oItem,...) {
  vars <- list(...)
  if (is.null (cl)) stop("No report design available")
  if (is.null (env$studyData)) stop("No data available")
  if (is.null(env$studyData$ICD_Primaerkode)) stop("Missing mandatory item: ICD_Primaerkode")
  if (is.null(env$studyData$Orpha_Kode)) env$studyData$Orpha_Kode <-NA
  env$ovrQuality <- totalRow
  env$dq_lbl = cl
  env$subjIdentifier= "PatientIdentifikator"
  if (!is.null(instID)){
    instData<- env$studyData[which(env$studyData$Institut_ID==instID),]
    if (nrow(instData)>0) env$studyData <- instData
  }
  rdDup_no =0
  inputData <-env$studyData
  row_no = nrow(inputData)
  eList <-refData1[which(refData1$Unique_SE=="yes"),]

  if(!is.empty(env$studyData$PatientIdentifikator) & !is.empty(env$studyData$Aufnahmenummer) & !is.empty(env$studyData$ICD_Primaerkode) & !is.empty(env$studyData$Orpha_Kode))
  {
    env$studyData<-env$studyData[!duplicated(env$studyData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode","Orpha_Kode")]),]
    env$dq <- subset(env$studyData, select = repCol)
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
    } else rdDup_no =0
  }
  else if(!is.empty(env$studyData$PatientIdentifikator) & !is.empty(env$studyData$Aufnahmenummer) & !is.empty(env$studyData$ICD_Primaerkode))
  {
    env$studyData<-env$studyData[!duplicated(env$studyData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode")]),]
    env$dq <- subset(env$studyData, select = repCol)
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
  #D1 completeness and D2 plausibility
  metrics <- dqChecker(env$studyData, "RD", NULL, env$catMeta, env$tempMeta, itemCol, oItem, c("", "NULL", NA), refData1, refData2)
  if(!is.empty(env$studyData$PatientIdentifikator)) patient_no = length (unique(env$studyData$PatientIdentifikator))
  if(!is.empty(env$studyData$Aufnahmenummer)) case_no = length (env$studyData$Aufnahmenummer[which(!duplicated(env$studyData$Aufnahmenummer)& ! is.na(env$studyData$Aufnahmenummer))])
  if(!is.empty(vars)) caseItems <- vars[[1]]
  else caseItems <- NULL
  oc <-checkOrphaCodingCompleteness(refData1, cl)
  dqReport<-cbind(oc,patient_no, case_no, env$metrics, metrics$parameters, metrics$indicators)
  if (!is.null(instID))dqReport$inst_id <- instID else dqReport$inst_id <- "ID fehlt"
  itemVec <- names (env$studyData)
  inter <- intersect (env$im, itemVec)
  dqReport <- cbind(checkSubjCompleteness(env$subjIdentifier, inter), dqReport)
  if(!is.null(caseItems)) dqReport <- cbind(checkCaseCompleteness(caseItems, itemCol), dqReport)
  dqReport$dqi_co_scr <- subjectCompletenessIndicator(dqReport$s, dqReport$s_inc)$value
  dqReport$dqi_co_ccr <- caseCompletenessIndicator(dqReport$vm_case, dqReport$vm_case_misg)$value
  dqReport$dqi_co_ocr <- orphaCompletenessIndicator(dqReport$icd_tracer, dqReport$oc_misg)$value
  dqReport$dqi_pl_opr <- orphaPlausibilityIndicator(dqReport$link, dqReport$link_ip)$value
  #D3 uniqueness
  dqReport$case_dup = row_no - nrow(env$studyData)
  dqReport$rdCase_dup = rdDup_no
  dqReport <- cbind (checkSemanticUniqueness(refData1, refData2, cl), dqReport)
  dqReport$dqi_un_cur <- rdCaseUnambiguityIndicator(dqReport$rdCase, dqReport$rdCase_amb)$value
  dqReport$dqi_un_cdr <- rdCaseDissimilarityIndicator(dqReport$rdCase, dqReport$rdCase_dup)$value
  #D4 concordance
  dqReport <- setAnnualVars(reportYear, dqReport)
  dqReport <- cbind (rdConcordanceMetrics(reportYear, inpatientCases, dqReport$tracerCase_no_py, dqReport$rdCase_no_py, env$studyData$Orpha_Kode, cl), dqReport)
  if(!is.empty(vars) & length(vars)>=2) concRef <- vars[[2]]
  else concRef <- NULL
  if(!is.null(concRef)) dqReport$conc_with_refValues<-getConcWithRefValues(dqReport$tracerCase_rel, concRef)
  orphaCases <- env$dq[which(env$dq$orphaCase=="yes"),]
  rdCases <- env$dq[which (env$dq$CheckedRdCase=="yes"),]
  dqReport$orphaPatient_no_py = length(unique(orphaCases$PatientIdentifikator))
  dqReport$rdPatient_no_py = length(unique(rdCases$PatientIdentifikator))
  env$report <- deprecatedMetrics(dqReport)
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
  if(!cl %in% colnames(env$dq)) env$dq[,cl]<-""
  refData <-refData[which(refData$Complete=="yes"),]
  env$catMeta <- addMissingValue("Orpha_Kode",env$catMeta, 0,0)
  env$catMeta <- addMissingValue("AlphaID_Kode",env$catMeta, 0,0)
  if (!is.null(env$studyData$ICD_Primaerkode))
  {
    iList <-which(env$studyData$ICD_Primaerkode !="" & !is.na(env$studyData$ICD_Primaerkode)  & !is.empty(env$studyData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$studyData$ICD_Primaerkode[i]))
      rdRefList<- which(stri_trim(as.character(refData$IcdCode))==iCode)
      if (!is.empty(rdRefList)) {
        env$dq$tracer[i] <-"yes"
        if("Orpha_Kode" %in% colnames(env$studyData)){
          code <-as.character(env$studyData$Orpha_Kode[i])
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
        if("AlphaID_Kode" %in% colnames(env$studyData)){
          aCode <-as.character(env$studyData$AlphaID_Kode[i])
          if (is.na(aCode) | is.empty(aCode)) {
            env$dq[,cl][i] <- paste("Missing AlphaID Code. ", env$dq[,cl][i])
            missing_counter2 =missing_counter2 +1
          }
        }
      }
    }
  }
  else {
    oList <-which(env$studyData$Orpha_Kode !="" & !is.na(env$studyData$Orpha_Kode)  & !is.empty(env$studyData$Orpha_Kode))
    k2_orpha_no = length(oList)
    k2_orphaCheck_no = length(env$studyData$Orpha_Kode)
    missing_counter1 = k2_orphaCheck_no - k2_orpha_no
    aList <-which(env$studyData$AlphaID_Kode !="" & !is.na(env$studyData$AlphaID_Kode)  & !is.empty(env$studyData$AlphaID_Kode))
    k2_alpha_no = length(aList)
    k2_checkAlpha_no = length(env$studyData$AlphaID_Kode)
    missing_counter2 = k2_checkAlpha_no -k2_alpha_no
  }
  tracer <-env$dq[ which (env$dq$tracer=="yes"),]
  env$metrics$tracerCase_no <- length (unique(tracer$Aufnahmenummer))
  env$catMeta <- addMissingValue("Orpha_Kode", env$catMeta, missing_counter1,k2_orphaCheck_no )
  env$catMeta <- addMissingValue("AlphaID_Kode", env$catMeta, missing_counter2 ,k2_orphaCheck_no )
  out <- data.frame(icd_tracer=k2_orphaCheck_no, oc_misg =k2_orphaCheck_no-k2_orpha_no)
  out
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

#' @title checkOrphaCodingPlausibility
#' @description This function checks the plausibility of ICD-Orpha links.
#' @import stringi
#'
checkOrphaCodingPlausibility<- function (refData2, cl) {
  link_counter =0
  link_ip_counter =0
  k1_rd_counter=0
  if(!cl %in% colnames(env$dq)) env$dq[,cl]<-""
  if(!is.empty(env$studyData$ICD_Primaerkode)){
    iList <-which(env$studyData$ICD_Primaerkode !="" & !is.na(env$studyData$ICD_Primaerkode) & !is.empty(env$studyData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$studyData$ICD_Primaerkode[i]))
      oCode <-as.numeric(as.character(env$studyData$Orpha_Kode[i]))
      code <-as.character(env$studyData$Orpha_Kode[i])
      if (is.na(oCode) & !is.na(code) ) {
        link_counter =link_counter+1
        link_ip_counter =  link_ip_counter+1
        msg<- paste("ICD10-Orpha combination:" , iCode,"-", code ,  "is implausible according to Alpha-ID-SE.",  env$dq[,cl][i])
        env$dq[,cl][i] <- msg

      }
      else if (!(is.null(oCode) | is.na(code) | is.empty(oCode))){
        iRefList<- which(stri_trim(as.character(refData2$ICD_Primaerkode1))==iCode)
        if (!is.empty (iRefList)){
          oRefList <- ""
          link_counter =link_counter+1
          for (j in iRefList){
            oRefCode <-as.integer(refData2$Orpha_Kode[j])
            oRefList <- append( oRefList,oRefCode)
          }
          if ( !is.element(oCode, oRefList))
          {
            link_ip_counter =  link_ip_counter+1
            msg<- paste("ICD10-Orpha combination:" , iCode,"-", oCode ,  "is implausible according to Alpha-ID-SE.",  env$dq[,cl][i])
            env$dq[,cl][i] <- msg
          }
          else k1_rd_counter=k1_rd_counter+1
        }
        else{
          if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
            link_counter =link_counter+1
            oRef<- which(as.character (refData2$Orpha_Kode)==oCode)
            if (!is.empty ( oRef)){
              link_ip_counter =  link_ip_counter+1
              msg<- paste("ICD10-Orpha combination:" , iCode,"-", oCode ,  "is implausible according to Alpha-ID-SE.",  env$dq[,cl][i])
              env$dq[,cl][i] <- msg
            }
          }
        }
      }
    }
  }
  out <- data.frame(link =link_counter, link_ip =link_ip_counter, vs_cd =link_counter*2, vc =link_ip_counter *2)
  out
}

#' @title checkTemporalPlausibility
#' @description This function checks the plausibility of date values in a given data set.
#'
checkTemporalPlausibility<-function (dataset, itemCol, cl) {
  if (!is.null(dataset))
  {
    dItem <- dataset[, itemCol]
    if (!is.empty(dItem)) {
      for (item in unique(dItem)) {
        dataset  <-getImplausibleDateValues(dataset, item, cl)
      }
    }
  }
  dataset
}

#' @title checkDatePlausibility
#' @description This function identifies and returns the implausible date values detected in a given data vector.
#'
getImplausibleDateValues<-function (dataset, item, cl) {
  item.vec <- env$studyData[[item]]
  index = which(dataset$basicItem==item)[1]
  if (!is.empty (env$dataset$engLabel)) name <- env$dataset$engLabel[index]
  else name<- item
  if(!is.empty(item.vec)){
    item.vec <-  as.Date(ISOdate(env$studyData[[item]], 1, 1))
    out <- checkFutureDate(item.vec)
    if (!is.empty(out)) {
      dataset<- addOutlier (item, dataset, length(out), length(item.vec))
      for(i in out) {
        env$dq[,cl][i] <- paste( "Implausible", name , item.vec[i], "date in the future.")
      }
    }   else dataset <- addOutlier(item, dataset, 0,length(item.vec))

    if(item == "Geburtsdatum")
    {
      item1.vec <-  as.Date(ISOdate(env$studyData[["Geburtsdatum"]], 1, 1))
      now<- as.Date(Sys.Date())
      if (!exists("ageMax")) ageMax=105
      out<-checkAgePlausibility(item1.vec,  now, ageMax)
      if (!is.empty(out)) {
        dataset<- addOutlier (item, dataset, length(out), length(item1.vec) )
        maxAge <-paste( "maximal age ", ageMax, ".", sep = "")
        for(i in out) env$dq[,cl][i] <- paste( "Implausible birthdate", item1.vec[i] , maxAge,  env$dq[,cl][i])
      }
    }
  }
  else if (item!="Total"){
    dataset <- addOutlier(item, dataset, 0,0)
  }
  dataset
}

#------------------------------------------------------------------------------------------------------
# functions for D3 uniqueness dimension
#------------------------------------------------------------------------------------------------------

#' @title rdCaseUnambiguityIndicator
#' @description This function calculates the RD Case unambiguity Rate (dqi_un_cur), a specific indicator for assessing the semantic uniqueness of RD diagnoses, and adds related metadata and DQ parameters.
#' @export
#'
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
#' @description This function calculates the RD Case Dissimilarity Rate(dqi_un_cdr), a specific indicator for assessing the syntactic uniqueness of RD diagnoses, and provides related metadata and DQ parameters.
#' @export
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

#' @title checkSemanticUniqueness
#' @description This function checks the quality of loaded data regarding the semantic uniqueness.
#'
checkSemanticUniqueness <- function (refData1, refData2, cl){
  if (is.null(env$studyData$ICD_Primaerkode)) param <-checkUniqueOrphaCoding(cl)
  else if (!is.null(env$studyData$Orpha_Kode)) param <-checkUniqueIcdOrphaCoding(refData1, refData2, cl)
  else param <- checkUniqueIcd(refData1, cl)
  df <- data.frame(rdCase=param$k3_checkedRdCase_no, rdCase_amb=param$k3_checkedRdCase_no-param$k3_unambiguous_rdCase_no)
  df
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
  if(!is.empty(env$studyData$ICD_Primaerkode)){
    iList <-which(env$studyData$ICD_Primaerkode !="" & !is.na(env$studyData$ICD_Primaerkode)  & !is.empty(env$studyData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$studyData$ICD_Primaerkode[i]))
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
  env$metrics$tracerCase_no <- length (unique(tracer$Aufnahmenummer))
  ambigTracer <-env$dq[ which (env$dq$ambiguous_tracer=="yes"),]
  env$metrics$ambiguous_tracerCase_no <- length (unique(ambigTracer$Aufnahmenummer))
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
  oList <-which(env$studyData$Orpha_Kode !="" & !is.na(env$studyData$Orpha_Kode)  & !is.empty(env$studyData$Orpha_Kode)& !is.null(env$studyData$Orpha_Kode))
  for (i in oList)
  {
    code <-env$studyData$Orpha_Kode[i]
    oCode <-as.numeric(as.character(env$studyData$Orpha_Kode[i]))
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
  if(!is.empty(env$studyData$ICD_Primaerkode)){
    cq <- which(env$studyData$ICD_Primaerkode=="" | is.na(env$studyData$ICD_Primaerkode) | is.empty(env$studyData$ICD_Primaerkode))
    if (!is.empty (cq)) for(i in cq) {
      env$dq[,cl][i]<- paste("Missing ICD-Code. ", env$dq[,cl][i])
      code <- env$studyData$Orpha_Kode[i]
      if (! (is.na(code) || is.null(code) || is.empty(code))){
        oCode <-as.numeric(as.character(env$studyData$Orpha_Kode[i]))
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
    iList <-which(env$studyData$ICD_Primaerkode !="" & !is.na(env$studyData$ICD_Primaerkode)  & !is.empty(env$studyData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$studyData$ICD_Primaerkode[i]))
      numIcd <-as.numeric(iCode)
      if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
        if ( !is.na(numIcd))
        {
          msg<- paste("Invalid ICD code.",  numIcd, env$dq[,cl][i])
          env$dq[,cl][i] <- msg
        }
        else {
          oCode <-env$studyData$Orpha_Kode[i]
          numCode <-as.numeric(as.character(env$studyData$Orpha_Kode[i]))
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
  env$metrics$ambiguous_tracerCase_no <- length (unique(ambigTracer$Aufnahmenummer))
  out <- list()
  out[["k3_unambiguous_rdDiag_no"]] <- length(aRd$Aufnahmenummer)
  out[["k3_unambiguous_rdCase_no"]] <- length(unique(aRd$Aufnahmenummer))
  out[["k3_checkedRdCase_no"]] <-  length(unique(checkedRd$Aufnahmenummer))
  out
}


#------------------------------------------------------------------------------------------------------
# functions for concordance dimension (D4)
#------------------------------------------------------------------------------------------------------

#' @title getOrphaCaseNo
#' @description This function calculates the number of Orpha cases.
#'
getOrphaCaseNo<- function (orphaVec, cl){
  env$dq$orphaCase <- NA
  orphaCaseNo =0
  oList <-which(orphaVec !="" & !is.na(orphaVec)  & !is.empty(orphaVec)& !is.null(orphaVec))
  for (i in oList)
  {
    code <-orphaVec[i]
    oCode <-as.numeric(as.character(orphaVec[i]))
    if (!is.na(oCode)) env$dq$orphaCase[i] = "yes"

  }
  oc <-env$dq[ which (env$dq$orphaCase=="yes"),]
  orphaCaseNo <- length (unique(oc$Aufnahmenummer))
  orphaCaseNo
}

#' @title getOrphaCodeNo
#' @description This function calculates the number of Orphacodes.
#'
getOrphaCodeNo <- function (orphaVec, cl) {
  k4_counter_orpha =0
  oList <-which(orphaVec !="" & !is.na(orphaVec)& !is.empty(orphaVec) & !is.null(orphaVec))
  if (!is.empty (oList)) for(i in oList) {
    code <-orphaVec[i]
    oCode <-as.numeric(as.character(orphaVec[i]))
    if (!is.na(oCode)) k4_counter_orpha = k4_counter_orpha +1
    else env$dq[,cl][i] <- paste("Invalid Orpha code.",code, env$dq[,cl][i] )
  }
  k4_counter_orpha
}

#' @title rdConcordanceMetrics
#' @description This function provides normalized RD-specific metrics for the concordance dimension (D4).
#' @export
#'
rdConcordanceMetrics<- function (year, ipat, tracerCase, rdCase, orphaVec, cl) {
  metrics <-data.frame(report_year = c(year), tracerCase_no_py = c(tracerCase), rdCase_no_py = c(rdCase), case_no_py_ipat = c(ipat))
  if (!(is.empty(metrics$report_year) | is.na(metrics$report_year)))
  {
    if(rdCase>0 & ipat >0) metrics$rdCase_rel <- round (((rdCase  * 100000)/ipat), 0)
    else metrics$rdCase_rel=0
    if (tracerCase >0 & ipat >0) metrics$tracerCase_rel  <-  round (((tracerCase * 100000)/ipat),0)
    else metrics$tracerCase_rel=0
    metrics$orphaCoding_no_py <- getOrphaCodeNo(orphaVec, cl)
    metrics$orphaCase_no_py <- getOrphaCaseNo(orphaVec, cl)
    if( metrics$orphaCase_no_py>0 & ipat >0) metrics$orphaCase_rel <- round (((metrics$orphaCase_no_py* 100000)/ipat),0)
    else metrics$orphaCase_rel=0
  }
  metrics
}

#' @title getConcWithRefValues
#' @description This function evaluates the concordance of tracer cases with reference values from the literature of national references.
#' @export
#'
getConcWithRefValues <- function(tracerCase_rel, concRef){
  conc =NA
  if (is.integer(tracerCase_rel) | is.double(tracerCase_rel))
  {
    if (concRef[["min"]] <= tracerCase_rel && tracerCase_rel<=concRef[["max"]] ) conc=1
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
setAnnualVars<- function (year, tdata) {
  if (! (is.empty(year) | is.na(year)))
  {
    tdata$report_year <- year
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
