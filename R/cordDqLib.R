####################################################################################################### 
# Kais Tahar
# this script provides functions for data quality analysis in CORD
#######################################################################################################

#------------------------------------------------------------------------------------------------------
# functions to generate DQ reports
#------------------------------------------------------------------------------------------------------
getReport <- function (repCol, cl, td, path) {
  repCol = append (repCol, cl)
  repData <-subset(env$dq, select= repCol)
  dfq <-repData[ which(env$dq[,cl]!="")  ,]
  dfq[nrow(dfq)+1,] <- NA
  dfq[nrow(dfq)+1,1] <- env$mItem
  sheets <- list("DQ_Report"=dfq, "Statistik" = td)
  write_xlsx(sheets, paste (path,".xlsx", sep =""))
  write.csv(td, paste (path,".csv", sep =""), row.names = FALSE)
 # env <-NULL
}

getExtendedReport <- function ( repCol,cl, td, useCase, path) {
  repData <-subset(env$dq,select= repCol)
  dfq <-repData[ which(env$dq[,cl]!="")  ,]
  sheets <- list("DQ_Report"=dfq, "Statistik"= td, "Projectathon"=useCase)
  write_xlsx(sheets, path)
}

#------------------------------------------------------------------------------------------------------
# functions for DQ analysis
#------------------------------------------------------------------------------------------------------
checkCordDQ <- function ( instID, reportYear, inpatientCases, refData1, refData2, dqInd, cl, bItemCl, totalRow, oItem) {
  env$tdata$report_year <-reportYear
  if (is.null(oItem)) mv <-totalRow
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
  row_no = nrow(env$medData)
  input <-env$medData
  if(!is.empty(env$medData$PatientIdentifikator)) env$tdata$patient_no = length (unique(env$medData$PatientIdentifikator))
  if(!is.empty(env$medData$Aufnahmenummer)) env$tdata$case_no = length (unique(env$medData$Aufnahmenummer))
  if(!is.empty(env$medData$PatientIdentifikator) & !is.empty(env$medData$Aufnahmenummer) & !is.empty(env$medData$ICD_Primaerkode) & !is.empty(env$medData$Orpha_Kode))
    env$medData<-env$medData[!duplicated(env$medData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode","Orpha_Kode")]),]
  else if(!is.empty(env$medData$PatientIdentifikator) & !is.empty(env$medData$Aufnahmenummer) & !is.empty(env$medData$ICD_Primaerkode))
    env$medData<-env$medData[!duplicated(env$medData[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode")]),]
  #D1 completeness
  keyD1 <- checkD1( refData1, cl, basicItem, bItemCl)
  env$mItem <- keyD1$mItem
  env$tdata <- addD1( env$tdata, keyD1$k2_orpha_no, keyD1$k2_orphaCheck_no)
  #D2 plausibility
  keyD2 <- checkD2( refData2, bItemCl, cl)
  env$tdata <- addD2( env$tdata, keyD2$k1_rd_counter, keyD2$k1_check_counter)
  #D3 uniquness
  keyD3 <- checkD3( refData1, refData2, cl)
  env$tdata <- addD3(env$tdata, keyD3$k3_rd_counter,  keyD3$k3_check_counter)
  env$tdata$dup_no = row_no - nrow(env$medData)
  env$tdata$duplication_rate <- round((env$tdata$dup_no/row_no)*100,2)
  env$dup <- input[duplicated(input[c("PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode","Orpha_Kode")]),]
  #D4 concordance
  keyD4 <- checkD4(cl)
  env$tdata <- addD4( env$tdata,  keyD4$k4_counter_orpha, keyD3$k3_rd_counter, inpatientCases)
  td<-getTotalStatistic(dqInd, bItemCl, totalRow)
  out <- list()
  out[["metric"]] <-td
  out[["mItem"]] <-env$mItem
  out
}

#------------------------------------------------------------------------------------------------------
# functions for D1 completeness dimension
#------------------------------------------------------------------------------------------------------
#D1 completeness
checkD1 <- function ( refData, cl, basicItems,bItemCl){
  env$medData<- env$medData[!sapply(env$medData, function(x) all( is.empty(x) | is.na(x)))]
  mItem <- getMissingItem(basicItems)
  if (!is.null(env$cdata)) env$cdata <- getMissingValue(env$cdata, bItemCl, "missing_value", "missing_item")
  if (!is.null(env$ddata))env$ddata <- getMissingValue(env$ddata, bItemCl, "missing_value", "missing_item")
  if (!is.null(env$medData$Orpha_Kode)) dqList <- append(checkOrphaCodingCompleteness(refData, cl), list (mItem=mItem))
  else dqList <-list(k2_orphaCheck_no =0,k2_orpha_no=0,mItem=mItem)
  dqList
}

checkOrphaCodingCompleteness <- function ( refData, cl){
  k2_orpha_no =0
  k2_orphaCheck_no  =0
  missing_counter1=0 
  missing_counter2=0
  refData <-refData[(which(refData$Type=="1:1" | refData$Type=="n:1")),]
  
  env$cdata <- addMissingValue("Orpha_Kode",env$cdata, 0,0)
  env$cdata <- addMissingValue("AlphaID_Kode",env$cdata, 0,0)
  if (!is.null(env$medData$ICD_Primaerkode))
  {
    iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
      rdRefList<- which(stri_trim(as.character(refData$IcdCode))==iCode)
      if (!is.empty(rdRefList)) {
       k2_orphaCheck_no = k2_orphaCheck_no +1
        if("Orpha_Kode" %in% colnames(env$medData)){
          oCode <-as.character(env$medData$Orpha_Kode[i])
          if (!(is.na(oCode) | is.empty(oCode)))k2_orpha_no =k2_orpha_no +1
          else{
            env$dq[,cl][i] <- paste("Fehlender Orpha Code. ", env$dq[,cl][i])
            missing_counter1 =missing_counter1 +1
          }
        }
        if("AlphaID_Kode" %in% colnames(env$medData)){
          aCode <-as.character(env$medData$AlphaID_Kode[i])
          if (is.na(aCode) | is.empty(aCode)) {
            env$dq[,cl][i] <- paste("Fehlender AlphaID Code. ", env$dq[,cl][i])
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

  env$cdata <- addMissingValue("Orpha_Kode", env$cdata, missing_counter1,k2_orphaCheck_no )
  env$cdata <- addMissingValue("AlphaID_Kode", env$cdata, missing_counter2 ,k2_orphaCheck_no )
  out <- list()
  out[["k2_orphaCheck_no"]] <-k2_orphaCheck_no 
  out[["k2_orpha_no"]] <-k2_orpha_no 
  out
}

addD1<- function ( tdata,  orpha, checkNo) {
  if(checkNo>0 & orpha>0){
    tdata$orpha_no <- orpha
    tdata$icdRd_no<- checkNo
    or <- ( orpha/checkNo) * 100
    tdata$orphaCoding_completeness_rate <- round(or,2)
  }
  else {
    tdata$orpha_no <- 0
    tdata$icdRd_no <- 0
    tdata$orphaCoding_completeness_rate<-0
  }
  tdata
}

#------------------------------------------------------------------------------------------------------
# functions for D2 plausibility dimension
#------------------------------------------------------------------------------------------------------
# D2 plausibility
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
  if (!is.null(env$medData$Orpha_Kode)) out <-checkOrphaCoding(refData2, bItemCl, cl)
  else out <- list (k1_rd_counter=0,k1_check_counter=0 )
  out 
}

checkOrphaCoding<- function (refData2, bItemCl, cl) {
  k1_check_counter =0
  k1_rd_counter=0
  if(!is.empty(env$medData$ICD_Primaerkode)){
    iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode) & !is.empty(env$medData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
      oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
      if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode))){
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
            msg<- paste("ICD10-Orpha Zuordnung ist gemäß BfArM nicht plausibel.",  env$dq[,cl][i])
            env$dq[,cl][i] <- msg
          }
          else k1_rd_counter=k1_rd_counter+1
        }
        else{
          if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
            k1_check_counter =k1_check_counter+1
            oRef<- which(as.character (refData2$Orpha_Kode)==oCode)
            if (!is.empty ( oRef)){
              msg<- paste("ICD10-Orpha Zuordnung ist gemäß BfArM nicht plausibel.",  env$dq[,cl][i])
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

checkOutlier<-function (ddata, item, cl) {
  item.vec <- env$medData[[item]]
  if(!is.empty(item.vec)){
    item.vec <-  as.Date(ISOdate(env$medData[[item]], 1, 1))
    out <- getDateOutlier(item.vec)
    if (!is.empty(out)) {
      ddata<- addOutlier (item, ddata, length(out), length(item.vec))
      for(i in out) env$dq[,cl][i] <- paste( "Unplausibles", item , item.vec[i], "Datum liegt in der Zukunft.")
    }   else ddata <- addOutlier(item, ddata, 0,length(item.vec))
    
    if(item == "Geburtsdatum")
    {
      item1.vec <-  as.Date(ISOdate(env$medData[["Geburtsdatum"]], 1, 1))
      now<- as.Date(Sys.Date())
      out<-getAgeMaxOutlier(item1.vec,  now, 105)
      if (!is.empty(out)) {
        ddata<- addOutlier (item, ddata, length(out), length(item1.vec) )
        for(i in out) env$dq[,cl][i] <- paste( "Unplausibles",  item, item1.vec[i] , "Max Alter 105.",  env$dq[,cl][i])
      }
    }
    
  }
  else if (item!="Total"){
    ddata <- addOutlier(item, ddata, 0,0)
  }
  ddata
}

addD2<- function ( tdata,  se, n) {
  if(se>0 & n >0){
    tdata$icdOrpha_no <- n
    tdata$plausible_icdOrpha_no<- se
    or <- ( se/n) * 100
    tdata$orphaCoding_plausibility_rate <- round(or,2)
  }
  else {
    tdata$icdOrpha_no <- 0
    tdata$plausible_icdOrpha_no <- 0
    tdata$orphaCoding_plausibility_rate<-0
  }
  tdata
}

#------------------------------------------------------------------------------------------------------
# functions for D3 uniqueness dimension
#------------------------------------------------------------------------------------------------------
checkD3 <- function (refData1, refData2, cl){
  if (is.null(env$medData$ICD_Primaerkode)) out <-checkUniqueOrphaCoding(cl)
  else if (!is.null(env$medData$Orpha_Kode)) out <-checkUniqueIcdOrphaCoding(refData1, refData2, cl)
       else out <- checkUniqueIcd(refData1, cl)
  out
}
checkUniqueIcd <- function (refData1, cl){
  eList <-refData1[(which(refData1$Type=="1:1" | refData1$Type=="n:1")),]
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
      }
      else {
        iRefList<- which(stri_trim(as.character (refData1$IcdCode))==iCode)
        if (!is.empty (iRefList)){
          msg<- paste("ICD10 Kodierung",iCode, "ist nicht eindeutig. ICD10-Orpha Relation ist gemäß Tracer-Diagnosenliste vom Typ 1-m. ",  env$dq[,cl][i])
          env$dq[,cl][i] <- msg
          k3_check_counter =k3_check_counter+1
        }
      }
    }
  }
  out <- list()
  out[["k3_rd_counter"]] <- k3_rd_counter
  out[["k3_check_counter"]] <- k3_check_counter
  out
}

checkUniqueOrphaCoding <- function (cl){
  k3_check_counter =0
  k3_rd_counter=0
  rd_counter=0
  oList <-which(env$medData$Orpha_Kode !="" & !is.na(env$medData$Orpha_Kode)  & !is.empty(env$medData$Orpha_Kode)& !is.null(env$medData$Orpha_Kode))
  for (i in oList)
  {
    code <-env$medData$Orpha_Kode[i]
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    if (!is.na(oCode)) k3_check_counter =k3_check_counter+1
    else env$dq[,cl][i] <- paste("Fall ist nicht eindeutig.",env$dq[,cl][i] )
    
  }
  out <- list()
  out[["k3_rd_counter"]] <- k3_rd_counter
  out[["k3_check_counter"]] <- k3_check_counter
  out
  }
  
checkUniqueIcdOrphaCoding <- function (refData1, refData2, cl){
  eList <-refData1[(which(refData1$Type=="1:1" | refData1$Type=="n:1")),]
  k3_check_counter =0
  k3_rd_counter=0
  rd_counter=0
  if(!is.empty(env$medData$ICD_Primaerkode)){
    cq <- which(env$medData$ICD_Primaerkode=="" | is.na(env$medData$ICD_Primaerkode))
    #env$cdata <- addMissing("ICD_Primaerkode", env$cdata, length (cq), length(env$medData$ICD_Primaerkode))
    if (!is.empty (cq)) for(i in cq) {
      env$dq[,cl][i]<- paste("Fehlender ICD-Code. ", env$dq[,cl][i])
      code <- env$medData$Orpha_Kode[i]
      oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
      #oRefList<- which(as.character (refData2$Orpha_Kode)==oCode)
      if (! (is.na(code) || is.null(code) || is.empty(code))){
        k3_check_counter =k3_check_counter+1
        #SE-Fälle
        if (!is.na(oCode)) k3_rd_counter=k3_rd_counter+1
        else{
          #else env$dq[,cl][i] <- paste("Kodierung ist nicht eindeutig. Orpha Code",oCode, "ist im BfArM-Mapping nicht enthalten. ",env$dq[,cl][i] )
          env$dq[,cl][i] <- paste("Fall ist nicht eindeutig.",env$dq[,cl][i] )
        }
      }else{
        # Kein SE-Fall
        #k3_check_counter =k3_check_counter+1
        #env$dq[,cl][i] <- paste("Fall ist nicht eindeutig.",env$dq[,cl][i] )
      }
    }
    iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
    for(i in iList){
      iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
      oCode <-env$medData$Orpha_Kode[i]
      numCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
      if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode))){
        iRefList<- which(stri_trim(as.character(refData2$ICD_Primaerkode1))==iCode)
        if (!is.empty (iRefList)){
          oRefList <- ""
          k3_check_counter =k3_check_counter+1
          
          for (j in iRefList){
            oRefCode <-as.integer(refData2$Orpha_Kode[j])
            oRefList <- append( oRefList,oRefCode)
          }
          if ( !is.element(numCode, oRefList))
          {
            msg<- paste("Kodierung ist nicht eindeutig. Relation",iCode,"-", oCode , "ist im BfArM nicht vorhanden. ",  env$dq[,cl][i])
            env$dq[,cl][i] <- msg
          }
          else k3_rd_counter=k3_rd_counter+1
        }
        else{
          if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
            k3_check_counter =k3_check_counter+1
            oRef<- which(as.numeric(as.character(refData2$Orpha_Kode))==numCode)
            if (!is.empty ( oRef)){
              msg<- paste("Kodierung ist nicht eindeutig. ICD10 Code",iCode , "ist im BfArM Mapping nicht enthalten. ",  env$dq[,cl][i])
              env$dq[,cl][i] <- msg
            }
          }
        }
      }
      else{
        if (is.element(iCode, stri_trim(as.character(eList$IcdCode))))
        {
          k3_rd_counter=k3_rd_counter+1
          k3_check_counter =k3_check_counter+1
        }
        else {
          iRefList<- which(stri_trim(as.character (refData1$IcdCode))==iCode)
          if (!is.empty (iRefList)){
            msg<- paste("ICD10 Kodierung",iCode, "ist nicht eindeutig. ICD10-Orpha Relation ist gemäß Tracer-Diagnosenliste vom Typ 1-m. ",  env$dq[,cl][i])
            env$dq[,cl][i] <- msg
            k3_check_counter =k3_check_counter+1
          }
        }
      }
    }
  }
  out <- list()
  out[["k3_rd_counter"]] <- k3_rd_counter
  out[["k3_check_counter"]] <- k3_check_counter
  out
}

addD3<- function (tdata, uRD, checkNo) {
  if(uRD>0 & checkNo >0){
    tdata$unique_rdCase_no <- uRD
    tdata$rdCase_no<- checkNo
    ur <- ( uRD/checkNo) * 100
    tdata$rdCase_uniqueness_rate <- round (ur,2)
  }
  else {
    tdata$unique_rdCase_no <- 0
    tdata$rdCase_no <- 0
    tdata$rdCase_uniqueness_rate  <- 0
  }
  tdata
}

#------------------------------------------------------------------------------------------------------
# functions for D4 concordance dimension
#------------------------------------------------------------------------------------------------------
checkD4 <- function (cl) {
  iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
  k4_counter_icd= length(iList)
  if (!is.null(env$medData$Orpha_Kode))  k4_counter_orpha = getOrphaCodeNo (cl)
  else k4_counter_orpha=0
  out <- list()
  out[["k4_counter_icd"]] <- k4_counter_icd
  out[["k4_counter_orpha"]] <- k4_counter_orpha
  out
}

getOrphaCodeNo <- function (cl) {
  k4_counter_orpha =0 
  oList <-which(env$medData$Orpha_Kode !="" & !is.na(env$medData$Orpha_Kode)  & !is.empty(env$medData$Orpha_Kode) & !is.null(env$medData$Orpha_Kode))
  if (!is.empty (oList)) for(i in oList) {
    code <-env$medData$Orpha_Kode[i]
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    if (!is.na(oCode)) k4_counter_orpha = k4_counter_orpha +1
    else env$dq[,cl][i] <- paste("Orpha Code",code, "ist nicht valide. ", env$dq[,cl][i] )
  }
  k4_counter_orpha
}

addD4<- function (tdata,orpha, uRD, inPtCase) {
  if(orpha>0){
    tdata$orphaCoding_no <- orpha
    tdata$inpatientCases_no <-inPtCase
    or <- ( orpha/inPtCase) * 100
    tdata$orphaCoding_relativeFrequency  <- round (or,2)
  }
  else {
    tdata$orphaCoding_no <- 0
    tdata$inpatientCases_no <-inPtCase
    tdata$orphaCoding_relativeFrequency  <- 0
  }
  if(uRD>0){
    tdata$inpatientCases_no <-inPtCase
    tdata$unique_rdCase_no <-uRD
    rf <- ( uRD/inPtCase) * 100
    tdata$unique_rdCase_relativeFrequency <- round (rf,2)
  }
  else {
    tdata$inpatientCases_no <-inPtCase
    tdata$unique_rdCase_no <- 0
    tdata$unique_rdCase_relativeFrequency<- 0
  }
  tdata
}
getCaseCount<- function (oRefCode, iRefCode) {
  out <- ""
  oCase_counter=0
  iCase_counter=0
  if(!is.empty(env$medData$ICD_Primaerkode)){
    iCaseList<- which(as.character (env$medData$ICD_Primaerkode)==iRefCode)
    if (!is.empty (iCaseList)){
      for (j in iCaseList){
        iCase_counter = iCase_counter+1
        iCode <-as.character(env$medData$ICD_Primaerkode[j])
        oCode <-as.integer(env$medData$Orpha_Kode[j])
        if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode)))
        {
          if ( stri_cmp_eq(oCode, oRefCode))
          {
            oCase_counter = oCase_counter+1
          }
        }
      }
    }
  }
  out <- list()
  out$iCase_counter=iCase_counter
  out$oCase_counter=oCase_counter
  out
}

addRdCase<- function (item, item_text, oCode, iCode, useCase) {
  caseCount <- getCaseCount(oCode, iCode)
  oCase =caseCount$oCase_counter
  iCase= caseCount$iCase_counter
  index = which(useCase$Haus==item)
  if(!is.empty(iCase)){
    useCase$Diagnosetext[index] <- item_text
    useCase$Orpha_Kode[index] <- oCode
    useCase$ICD_Primaerkode[index] <- iCode
    useCase$Fallzahl_ICDKode[index] <- iCase
    useCase$Fallzahl_OrphaKode[index] <- oCase
    useCase$Anteil_OrphaKode[index] <- round((oCase/iCase)* 100,1)
  }
  else {
    useCase$Orpha_Kode[index] <-0
    useCase$ICD_Primaerkode[index] <- 0
    useCase$Fallzahl_ICD-Kode[index] <- 0
    useCase$Fallzahl_OrphaKode[index] <- 0
    useCase$Anteil_OrphaKode[index] <-0

  }
  useCase
}

