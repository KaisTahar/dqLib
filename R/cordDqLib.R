# Kais Tahar
# this script provides functions for data quality analysis in CORD

getReport <- function (repCol, cl, td, path) {
  repCol = append (repCol, cl)
  repData <-subset(env$dq, select= repCol)
  dfq <-repData[ which(env$dq[,cl]!="")  ,]
  dfq[nrow(dfq)+1,] <- NA
  dfq[nrow(dfq)+1,1] <- env$mItem
  sheets <- list("DQ_Report"=dfq, "Statistik" = td)
  write_xlsx(sheets, paste (path,".xlsx"))
  write.csv(td, paste (path,".csv"), row.names = FALSE)
 # env <-NULL
}

getExtendedReport <- function ( repCol,cl, td, useCase, path) {
  repData <-subset(env$dq,select= repCol)
  dfq <-repData[ which(env$dq[,cl]!="")  ,]
  sheets <- list("DQ_Report"=dfq, "Statistik"= td, "Projectathon"=useCase)
  write_xlsx(sheets, path)
}

checkCordDQ <- function ( instID, reportYear, inpatientCases, refData1, refData2, dqInd, cl, bItemCl, totalRow) {
  env$tdata$report_year <-reportYear
  basicItem <- setdiff  (union(env$cdata[, bItemCl], env$ddata[, bItemCl]),totalRow)
   if ( !is.null(instID)){
   env$tdata$inst_id <- instID
   instData<- medData[which(medData$Institut_ID==instID),]
   if (nrow(instData)>0) env$medData <- instData
  }else {
  	env$tdata$inst_id <- "ID fehlt"
  }
  if(!is.empty(env$medData$ICD_Primaerkode)){
    if(!is.empty(env$medData$PatientIdentifikator)) {
      env$tdata$patient_no = length (unique(env$medData$PatientIdentifikator))
    }
    if(!is.empty(env$medData$Aufnahmenummer)) {
      env$tdata$case_no = length (unique(env$medData$Aufnahmenummer))
    }
    #D1
    dqList <- checkK1( refData2, bItemCl, cl)
    env$tdata <- addK1( env$tdata, dqList$k1_rd_counter, dqList$k1_check_counter)
    #D2 
    dqList <- checkK2( refData1, cl, basicItem, bItemCl)
    env$mItem <- dqList$mItem
    env$tdata <- addK2( env$tdata, dqList$k2_icdOrpha_counter, dqList$k2_icdRd_counter)
    #D3
    dqList <- checkK3( refData1, refData2, cl)
    env$tdata <- addK3(env$tdata, dqList$k3_rd_counter,  dqList$k3_check_counter, inpatientCases)
    #D4
    dqList <- checkK4( refData2, cl)
    #env$tdata <- addK4( env$tdata,  dqList$k4_counter_orpha, dqList$k4_counter_icd)
    env$tdata <- addK4( env$tdata,  dqList$k4_counter_orpha, inpatientCases)
  }else {
    env$cdata <- addMissing("ICD_Primaerkode", env$cdata, 0,0)
    env$cdata <- addK2(env$tdata, 0,0)
    env$cdata <- addK3(env$tada,0,0)
  }
  td<-getTotalStatistic(dqInd, bItemCl, totalRow)
  td
}


checkK1 <- function (refData2, bItemCl, cl){
# get outliers
  dItem <- env$ddata[, bItemCl]
  if (!is.empty(dItem)) {
    for (item in unique(dItem)) {
      env$ddata  <-checkOutlier(env$ddata, item, cl)
    }
  }
# check ICD10-Orpha
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

checkK2 <- function ( refData, cl, basicItems,bItemCl){
  mItem <- getMissingItem(basicItems)
  env$cdata <- getMissingValue(env$cdata, bItemCl, "missing_value", "missing_item")
  env$ddata <- getMissingValue(env$ddata, bItemCl, "missing_value", "missing_item")
  dqList <- append(checkOrphaCodingCompleteness(refData, cl), list (mItem=mItem))
  dqList
}

checkOrphaCodingCompleteness <- function ( refData, cl){
  k2_counter_icdOrpha=0
  k2_counter_icdRd =0
  missing_counter1=0 
  missing_counter2=0
  env$cdata <- addMissingValue("Orpha_Kode",env$cdata, 0,0)
  env$cdata <- addMissingValue("AlphaID_Kode",env$cdata, 0,0)
  iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
  for(i in iList){
    iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
    rdRefList<- which(stri_trim(as.character(refData$IcdCode))==iCode)
    if (!is.empty(rdRefList)) {
      k2_counter_icdRd =k2_counter_icdRd+1
      if("Orpha_Kode" %in% colnames(env$medData)){
        oCode <-as.character(env$medData$Orpha_Kode[i])
        if (!(is.na(oCode) | is.empty(oCode))) k2_counter_icdOrpha=k2_counter_icdOrpha+1
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
  env$cdata <- addMissingValue("Orpha_Kode", env$cdata, missing_counter1, k2_counter_icdRd)
  env$cdata <- addMissingValue("AlphaID_Kode", env$cdata, missing_counter2 , k2_counter_icdRd)
  out <- list()
  out[["k2_icdRd_counter"]] <- k2_counter_icdRd
  out[["k2_icdOrpha_counter"]] <- k2_counter_icdOrpha
  out
}

checkK3 <- function (refData1, refData2, cl){
  eRel <- which(refData1$Type=="1:1" | refData1$Type=="n:1")
  eList <- ""
  for (i in eRel){
    icdCode <- stri_trim(as.character(refData1$IcdCode[i]))
    eList <- append(eList,icdCode)
  }
  k3_check_counter =0
  k3_rd_counter=0
  rd_counter=0
  if(!is.empty(env$medData$ICD_Primaerkode)){
    cq <- which(env$medData$ICD_Primaerkode=="" | is.na(env$medData$ICD_Primaerkode))
    #env$cdata <- addMissing("ICD_Primaerkode", env$cdata, length (cq), length(env$medData$ICD_Primaerkode))
    if (!is.empty (cq)) for(i in cq) {
      env$dq[,cl][i]<- paste("Fehlender ICD-Code. ", env$dq[,cl][i])
      oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
      oRefList<- which(as.character (refData2$Orpha_Kode)==oCode)
      if (! (is.na(oCode) || is.null(oCode) || is.empty(oCode))){
        #SE-Fälle
        k3_check_counter =k3_check_counter+1
        if (!is.empty (oRefList))  k3_rd_counter=k3_rd_counter+1
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
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode))){
      iRefList<- which(stri_trim(as.character(refData2$ICD_Primaerkode1))==iCode)
      if (!is.empty (iRefList)){
        oRefList <- ""
        k3_check_counter =k3_check_counter+1

        for (j in iRefList){
            oRefCode <-as.integer(refData2$Orpha_Kode[j])
            oRefList <- append( oRefList,oRefCode)
        }
        if ( !is.element(oCode, oRefList))
        {
          msg<- paste("Kodierung ist nicht eindeutig. Relation",iCode,"-", oCode , "ist im BfArM nicht vorhanden. ",  env$dq[,cl][i])
          env$dq[,cl][i] <- msg
        }
        else k3_rd_counter=k3_rd_counter+1
      }
      else{
        if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
          k3_check_counter =k3_check_counter+1
          oRef<- which(as.character (refData2$Orpha_Kode)==oCode)
          if (!is.empty ( oRef)){
            msg<- paste("Kodierung ist nicht eindeutig. ICD10 Code",iCode , "ist im BfArM Mapping nicht enthalten. ",  env$dq[,cl][i])
            env$dq[,cl][i] <- msg
          }
        }
      }
    }
    else{
      if (is.element(iCode, eList))
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

checkK4 <- function ( refData, cl) {
  k4_counter_icd =0
  k4_counter_orpha =0 
  iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
  oList <-which(env$medData$Orpha_Kode !="" & !is.na(env$medData$Orpha_Kode)  & !is.empty(env$medData$Orpha_Kode))
  k4_counter_icd= length(iList)
  if (!is.empty (oList)) for(i in oList) {
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    oRefList<- which(as.character (refData$Orpha_Kode)==oCode)
    if (! (is.na(oCode) || is.null(oCode) || is.empty(oCode))){
      if (!is.empty (oRefList)) k4_counter_orpha = k4_counter_orpha +1
      else env$dq[,cl][i] <- paste("Orpha Code",oCode, "ist im BfArM-Mapping nicht enthalten. ",env$dq[,cl][i] )
    }
  }
  
  out <- list()
  out[["k4_counter_icd"]] <- k4_counter_icd
  out[["k4_counter_orpha"]] <- k4_counter_orpha
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

addK1<- function ( tdata,  se, n) {
  if(se>0){
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

addK2<- function ( tdata,  n, se) {
  if(se>0){
    tdata$orpha_no <- n
    tdata$icdRd_no<- se
    or <- ( n/se) * 100
    tdata$orphaCoding_completeness_rate <- round(or,2)
  }
  else {
    tdata$orpha_no <- 0
    tdata$icdRd_no <- 0
    tdata$orphaCoding_completeness_rate<-0
  }
  tdata
}

addK3<- function (tdata,  se, n, inpatientCases) {
  if(se>0){
    tdata$unique_rdCase_no <- se
    tdata$check_no<- n
    ur <- ( se/n) * 100
    tdata$rdCase_uniqueness_rate <- round (ur,2)
  }
  else {
    tdata$unique_rdCase_no <- 0
    tdata$check_no <- 0
    tdata$rdCase_uniqueness_rate  <- 0
  }
  tdata
}

addK4<- function (tdata,  se, n) {
  if(se>0){
    tdata$orphaCoding_no <- se
    tdata$inpatientCases_no <-n
    or <- ( se/n) * 100
    tdata$orphaCoding_relativeFrequency  <- round (or,2)
    rf <- ( tdata$unique_rdCase_no/n) * 100
    tdata$unique_rdCase_relativeFrequency <- round (rf,2)
  }
  else {
    tdata$orphaCoding_no <- 0
    tdata$inpatientCases_no <-n
    tdata$orphaCoding_relativeFrequency  <- 0
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

is.empty <- function(x) return(length(x) ==0 )

isDate <- function(mydate) {
  tryCatch(!is.na(as.Date(mydate,tryFormats = c("%Y-%m-%d", "%Y/%m/%d","%d-%m-%Y","%m-%d-%Y"))),
           error = function(err) {FALSE})
}
