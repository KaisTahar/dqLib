# Last Change at 13.08.2021
# Kais Tahar
# this script provides functions for data quality analysis in CORD

getReport <- function (repCol, cl, td, path) {
  repCol = append (repCol, cl)
  repData <-subset(env$dq, select= repCol)
  dfq <-repData[ which(env$dq[,cl]!="")  ,]
  sheets <- list("DQ_Report"=dfq, "Statistik" = td)
  write_xlsx(sheets, paste (path,".xlsx"))
  write.csv(td, paste (path,".csv"), row.names = FALSE)
}

getExtendedReport <- function ( repCol,cl, td, useCase, path) {
  repData <-subset(env$dq,select= repCol)
  dfq <-repData[ which(env$dq[,cl]!="")  ,]
  sheets <- list("DQ_Report"=dfq, "Statistik"= td, "Projectathon"=useCase)
  write_xlsx(sheets, path)
}

checkCordDQ <- function ( instID, refData1, refData2, cl) {
  cdata <- env$cdata
   if ( !is.null(instID)){
   env$tdata$inst_id <- instID
   instData<- medData[which(medData$Institut_ID==instID),]
   if (nrow(instData)>0) env$medData <- instData
  }else {
  	env$tdata$inst_id <- "ID fehlt"
  }
  if(!is.empty(env$medData$ICD_Primaerkode)){
    if(!is.empty(env$medData$PatientIdentifikator)) {
      env$tdata$pt_no = length (unique(env$medData$PatientIdentifikator))
    }
    if(!is.empty(env$medData$Aufnahmenummer)) {
      env$tdata$case_no = length (unique(env$medData$Aufnahmenummer))
    }
    dqList <- checkK2( refData1, cl)
    env$tdata <- addK2( env$tdata, dqList$k2_counter_icdOrpha, dqList$k2_counter_icdRd)
    dqList <- checkK3( refData1, refData2, cl)
    env$tdata <- addK3(env$tdata, dqList$k3_counter,  dqList$k3_counter_rd)
  }else {
    env$cdata <- addMissing("ICD_Primaerkode", env$cdata, 0,0)
    env$cdata <- addK2(env$tdata, 0,0)
    env$cdata <- addK3(env$tada,0,0)
  }
  out <- list()
  out[["dq"]] <- env$dq
  out[["cdata"]] <- env$cdata
  out[["tdata"]] <- env$tdata
  out
}

checkK2 <- function ( refData, cl)
{
  k2_counter_icdOrpha=0
  k2_counter_icdRd =0
  k2_orphaMissing = 0
  iList <-which(env$medData$ICD_Primaerkode !="" & !is.na(env$medData$ICD_Primaerkode)  & !is.empty(env$medData$ICD_Primaerkode))
  for(i in iList){
    iCode <- stri_trim(as.character(env$medData$ICD_Primaerkode[i]))
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    rdRefList<- which(stri_trim(as.character(refData$IcdCode))==iCode)
    if (!is.empty(rdRefList)) {
      k2_counter_icdRd =k2_counter_icdRd+1
      if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode))) k2_counter_icdOrpha=k2_counter_icdOrpha+1
      else{
        k2_orphaMissing =  k2_orphaMissing +1
        env$dq[,cl][i] <- paste("Fehlendes Orpha_Kode. ", env$dq[,cl][i])
        env$cdata <- addMissing("Orpha_Kode", env$cdata, k2_orphaMissing, length(env$medData$Orpha_Kode))
      }
    }
  }
  out <- list()
  out[["k2_counter_icdRd"]] <- k2_counter_icdRd
  out[["k2_counter_icdOrpha"]] <- k2_counter_icdOrpha
  out
}

checkK3 <- function (refData1, refData2, cl)
{
  eRel <- which(refData1$Type=="1:1" | refData1$Type=="n:1")
  eList <- ""
  for (i in eRel){
    icdCode <- stri_trim(as.character(refData1$IcdCode[i]))
    eList <- append(eList,icdCode)
  }
  k3_counter_icdRd =0
  k3_counter=0
  if(!is.empty(env$medData$ICD_Primaerkode)){
    cq <- which(env$medData$ICD_Primaerkode=="" | is.na(env$medData$ICD_Primaerkode))
    env$cdata <- addMissing("ICD_Primaerkode", env$cdata, length (cq), length(env$medData$ICD_Primaerkode))
    if (!is.empty (cq)) for(i in cq) {
      env$dq[,cl][i]<- paste("Fehlendes ICD10 Code. ", env$dq[,cl][i])
      oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
      oRefList<- which(as.character (refData2$Orpha_Kode)==oCode)
      if (is.empty (oRefList) & ! (is.na(oCode) || is.null(oCode))){
        msg<- paste("Orpha Kodierung",oCode, "ist im BfArM-Mapping nicht enthalten. ",env$dq[,cl][i] )
        env$dq[,cl][i] <- msg
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
        k3_counter_icdRd =k3_counter_icdRd+1

        for (j in iRefList){
            oRefCode <-as.integer(refData2$Orpha_Kode[j])
            oRefList <- append( oRefList,oRefCode)
        }
        if ( !is.element(oCode, oRefList))
        {
          msg<- paste("Kodierung ist nicht eindeutig. Relation",iCode,"-", oCode , "ist im BfArM nicht vorhanden. ",  env$dq[,cl][i])
          env$dq[,cl][i] <- msg
        }
        else k3_counter=k3_counter+1
      }
      else{
        if (!(is.null(iCode) |is.na(iCode) | is.empty(iCode))){
          k3_counter_icdRd =k3_counter_icdRd+1
          oRef<- which(as.character (refData2$Orpha_Kode)==oCode)
          if (!is.empty ( oRef)){
            msg<- paste("Kodierung ist nicht eindeutig. Relation",iCode,"-", oCode , "ist im BfArM nicht vorhanden. ")
            env$dq[,cl][i] <- msg
          }else{  msg<- paste("Kodierung ist nicht eindeutig. ICD10 Kodierung",iCode , "ist im BfArM Mapping nicht enthalten. ",  env$dq[,cl][i])
          env$dq[,cl][i] <- msg
          }
        }
      }
    }
    else{
      if (is.element(iCode, eList))
      {
        k3_counter=k3_counter+1
        k3_counter_icdRd =k3_counter_icdRd+1
      }
      else {
        iRefList<- which(stri_trim(as.character (refData1$IcdCode))==iCode)
        if (!is.empty (iRefList)){
          msg<- paste("ICD10 Kodierung",iCode, "ist nicht eindeutig. ICD10-Orpha Relation ist gemäß Tracer-Diagnosenliste vom Typ 1-m. ",  env$dq[,cl][i])
          env$dq[,cl][i] <- msg
          k3_counter_icdRd =k3_counter_icdRd+1
        }
      }
    }
  }
  }
  out <- list()
  out[["k3_counter"]] <- k3_counter
  out[["k3_counter_rd"]] <- k3_counter_icdRd
  out
}

addK2<- function ( tdata,  n, se) {
  if(se>0){
    tdata$orpha_no <- n
    tdata$icdRd_no<- se
    or <- ( n/se) * 100
    tdata$orphaCoding_completeness <- round(or,2)
  }
  else {
    tdata$orpha_no <- 0
    tdata$icdRd_no <- 0
    tdata$orphaCoding_completeness<-0
  }
  tdata
}

addK3<- function (tdata,  n, se) {
  if(se>0){
    tdata$rd_no <- n
    tdata$rd_no_ext<- se
    or <- ( n/se) * 100
    tdata$uniqueness_rate <- round (or,1)
  }
  else {
    tdata$rd_no <- 0
    tdata$rd_no_ext <- 0
    tdata$uniqueness_rate  <- 0
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

