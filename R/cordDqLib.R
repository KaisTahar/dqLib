# Last Change at 29.04.2021
# Kais Tahar
# this script provides functions for data quality analysis in CORD

getReport <- function (repCol, td, path) {
  repData <-subset(env$dq, select= repCol)
  dfq <-repData[ which(env$dq$dq_msg!="")  ,]
  sheets <- list("DQ_Report"=dfq, "Statistik" = td)
  write_xlsx(sheets, paste (path,".xlsx"))
  write.csv(td, paste (path,".csv"), row.names = FALSE)
}

getExtendedReport <- function ( repCol, td, useCase, path) {
  repData <-subset(env$dq,select= repCol)
  dfq <-repData[ which(env$dq$dq_msg!="")  ,]
  sheets <- list("DQ_Report"=dfq, "Statistik"= td, "Projectathon"=useCase)
  write_xlsx(sheets, path)
}

checkCordDQ <- function ( refData1, refData2) {
  cdata <- env$cdata
  if(!is.empty(env$medData$ICD_Primärkode)){
    cq <- which(env$medData$ICD_Primärkode=="" | is.na(env$medData$ICD_Primärkode))
    cdata <- addMissing("ICD_Primärkode", cdata, length (cq), length(env$medData$ICD_Primärkode))
    if (!is.empty (cq)) for(i in cq) {
      env$dq$dq_msg[i]<- paste("Fehlendes ICD10 Code ", env$dq$dq_msg[i])
      oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
      oRefList<- which(as.character (refData2$Orpha_Kode)==oCode)
      if (is.empty ( oRefList) ){
        msg<- paste("Orpha Kodierung ",oCode, " ist im BfArM-Mapping nicht enthalten",env$dq$dq_msg[i] )
        env$dq$dq_msg[i] <- msg
      }
    }
    if(!is.empty(env$medData$PatientIdentifikator)) {
      cdata$pt_no = length (unique(env$medData$PatientIdentifikator))
    }
    dqList <- checkK2( refData1, cdata)
    dq<- dqList$dq
    cdata<- dqList$cdata
    cdata <- addK2("basicItem", "ICD_Primärkode", cdata, dqList$k2_counter_icdOrpha, dqList$k2_counter_icdRd)
    dqList <- checkK3( refData1, refData2)
    dq<- dqList$dq
    cdata <- addK3("basicItem", "ICD_Primärkode", cdata, dqList$k3_counter,  dqList$k3_counter_icdRd)
  }else {
    cdata <- addMissing("ICD_Primärkode", cdata, 0,0)
    cdata <- addK2("basicItem", "ICD_Primärkode", cdata, 0,0)
    cdata <- addK3("basicItem", "ICD_Primärkode", cdata,0,0)
  }
  out <- list()
  out[["dq"]] <- env$dq
  out[["cdata"]] <- cdata
  out
}

checkK2 <- function ( refData, cdata)
{
  k2_counter_icdOrpha=0
  k2_counter_icdRd =0
  k2_orphaMissing = 0
  iList <-which(env$medData$ICD_Primärkode !="" & !is.na(env$medData$ICD_Primärkode)  & !is.empty(env$medData$ICD_Primärkode))
  for(i in iList){
    iCode <- stri_trim(as.character(env$medData$ICD_Primärkode[i]))
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    rdRefList<- which(stri_trim(as.character(refData$IcdCode))==iCode)
    if (!is.empty(rdRefList)) {
      k2_counter_icdRd =k2_counter_icdRd+1
      if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode))) k2_counter_icdOrpha=k2_counter_icdOrpha+1
      else{
        k2_orphaMissing =  k2_orphaMissing +1
        env$dq$dq_msg[i] <- paste("Fehlendes Orpha_Kode ", env$dq$dq_msg[i])
      }
    }
  }
  cdata <- addMissing("Orpha_Kode", cdata, k2_orphaMissing, length(env$medData$Orpha_Kode))
  out <- list()
  out[["dq"]] <- env$dq
  out[["cdata"]] <- cdata
  out[["k2_counter_icdRd"]] <- k2_counter_icdRd
  out[["k2_counter_icdOrpha"]] <- k2_counter_icdOrpha
  out
}

checkK3 <- function (refData1, refData2)
{
  eRel <- which(refData1$Type=="1:1" | refData1$Type=="n:1")
  eList <- ""
  for (i in eRel){
    icdCode <- stri_trim(as.character(refData1$IcdCode[i]))
    eList <- append(eList,icdCode)
  }
  k3_counter_icdRd =0
  k3_counter=0
  iList <-which(env$medData$ICD_Primärkode !="" & !is.na(env$medData$ICD_Primärkode)  & !is.empty(env$medData$ICD_Primärkode))
  for(i in iList){
    iCode <- stri_trim(as.character(env$medData$ICD_Primärkode[i]))
    oCode <-as.numeric(as.character(env$medData$Orpha_Kode[i]))
    if (!(is.null(oCode) |is.na(oCode) | is.empty(oCode))){
      iRefList<- which(stri_trim(as.character(refData2$ICD_Primärkode1))==iCode)
      if (!is.empty (iRefList)){
        oRefList <- ""
        k3_counter_icdRd =k3_counter_icdRd+1
        for (j in iRefList){
            oRefCode <-as.integer(refData2$Orpha_Kode[j])
            oRefList <- append( oRefList,oRefCode)
        }
        if ( !is.element(oCode, oRefList))
        {
          msg<- paste("Relation ",iCode,"-", oCode , " ist im BfArM nicht vorhanden",  env$dq$dq_msg[i])
          env$dq$dq_msg[i] <- msg
        }
        else k3_counter=k3_counter+1
      }
      else{
        oRef<- which(as.character (refData2$Orpha_Kode)==oCode)
        if (!is.empty ( oRef)){
          msg<- paste("Relation ",iCode,"-", oCode , " ist im BfArM nicht vorhanden")
          env$dq$dq_msg[i] <- msg
        }else{
          msg<- paste("ICD10 Kodierung ",iCode , " ist im BfArm-Mapping nicht enthalten",  env$dq$dq_msg[i])
          env$dq$dq_msg[i] <- msg
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
        iRefList<- which(stri_trim(as.character (refData2$ICD_Primärkode1))==iCode)
        if (!is.empty (iRefList)){
          msg<- paste("ICD10-Kodierung nicht eindeutig",iCode,  env$dq$dq_msg[i])
          env$dq$dq_msg[i] <- msg
        }
      }
    }

  }
  out <- list()
  out[["dq"]] <- env$dq
  out[["k3_counter"]] <- k3_counter
  out[["k3_counter_icdRd"]] <- k3_counter_icdRd
  out
}

addK2<- function (col, row, cdata, n, se) {
  item.vec <- env$medData[[row]]
  index = which( cdata[,col]==row)
  if(!is.empty(item.vec)){
    cdata$orpha_no[index] = n
    cdata$icdRd_no[index] = se
    or <- ( n/se) * 100
    cdata$orphaCoding_completeness[index] = round(or,2)
  }
  else {
    cdata$icdRd_no[index] = 0
    cdata$orphaCoding_completeness[index] = 0
  }
  cdata
}

addK3<- function (col, row, cdata,  n, se) {
  item.vec <- env$medData[[row]]
  index = which( cdata[,col]==row)
  if(!is.empty(item.vec)){
    cdata$K3_no[index] <- n
    cdata$icdRd_no_ext[index] <- se
    or <- ( n/se) * 100
    cdata$uniqueness_rate[index] <- round (or,1)
  }
  else {
    cdata$K3_no[index] <- 0
    cdata$uniqueness_rate[index] <- 0
  }
  cdata
}

getCaseCount<- function (oRefCode, iRefCode) {
  out <- ""
  oCase_counter=0
  iCase_counter=0
  if(!is.empty(env$medData$ICD_Primärkode)){
    iCaseList<- which(as.character (env$medData$ICD_Primärkode)==iRefCode)
    if (!is.empty (iCaseList)){
      for (j in iCaseList){
        iCase_counter = iCase_counter+1
        iCode <-as.character(env$medData$ICD_Primärkode[j])
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
    useCase$ICD_Primärkode[index] <- iCode
    useCase$Fallzahl_ICDKode[index] <- iCase
    useCase$Fallzahl_OrphaKode[index] <- oCase
    useCase$Anteil_OrphaKode[index] <- round((oCase/iCase)* 100,1)
  }
  else {
    useCase$Orpha_Kode[index] <-0
    useCase$ICD_Primärkode[index] <- 0
    useCase$Fallzahl_ICD-Kode[index] <- 0
    useCase$Fallzahl_OrphaKode[index] <- 0
    useCase$Anteil_OrphaKode[index] <-0

  }
  useCase
}

is.empty <- function(x) return(length(x) ==0 )

