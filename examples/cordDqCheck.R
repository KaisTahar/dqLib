###################
# Kais Tahar
# Data quality analysis for CORD

library(writexl)
library(stringi)
library(openxlsx)
library(dqLib)

# install R package
setwd("./")
rm(list = ls())
if (!requireNamespace("devtools")){
  install.packages("devtools")
}
library(devtools)
install_github("https://github.com/KaisTahar/dqLib")
#source("../R/cordDqLib.R")
#source("../R/dqCore.R")


########## data import #############
# import CORD data
# CSV and XLSX file formats are supported
studycode = "dqTestData_KT"
inst_ID="259294944-TestHaus"
path="./Data/medData/dqTestData_KT.csv"
ext <-getFileExtension (path)
if (ext=="csv") medData<- read.table(path, sep=";", dec=",",  header=T, na.strings=c("","NA"), encoding = "latin1")
if (ext=="csv") test<- read.table(path, sep=";", dec=",",  header=T, na.strings=c("","NA"), encoding = "latin1")
if (ext=="xlsx") medData <- read.xlsx(path, sheet=1,skipEmptyRows = TRUE)
refData1 <- read.table("./Data/refData/cordDqList.csv", sep=",",  dec=",", na.strings=c("","NA"), encoding = "UTF-8")
refData2 <- read.table("./Data/refData/icd10gm2020_alphaid_se_muster_edvtxt_20191004.txt", sep="|",  dec=",", na.strings=c("","NA"), encoding = "UTF-8")
names(medData)
mdHeader <- c ("Institut_ID","PatientIdentifikator","Aufnahmenummer","Diagnosetext","ICD_Text","ICD_Primaerkode","ICD_Manifestation","Orpha_Kode","AlphaID_Kode")
headerRef1<- c ("IcdCode", "OrphaCode", "Type")
headerRef2<- c ("Gueltigkeit", "Alpha_ID", "ICD_Primaerkode1", "ICD_Manifestation", "ICD_Zusatz","ICD_Primaerkode2", "Orpha_Kode", "Label")

diff <- setdiff (mdHeader, names (medData))
if (!is.empty (diff)){
  str<- paste (diff,collapse=" " )
  msg <- paste ("Folgende Items fehlen: ", str)
  stop(msg)
}
names(refData1)<-headerRef1
names(refData2)<-headerRef2
dim (medData)
inpatientCases <- 1000


########## DQ Analysis #############
cdata <- data.frame(
  basicItem=

  c ("PatientIdentifikator","Aufnahmenummer", "Institut_ID",  "Geschlecht", "Diagnosetext","ICD_Text","ICD_Primaerkode","Orpha_Kode", "Total")
)
ddata <- data.frame(
  basicItem=
    c ( "Geburtsdatum",  "Aufnahmedatum", "Entlassungsdatum", "Total")
)
tdata <- data.frame(
  pt_no =NA, case_no =NA
)
repCol=c( "PatientIdentifikator", "Aufnahmenummer", "ICD_Primaerkode","Orpha_Kode")
setGlobals(medData, repCol, cdata, ddata, tdata)
items <- setdiff (cdata$basicItem ,c ("ICD_primaerkode","Orpha_Kode", "Total"))
cdata <-getMissing(cdata,  cdata$basicItem, "missing_value", "missing_item")
ddata <-getMissing(ddata,  ddata$basicItem, "missing_value", "missing_item")
out <-checkCordDQ(inst_ID, inpatientCases, cdata, ddata, refData1, refData2, "dq_msg")
##########Statistic#####################
tdata<-cbind (getTotalStatistic(out$cdata, out$ddata, "basicItem", "Total"),  out$tdata)
td<-subset(tdata, select= c( inst_id, missing_value_rate, outlier_rate, orphaCoding_completeness, uniqueness_rate, orphaCoding_plausibility_rate, orphaCoding_relativeFrequency, orphaCoding_absoluteFrequency,  uniqueRd_no, pt_no, case_no))
########## DQ-Report ###################
path<- paste ("./Data/Export/Datenqualitätsreport_", studycode)
getReport( repCol, "dq_msg", td, path)

