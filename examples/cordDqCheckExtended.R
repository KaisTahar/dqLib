# Last Change at 23.06.2021
# Kais Tahar
# Data quality analysis for CORD

library(writexl)
library(stringi)
library(readxl)
library(dqLib)

# install R package
setwd("./")
rm(list = ls())
if (!requireNamespace("devtools")){
  install.packages("devtools")
}
devtools::install_local("../")
#("../R/cordDqLib.R")
#source("../R/dqCore.R")
########## data import #############
# import CORD data
studycode = "CORD_TestData"
medData <- read.table("./Data/medData/DaliTestData_KT.csv", sep=";", dec=",",  header=T, na.strings=c("","NA"), encoding = "latin1")
refData1 <- read.table("./Data/refData/Hamburger-Cord_DQM-List.csv", sep=",",  dec=",", na.strings=c("","NA"), encoding = "UTF-8")
refData2 <- read.table("./Data/refData/icd10gm2020_alphaid_se_muster_edvtxt_20191004.txt", sep="|",  dec=",", na.strings=c("","NA"), encoding = "UTF-8")
names(medData)
mdHeader <- c ("PatientIdentifikator","Aufnahmenummer","DiagnoseText","ICD_Text","ICD_primaerkode","ICD_Manifestation","Orpha_Kode","AlphaID_Kode")
headerRef1<- c ("IcdCode", "OrphaCode", "Type")
headerRef2<- c ("Gueltigkeit", "Alpha_ID", "ICD_primaerkode1", "ICD_Manifestation", "ICD_Zusatz","ICD_primaerkode2", "Orpha_Kode", "Label")
names(medData)<-mdHeader
names(refData1)<-headerRef1
names(refData2)<-headerRef2
dim (medData)

########## DQ Analysis #############
cdata <- data.frame(
  basicItem=
    c ("PatientIdentifikator","Aufnahmenummer","DiagnoseText","ICD_Text","ICD_primaerkode","ICD_Manifestation","Orpha_Kode", "Total")
)
tdata <- data.frame(
  pt_no =NA, case_no =NA
)
repCol=c( "PatientIdentifikator", "ICD_primaerkode","Orpha_Kode")
setGlobals(medData, repCol, cdata, tdata)
env$dq$missing_item<-""
env$dq$missing_value<-""
items <- setdiff (cdata$basicItem ,c ("ICD_primaerkode","Orpha_Kode", "Total"))
cdata <- getMissing(items, "missing_value", "missing_item")$cdata
env$dq$dq_msg<-""
out <-checkCordDQ(refData1,refData2, "dq_msg")
cdata <-out$cdata
tdata <- out$tdata
##########Statistic#####################
tdata<-cbind (getDQStatis(cdata, "basicItem", "Total"), tdata)
td<-subset(tdata, select= c( basicItem, missing_value_rate, completness_rate, orphaCoding_completeness, uniqueness_rate,icdRd_no,rd_no,pt_no, case_no))

########## DQ-Report ###################
path<- paste ("./Data/Export/Datenqualitätsreport_", studycode)
getReport( repCol, "dq_msg", td, path)
########## DQ-Report Extended ##########
#data quality analysis for Projectathon
useCase <- data.frame(
  Haus=c ( "A1", "B1", "B2", "C1", "C2")
)
useCase <-addRdCase("A1", "Klassische Phenylketonurie",79254, "E70.0",  useCase)
useCase <-addRdCase("B1", "Hexosaminidase A‐Mangel",845, "E75.0", useCase)
useCase <-addRdCase ("B2", "Sandhoff‐Krankheit",796, "E75.0", useCase)
useCase <-addRdCase("C1", "Akut‐neuronopathische Gaucher‐Krankheit",77260, "E75.2", useCase)
useCase <-addRdCase("C2", "Anderson‐Fabry‐Krankheit", 324, "E75.2", useCase)
path<- paste ("./Data/Export/Datenqualitätsreport_", studycode, "_Extended", ".xlsx")
getExtendedReport( repCol, "dq_msg", td, useCase, path)

