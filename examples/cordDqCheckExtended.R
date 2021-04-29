# Last Change at 29.04.2021
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
#source("../R/cordDqLib.R")
#source("../R/dqCore.R")
########## data import #############
# import CORD data
studycode = "CORD_TestData"
medData <- read.table("./Data/medData/Dali2020_ICD_Orpha_Bezüge_KT.csv", sep=";", dec=",",  header=T, na.strings=c("","NA"), encoding = "latin1")
refData1 <- read.table("./Data/refData/Hamburger-Cord_DQM-List.csv", sep=",",  dec=",", na.strings=c("","NA"), encoding = "UTF-8")
refData2 <- read.table("./Data/refData/icd10gm2020_alphaid_se_muster_edvtxt_20191004.txt", sep="|",  dec=",", na.strings=c("","NA"), encoding = "UTF-8")
names(medData)
mdHeader <- c ("PatientIdentifikator","Aufnahmenummer","DiagnoseText","ICD_Text","ICD_Primärkode","ICD_Manifestation","Orpha_Kode","AlphaID_Kode")
headerRef1<- c ("IcdCode", "OrphaCode", "Type")
headerRef2<- c ("Gültigkeit", "Alpha_ID", "ICD_Primärkode1", "ICD_Manifestation", "ICD_Zusatz","ICD_Primärkode2", "Orpha_Kode", "Label")
names(medData)<-mdHeader
names(refData1)<-headerRef1
names(refData2)<-headerRef2
dim (medData)

########## DQ Analysis #############
cdata <- data.frame(
  basicItem=c ("ICD_Primärkode", "Orpha_Kode" , "Total")
)
repCol1<- c( "PatientIdentifikator", "ICD_Primärkode","Orpha_Kode", "dq_msg")
setGlobals (medData, cdata)
out <-checkCordDQ(refData1,refData2)
cdata <-out$cdata
dq <- out$dq

##########Statistic#####################
tdata<-getDQStatis(cdata, "basicItem", "Total")
td<-subset(tdata, select= c( basicItem, missing_value_rate, completness_rate, orphaCoding_completeness, uniqueness_rate,icdRd_no,icdRd_no_ext,pt_no))

########## DQ-Report ###################
path<- paste ("./Data/Export/Datenqualitätsreport_", studycode)
getReport( repCol1, td, path)
########## DQ-Report Extended ##########
#data quality analysis for Projectathon
useCase <- data.frame(
  Haus=c ( "A1", "B1", "B2", "C1")
)
useCase <-addRdCase("A1", "Klassische Phenylketonurie",79254, "E70.0",  useCase)
useCase <-addRdCase("B1", "Hexosaminidase A‐Mangel",845, "E75.0", useCase)
useCase <-addRdCase ("B2", "Sandhoff‐Krankheit",796, "E75.0", useCase)
useCase <-addRdCase("C1", "Akut‐neuronopathische Gaucher‐Krankheit",77260, "E75.2", useCase)
useCase <-addRdCase("C2", "Anderson‐Fabry‐Krankheit", 324, "E75.2", useCase)
path<- paste ("./Data/Export/Datenqualitätsreport_", studycode, "_Extended", ".xlsx")
getExtendedReport( repCol1,  td, useCase, path)

