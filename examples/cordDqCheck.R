
# Last Change at 03.06.2021
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
devtools::install_local("../")
#source("../R/cordDqLib.R")
#source("../R/dqCore.R")

########## data import #############
# import CORD data

studycode = "CORD_TestData"
# CSV and XLSX file formats are supported
# path="./Data/medData/Dali2020_ICD_Orpha_Bezüge_KT.xlsx"
path ="./Data/medData/Dali2020_ICD_Orpha_Bezüge_KT.csv"
ext <-getFileExtension (path)
if (ext=="csv") medData<- read.table(path, sep=";", dec=",",  header=T, na.strings=c("","NA"), encoding = "latin1")
if (ext=="xlsx") medData <- read.xlsx(path, sheet=1,skipEmptyRows = TRUE)
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
td<-subset(tdata, select= c( basicItem, missing_value_rate, completness_rate, orphaCoding_completeness, uniqueness_rate,icdRd_no, icdRd_no_ext,pt_no))

########## DQ-Report ###################
path<- paste ("./Data/Export/Datenqualitätsreport_", studycode)
getReport( repCol1, td, path)

