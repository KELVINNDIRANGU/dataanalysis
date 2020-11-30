library(readxl)
library(rmarkdown)
library(knitr)
library(magrittr) 
library(dplyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyr)
library(readxl)
library(export)

#Import the data from excelsheet to R

GeneralRecords_2019 <- read_excel("GeneralRecords_2019.xls")


#Vew the data
View(GeneralRecords_2019)

#create a summarized table of referring doctors procedures
ReferringDoctors<-GeneralRecords_2019 %>% 
  group_by(`Referring Doctor`) %>% 
  summarise(Procedures=n())
RefDoctors <- na.omit(ReferringDoctors) 
RefDoctors
#write the output in an excel file
table2excel(RefDoctors, file = "RefferingDoctors")

#create a summarized table of referring doctors/ prof


#create a summarized table of referring instutions


#Filter on the referring doctors procedures with 20 exams and above
CommonDoctors<-RefDoctors %>%
  filter(Procedures>=200) 
CommonDoctors



#create a summarized table of referring doctors and modalities from the bigdata
ReferringDoctors_mod <-GeneralRecords_2019 %>% 
  group_by(`Referring Doctor`, Modality) %>% 
  summarise(Procedure=n())
ReferringDoctors_mod

#create a summarized table of referring doctors and modalities using spread
ReferringDoctors_Modality <- ReferringDoctors_mod %>% 
  spread(Modality,Procedure ,fill = NA, convert = FALSE) 
ReferringDoctors_Modality
#summarized table refering doctors modality in excel
table2excel(ReferringDoctors_Modality, file = "ReferringDoctors_Modality")


#Referring Doctors and specific Modalities
#XRay
ReferringDoctors_Xray<-ReferringDoctors_mod %>% 
  filter(Modality=="CR")       #Procedure>=10)
ReferringDoctors_Xray
table2excel(ReferringDoctors_Xray, file = "ReferringDoctors_Xray")

#CT
ReferringDoctors_CT<-ReferringDoctors_mod %>% 
  filter(Modality=="CT")     #Procedure>=5)
ReferringDoctors_CT
table2excel(ReferringDoctors_CT, file = "ReferringDoctors_CT")

#MRI
ReferringDoctors_MR<-ReferringDoctors_mod %>% 
  filter(Modality=="MR")   #Procedure>=5)
ReferringDoctors_MR
table2excel(ReferringDoctors_MR, file = "ReferringDoctors_MRI")

#US
ReferringDoctors_US<-ReferringDoctors_mod %>% 
  filter(Modality=="US") #Procedure>=10)
ReferringDoctors_US
table2excel(ReferringDoctors_US, file = "ReferringDoctors_UltraSound")

#OT
ReferringDoctors_Lab<-ReferringDoctors_mod %>% 
  filter(Modality=="OT")  #Procedure>=10)
ReferringDoctors_Lab
table2excel(ReferringDoctors_Lab, file = "ReferringDoctors_lab")

#ECG
ReferringDoctors_ECG<-ReferringDoctors_mod %>% 
  filter(Modality=="ECG")  #Procedure>=0)
ReferringDoctors_ECG
table2excel(ReferringDoctors_ECG, file = "ReferringDoctors_ECG")

#SE
ReferringDoctors_SE<-ReferringDoctors_mod %>% 
  filter(Modality=="SE")    #Procedure>=0)
ReferringDoctors_SE
table2excel(ReferringDoctors_SE, file = "ReferringDoctors_SpecialExams")


#create a summarized table for Modalities
Modalities <- GeneralRecords_2019 %>% 
  group_by(Modality)%>% 
  summarise(Count=n())
modali<- na.omit(Modalities) 
modali

#writing to excel 
table2excel(modali, file = "Modalities")   



#create a summarized table for Specific Procedures
SpecificExams<- GeneralRecords_2019 %>% 
  group_by(`Procedure Name`) %>% 
  summarise(count=n())
specificexa <- na.omit(SpecificExams)
specificexa
table2excel(specificexa, file = "SpecificExams")


#Specific Modalities
ModalityProcedures<- GeneralRecords_2019 %>% 
  group_by(`Procedure Name`, Modality) %>% 
  summarise(count=n())

#Specific Modalities MRI
MRI<-ModalityProcedures %>% 
  filter(Modality=="MR") 
MRI
table2excel(MRI, file = "MRI PROCEDURES")


#Specific Modalities UTRASOUND
US <-ModalityProcedures %>% 
  filter(Modality=="US") 
US
table2excel(US, file = "US PROCEDURES")


#Specific Modalities CT SCAN
CT <-ModalityProcedures %>% 
  filter(Modality=="CT") 
CT
table2excel(CT, file = "CT PROCEDURES")

CT_UPPERBODY <- CT %>% 
  filter(str_detect(`Procedure Name`,"CT BRAIN" , "NECK")) 
CT_UPPERBODY


#Specific Modalities Xray SCAN
CR <-ModalityProcedures %>% 
  filter(Modality=="CR") 
CR
table2excel(CR, file = "Xray PROCEDURES")

#Specific exams in Laboratory
OT <-ModalityProcedures %>% 
  filter(Modality=="OT") 
OT
table2excel(OT, file = "LAB PROCEDURES")


#Insurance
Insurance <-GeneralRecords_2019 %>% 
  group_by(`PriceList Name`) %>% 
  summarise(Procedures=n())
RefInsurance <- na.omit(Insurance) 
RefInsurance
#write the output in an excel file
table2excel(RefInsurance, file = "Insurance")



