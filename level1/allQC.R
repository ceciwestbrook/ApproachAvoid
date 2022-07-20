# Ceci Westbrook, 2022
# File for creating the QC database for use in data examination and exclusion (among other things).

library(tidyverse)
library(reshape2)
library(haven)
library(reshape2)

setwd("/ix/cladouceur/westbrook-data/Scripts/preprocessing/")
spikes <- read.csv("spike_numbers.txt",sep=" ",header=FALSE)
spikes <- spikes[,c(1:2,4:5)]
colnames(spikes) <- c("ID_Num","session","run","nspikes")
spikes1 <- dcast(spikes,ID_Num ~ session + run)
write.table(spikes1,"spike_numbers_wide.txt",sep=" ",row.names=FALSE)

setwd("/ix/cladouceur/westbrook-data/Scripts/level1")
behav <- read.csv("behav_data.txt",sep=" ",header=FALSE)
colnames(behav) <- c("ID_Num","pretreatment1behav","pretreatment2behav",
                     "posttreatment1behav","posttreatment2behav")

allQC <- merge(spikes1,behav,by="ID_Num",all=TRUE)
allQC <- rename(allQC,"pretreatment_mot1" ="pretreatment_1",
      "pretreatment_mot2" = "pretreatment_2",
      "posttreatment_mot1" = "posttreatment_1",
      "posttreatment_mot2" = "posttreatment_2")

setwd("/ix/cladouceur/westbrook-data/files")
CATS <- read_spss("CATS_DataSet_All.sav")
dispo <- CATS[,c("ID_Num","dispo","Therapy")]
allQC <- merge(allQC,dispo,by="ID_Num")
allQC <- allQC[allQC$dispo<4,]

siegle_QC <- read_spss("fMRI_DC_QC.sav")
siegle_QC <- siegle_QC[siegle_QC$Contact_Num %in% c(2,22) & siegle_QC$Task_Num==2,c("ID_Num","Contact_Num","Avoid_Not_Usable")]
siegle_QC$Avoid_Not_Usable <- as.numeric(siegle_QC$Avoid_Not_Usable)
siegle_QC <- spread(siegle_QC,Contact_Num,Avoid_Not_Usable)
siegle_QC <- rename(siegle_QC,
       "Avoid_Not_Usable_pretreatment" = '2',
       "Avoid_Not_Usable_posttreatment" = '22')
allQC <- merge(allQC,siegle_QC,all.x=TRUE)

allQC$usable_prerun1 <- 0
allQC$usable_prerun2 <- 0
allQC$usable_postrun1 <- 0
allQC$usable_postrun2 <- 0
allQC[(which(allQC$pretreatment_mot1<=58 & allQC$pretreatment1behav==1)),"usable_prerun1"] <- 1
allQC[(which(allQC$pretreatment_mot2<=58 & allQC$pretreatment2behav==1)),"usable_prerun2"] <- 1
table(allQC$usable_prerun1)
table(allQC$usable_prerun2)
allQC[(which(allQC$posttreatment_mot1<=58 & allQC$posttreatment1behav==1)),"usable_postrun1"] <- 1
allQC[(which(allQC$posttreatment_mot2<=58 & allQC$posttreatment2behav==1)),"usable_postrun2"] <- 1
table(allQC$usable_postrun1)
table(allQC$usable_postrun2)
allQC$usable_pretreatment <- 0
allQC$usable_posttreatment <- 0
allQC[which(allQC$usable_prerun1==1 | allQC$usable_prerun2==1), "usable_pretreatment"] <- 1
allQC[which(allQC$usable_postrun1==1 | allQC$usable_postrun2==1), "usable_posttreatment"] <- 1

allQC[which(allQC$Avoid_Not_Usable_pretreatment==1),"ID_Num"]
allQC[which(allQC$Avoid_Not_Usable_pretreatment==1),"usable_pretreatment"] <- 0
allQC[which(allQC$Avoid_Not_Usable_posttreatment==1),"ID_Num"]
allQC[which(allQC$Avoid_Not_Usable_posttreatment==1),"usable_posttreatment"] <- 0

table(allQC[which(allQC$usable_pretreatment==1),"Therapy"])
table(allQC[which(allQC$usable_posttreatment==1),"Therapy"])
table(allQC[which(allQC$usable_pretreatment==1 & allQC$usable_posttreatment==1),"Therapy"])

table(CATS[CATS$dispo<4,c("K01_Diagnosis","Therapy")])

# write out spreadsheet
#setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/')
#write.table(allQC,"allQC.txt",sep="\t",row.names = FALSE)
