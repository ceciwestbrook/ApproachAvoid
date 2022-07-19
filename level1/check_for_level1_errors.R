library(gdata)
library(tidyverse)
setwd('/ix/cladouceur/westbrook-data/Scripts')
allsubs <- read.csv('imaging_subjects.txt')
setwd('level1')
prerun1 <- as.data.frame(t(read.table('subjects_prerun1.txt')))
prerun2 <- as.data.frame(t(read.table('subjects_prerun2.txt')))
postrun1 <- as.data.frame(t(read.table('subjects_postrun1.txt')))
postrun2 <- as.data.frame(t(read.table('subjects_postrun2.txt')))

for(subject in prerun1$V1){
      if(file.exists(paste0('/ix/cladouceur/westbrook-data/processed/sub-',subject,
        '/ses-pretreatment/level1/run1.feat/stats/cope8.nii.gz'))==FALSE){
        print(c(subject,"prerun1 NOT FOUND"))
      } else {
        print(c(subject,"prerun1 found"))
      }

}

for(subject in prerun2$V1){
  if(file.exists(paste0('/ix/cladouceur/westbrook-data/processed/sub-',subject,
                        '/ses-pretreatment/level1/run2.feat/stats/cope8.nii.gz'))==FALSE){
    print(c(subject,"prerun2 NOT FOUND"))
    } else {
      print(c(subject,"prerun2 found"))
  }
  
}

for(subject in postrun1$V1){
  if(file.exists(paste0('/ix/cladouceur/westbrook-data/processed/sub-',subject,
                        '/ses-posttreatment/level1/run1.feat/stats/cope8.nii.gz'))==FALSE){
    print(c(subject,"postrun1 NOT FOUND"))
    } else {
      print(c(subject,"postrun1 found"))
  }
  
}    

for(subject in postrun2$V1){
  if(file.exists(paste0('/ix/cladouceur/westbrook-data/processed/sub-',subject,
                        '/ses-posttreatment/level1/run2.feat/stats/cope8.nii.gz'))==FALSE){
    print(c(subject,"postrun2 NOT FOUND"))
    } else {
      print(c(subject,"postrun2 found"))
  }
  
}  
    