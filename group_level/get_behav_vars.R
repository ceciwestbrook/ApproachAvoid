# This is a script designed to output the list of behavioral covariates for copying/pasting
# into FSL's FEAT.

library(haven)
library(tidyverse)
library(readxl)
library(reshape2)
setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/')
d <- read_xlsx('/ix/cladouceur/westbrook-data/Scripts/group_level/allQC.xlsx')
clinical_vars <- d[,c("ID_Num","dispo","Therapy","Gender","Race","AGE_Pre","AGE_Post","AGE_1yr",
                      "Completed_TX","Number_TX","Remission21","Relapse","Response","PARS_01_Total_Score")]

# to get the right subjects by timepoint (pretreatment, posttreatment, or prepost) and cope

setwd('/ix/cladouceur/westbrook-data/Scripts/level2/make_fsfs/')
for(cope in c("cope1","cope2","cope3","cope4","cope5","cope6","cope7","cope8")) {
  for(session in c("pre","post")){
    c1 <- read.table(paste0(cope,session,"run1run2.txt"))
    c2 <- read.table(paste0(cope,session,"run1run1.txt"))
    c3 <- read.table(paste0(cope,session,"run2run2.txt"))
    towrite <- rbind(c1,c2,c3)
    #write.table(towrite,paste0('/ix/cladouceur/westbrook-data/Scripts/group_level/subjects_by_cope/',cope,session,"treatment.txt"),row.names = FALSE, col.names = FALSE)
    towrite$sub <- 1
    colnames(towrite) <- c("ID_Num",paste0(cope,session,"_subs"))
    assign(paste0(cope,session),towrite)
  }
}

df_list <- list(clinical_vars,cope1pre,cope1post,cope2pre,cope2post,cope3pre,cope3post,cope4pre,cope4post,
                cope5pre,cope5post,cope6pre,cope6post,cope7pre,cope7post,cope8pre,cope8post)

clinical_vars <- df_list %>% reduce(full_join, by='ID_Num')

### THERAPY GROUPS ###
clinical_vars$therapy_numeric <- ifelse(clinical_vars$Therapy=="CBT",1,
                                        ifelse(clinical_vars$Therapy=="CCT",2,
                                               ifelse(clinical_vars$Therapy=="Control",0,NA)))
clinical_vars$CBT <- ifelse(clinical_vars$Therapy=="CBT",1,0)
clinical_vars$CCT <- ifelse(clinical_vars$Therapy=="CCT",1,0)
clinical_vars$Control <- ifelse(clinical_vars$Therapy=="Control",1,0)
clinical_vars$Anx <- ifelse(clinical_vars$Therapy %in% c("CBT","CCT"),1,
                            ifelse(clinical_vars$Therapy=="Control",0,NA))

### Set up some variables that we will use later ###

clinical_vars$Anx_Inv <- ifelse(clinical_vars$Anx==1,0,
                                ifelse(clinical_vars$Anx==0,1,NA))
clinical_vars$Remission21 <- as.numeric(clinical_vars$Remission21)
clinical_vars$Remis_Inv <- ifelse(clinical_vars$Remission21==1,0,
                                 ifelse(clinical_vars$Remission21==0,1,NA))

# For the interaction terms
clinical_vars$Anx_dummy <- ifelse(clinical_vars$Anx==1,1,
                                  ifelse(clinical_vars$Anx==0,-1,NA))
clinical_vars$Resp_dummy <- ifelse(clinical_vars$PARS_21_35erImp_0==1,1,
                                   ifelse(clinical_vars$PARS_21_35erImp_0==0,-1,NA))
clinical_vars$Therapy_dummy <- ifelse(clinical_vars$Therapy=="CBT",1,
                                      ifelse(clinical_vars$Therapy=="CCT",-1,NA))

###### Imputed data ######
# Check out how much the imputed variables differ from another

# # This is the non-imputed data
# table(clinical_vars$PARS_21_35erImp_0)
# # These are all the same
# table(clinical_vars[,c("PARS_21_35erImp_0","PARS_21_35erImp_1")])
# 
# # this is the imputed data
# table(clinical_vars[,c("PARS_21_35erImp_1","PARS_21_35erImp_2")])
# table(clinical_vars[,c("PARS_21_35erImp_1","PARS_21_35erImp_3")])
# table(clinical_vars[,c("PARS_21_35erImp_1","PARS_21_35erImp_4")])
# table(clinical_vars[,c("PARS_21_35erImp_1","PARS_21_35erImp_5")])

setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/behav_analyses/')
imputed <- read_spss('Derived1_5.sav')
imputed <- imputed[order(imputed$ID_Num),c("ID_Num","Imputation_","Remission21_Imp","Remission36_Imp","PARS_21_35erImp","PARS_36_35erImp")]
imputed$PARS_21_35erImp <- as.numeric(imputed$PARS_21_35erImp)
imputed$PARS_36_35erImp <- as.numeric(imputed$PARS_36_35erImp)
imputed$Remission21_Imp <- as.numeric(imputed$Remission21_Imp)
imputed$Remission36_Imp <- as.numeric(imputed$Remission36_Imp)

for (varname in variable.names(imputed[,3:6])) {
  cur_var <- dcast(imputed,ID_Num~Imputation_,value.var=varname)
  colnames(cur_var) <- c("ID_Num",paste0(varname,"_0"),paste0(varname,"_1"),paste0(varname,"_2"),paste0(varname,"_3"),
                         paste0(varname,"_4"),paste0(varname,"_5"))
  assign(paste0("test_",varname),cur_var)
}

df_list <- list(test_PARS_21_35erImp,test_PARS_36_35erImp,test_Remission21_Imp,test_Remission36_Imp)
imputed_vars <- df_list %>% reduce(full_join, by='ID_Num')
clinical_vars <- left_join(clinical_vars,imputed_vars,by="ID_Num")

clinical_vars$RespImp1_dummy <- ifelse(clinical_vars$PARS_21_35erImp_1==1,1,
                                       ifelse(clinical_vars$PARS_21_35erImp_1==0,-1,NA))
clinical_vars$forAnova_RespImp1 <- clinical_vars$PARS_21_35erImp_1
clinical_vars[which(is.na(clinical_vars$forAnova_RespImp1)),"forAnova_RespImp1"] <- 0
clinical_vars$forAnova_RespImp1_Inv <- ifelse(clinical_vars$PARS_21_35erImp_1==1,0,1)
clinical_vars[which(is.na(clinical_vars$forAnova_RespImp1_Inv)),"forAnova_RespImp1_Inv"] <- 0

clinical_vars$Gender <- as.numeric(clinical_vars$Gender)
clinical_vars$AGE_Pre <- as.numeric(clinical_vars$AGE_Pre)
clinical_vars$PARS_01_Total_Score <- as.numeric(clinical_vars$PARS_01_Total_Score)

####### GENERATE BEHAV VARS FOR FEAT BELOW #######

### PRETREATMENT ###

setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/subjects_by_cope/')
#----------------------------------------------------------------------------------------------------
# COPE 1: Approach - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope1pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 

loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 2: Avoid - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope2pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 

loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}


#----------------------------------------------------------------------------------------------------
# COPE 3: AmbigApproach - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope3pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope3.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope3.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 4: AmbigAvoid - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope4pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope4.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope4.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 5: AmbigAll - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope5pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 6: AmbigApproach - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope6pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope6.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# # old version - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope6pre_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope6.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 7: AmbigAvoid - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope7pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope7.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old version - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope7pre_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope7.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 8: AmbigAll - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope8pre_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope8.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope8pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope8.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}




#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

### POSTTREATMENT ###

setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/subjects_by_cope/')
#----------------------------------------------------------------------------------------------------
# COPE 1: Approach - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope1post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope1post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 2: Avoid - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope2post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope2post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}


#----------------------------------------------------------------------------------------------------
# COPE 3: AmbigApproach - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope3post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope3.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope3post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope3.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 4: AmbigAvoid - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope4post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope4.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope4post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope4.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}


#----------------------------------------------------------------------------------------------------
# COPE 5: AmbigAll - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope5post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old version - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope5post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 6: AmbigApproach - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope6post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope6.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old version - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope6post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope6.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}


#----------------------------------------------------------------------------------------------------
# COPE 7: AmbigAvoid - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope7post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope7.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old version - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope7post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope7.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 8: AmbigAll - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope8post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope8.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope8post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope8.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}




#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

### PREPOST ###
#----------------------------------------------------------------------------------------------------
# COPE 1: Approach - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope1pre_subs)==FALSE &
                        is.na(clinical_vars$cope1post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

# Therapy type
# CBT vs CCT
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) & 
                        clinical_vars$Anx==1,"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"CBT"]$CBT,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"CCT"]$CCT,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num")]
nrow(loop)

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}


#----------------------------------------------------------------------------------------------------
# COPE 2: Avoid - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope2pre_subs)==FALSE &
                        is.na(clinical_vars$cope2post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

# Therapy type
# CBT vs CCT
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) & 
                        clinical_vars$Anx==1,"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"CBT"]$CBT,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"CCT"]$CCT,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num")]
nrow(loop)

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

#----------------------------------------------------------------------------------------------------
# COPE 3: AmbigApproach - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope3pre_subs)==FALSE &
                        is.na(clinical_vars$cope3post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope3.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

# Therapy type
# CBT vs CCT
loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) &
                        !is.na(clinical_vars$cope3post_subs) & 
                        clinical_vars$Anx==1,"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope3.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"CBT"]$CBT,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"CCT"]$CCT,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) &
                        !is.na(clinical_vars$cope3post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num")]
nrow(loop)

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope3.gfeat/cope1.feat\n"))
}
#----------------------------------------------------------------------------------------------------
# COPE 4: AmbigAvoid - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope4pre_subs)==FALSE &
                        is.na(clinical_vars$cope4post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope4.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

# Therapy type
# CBT vs CCT
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) &
                        !is.na(clinical_vars$cope4post_subs) & 
                        clinical_vars$Anx==1,"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope4.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"CBT"]$CBT,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"CCT"]$CCT,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) &
                        !is.na(clinical_vars$cope4post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num")]
nrow(loop)

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope3.gfeat/cope1.feat\n"))
}

#----------------------------------------------------------------------------------------------------
# COPE 5: AmbigAll - Control

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope5pre_subs)==FALSE &
                        is.na(clinical_vars$cope5post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

# Therapy type
# CBT vs CCT
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) & 
                        clinical_vars$Anx==1,"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"CBT"]$CBT,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"CCT"]$CCT,"\n")
}
## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num")]
nrow(loop)

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}

#----------------------------------------------------------------------------------------------------
# COPE 6: AmbigApproach - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope6pre_subs)==FALSE &
                        is.na(clinical_vars$cope6post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope6.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old version - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope6pre_subs) &
                        !is.na(clinical_vars$cope6post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope6.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}


#----------------------------------------------------------------------------------------------------
# COPE 7: AmbigAvoid - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope7pre_subs)==FALSE &
                        is.na(clinical_vars$cope7post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope7.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
# old version - don't use
loop <- clinical_vars[!is.na(clinical_vars$cope7pre_subs) &
                        !is.na(clinical_vars$cope7post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","Response")]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope7.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Response"]$Response,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_Inv"]$Resp_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 8: AmbigAll - (Approach + Avoid)

# Anx vs NPD
loop <- clinical_vars[is.na(clinical_vars$cope8pre_subs)==FALSE &
                        is.na(clinical_vars$cope8post_subs)==FALSE & 
                        is.na(clinical_vars$Anx)==FALSE,"ID_Num"]
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope8.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\n")
}

## Clinical variables ##

# Response # 
loop <- clinical_vars[!is.na(clinical_vars$cope8pre_subs) &
                        !is.na(clinical_vars$cope8post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num")]
nrow(loop)

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope8.gfeat/cope1.feat\n"))
}

###################################################################################################
########################### IMPUTED VARIABLES #####################################################

# PRE
#----------------------------------------------------------------------------------------------------
# COPE 1

loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)

# Response T21

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}

# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_2"]$PARS_21_35erImp_2
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_3"]$PARS_21_35erImp_3
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_4"]$PARS_21_35erImp_4
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}


# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_5"]$PARS_21_35erImp_5
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}


for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}

#----------------------------------------------------------------------------------------------------
# COPE 2

loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}

# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_2"]$PARS_21_35erImp_2
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_3"]$PARS_21_35erImp_3
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_4"]$PARS_21_35erImp_4
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_5"]$PARS_21_35erImp_5
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 3

loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope3.gfeat/cope1.feat\n"))
}

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}


# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_2"]$PARS_21_35erImp_2
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_3"]$PARS_21_35erImp_3
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_4"]$PARS_21_35erImp_4
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_5"]$PARS_21_35erImp_5
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 4


loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope4.gfeat/cope1.feat\n"))
}

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}


# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_2"]$PARS_21_35erImp_2
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_3"]$PARS_21_35erImp_3
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_4"]$PARS_21_35erImp_4
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_5"]$PARS_21_35erImp_5
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 5

loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
  
  # Imputation 2
  for(subject in loop$ID_Num){
    var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_2"]$PARS_21_35erImp_2
    var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
    cat(var1,"\t",var2,"\n")
  }
  
  # Imputation 3
  for(subject in loop$ID_Num){
    var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_3"]$PARS_21_35erImp_3
    var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
    cat(var1,"\t",var2,"\n")
  }
  
  # Imputation 4
  for(subject in loop$ID_Num){
    var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_4"]$PARS_21_35erImp_4
    var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
    cat(var1,"\t",var2,"\n")
  }
  
  # Imputation 5
  for(subject in loop$ID_Num){
    var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_5"]$PARS_21_35erImp_5
    var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
    cat(var1,"\t",var2,"\n")
  }
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

# PREPOST
#----------------------------------------------------------------------------------------------------
# COPE 1

loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}


for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

#----------------------------------------------------------------------------------------------------
# COPE 2

loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$cope2post_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 3

loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) & 
                        !is.na(clinical_vars$cope3post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope3.gfeat/cope1.feat\n"))
}

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}


# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_2"]$PARS_21_35erImp_2
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_3"]$PARS_21_35erImp_3
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_4"]$PARS_21_35erImp_4
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_5"]$PARS_21_35erImp_5
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 4

loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) & 
                        !is.na(clinical_vars$cope4post_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope4.gfeat/cope1.feat\n"))
}

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}


# Imputation 2
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_2"]$PARS_21_35erImp_2
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 3
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_3"]$PARS_21_35erImp_3
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 4
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_4"]$PARS_21_35erImp_4
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

# Imputation 5
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_5"]$PARS_21_35erImp_5
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 5

loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_1),"ID_Num"]
nrow(loop)

# Response T21

# Imputation 1
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  var2 <- ifelse(var1==1,0,ifelse(var1==0,1,NA))
  cat(var1,"\t",var2,"\n")
}
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

### INTERACTION TERMS ###
### PRETREATMENT ###

# _____Therapy group (CBT vs CCT) X Response_____

#----------------------------------------------------------------------------------------------------
# COPE 1
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$RespImp1_dummy),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}

# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"RespImp1_dummy"]$RespImp1_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}

# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 2
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$RespImp1_dummy),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}
# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"RespImp1_dummy"]$RespImp1_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}
# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}

# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 3
loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) & 
                        !is.na(clinical_vars$RespImp1_dummy),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope3.gfeat/cope1.feat\n"))
}
# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"RespImp1_dummy"]$RespImp1_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}
# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) & 
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope3.gfeat/cope1.feat\n"))
}

# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}
#----------------------------------------------------------------------------------------------------
# COPE 4
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) & 
                        !is.na(clinical_vars$RespImp1_dummy),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope4.gfeat/cope1.feat\n"))
}
# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"RespImp1_dummy"]$RespImp1_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}
# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) & 
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope4.gfeat/cope1.feat\n"))
}

# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}

# COPE 5
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) & 
                        !is.na(clinical_vars$RespImp1_dummy),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}
# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"RespImp1_dummy"]$RespImp1_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) & 
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}

# Just for imputation 1 for right now
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
## PREPOST ##
# COPE 1
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","PARS_21_35erImp_1")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 2
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","PARS_21_35erImp_1")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}
# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}

# COPE 3
loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) &
                        !is.na(clinical_vars$cope3post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$Response),c("ID_Num","PARS_21_35erImp_1")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope3.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

# COPE 4
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) &
                        !is.na(clinical_vars$cope4post_subs) &
                        clinical_vars$Anx==1 &
                        !is.na(clinical_vars$PARS_21_35erImp_1),c("ID_Num","PARS_21_35erImp_1")]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope4.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_1"]$PARS_21_35erImp_1
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}
# COPE 5

# _____Anx X Response_____

loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) & 
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$Anx_Inv),"ID_Num"]
nrow(loop)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  cat(var1,"\t",var2,"\t",var3,"\n")
}

### INTERACTION TERMS ###
### PRETREATMENT ###

# _____Therapy (CBT vs. CCT) X Response_____
#----------------------------------------------------------------------------------------------------
# COPE 1
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 2
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 3
loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope3.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 4
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]

# Without subject 2387
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0) &
                        clinical_vars$ID_Num != 2387,"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope4.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 5
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 7
loop <- clinical_vars[!is.na(clinical_vars$cope7pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope7.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}
#----------------------------------------------------------------------------------------------------
# COPE 8
loop <- clinical_vars[!is.na(clinical_vars$cope8pre_subs) & 
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope8.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

### PREPOST ###

# _____Therapy (CBT vs. CCT) X Response_____
#----------------------------------------------------------------------------------------------------
# COPE 1
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

# check if it works within CBT only
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        clinical_vars$Therapy=="CBT" &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 2
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
# minus subject 2004
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0) &
                        clinical_vars$ID_Num != 2004,"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}
# check if it works within CBT only
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        clinical_vars$Therapy=="CBT" &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 3
loop <- clinical_vars[!is.na(clinical_vars$cope3pre_subs) &
                        !is.na(clinical_vars$cope3post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope3.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 4
loop <- clinical_vars[!is.na(clinical_vars$cope4pre_subs) &
                        !is.na(clinical_vars$cope4post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope4.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 5
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
# COPE 8
loop <- clinical_vars[!is.na(clinical_vars$cope8pre_subs) &
                        !is.na(clinical_vars$cope8post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope8.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

### POSTTREATMENT ###
# Just for the ones with significant prepost

# _____Therapy (CBT vs. CCT) X Response_____
#----------------------------------------------------------------------------------------------------
# COPE 1
loop <- clinical_vars[!is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

# COPE 2
loop <- clinical_vars[!is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),"ID_Num"]
nrow(loop)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\n")
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
##### SENSITIVITY ANALYSES
# just for contrasts of interest

# Response adjusting for baseline PARS

# PRETREATMENT
# COPE 1
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\n")
}

# COPE 2
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\n")
}

# COPE 5
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\n")
}

# PREPOST
# COPE 1
# Response
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score,"\n")
}

# Anx X Response ANOVA
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_01_Total_Score),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  var4 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\n")
}

# COPE 2
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score,"\n")
}

# Anx X Response ANOVA
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_01_Total_Score),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  var4 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\n")
}

# COPE 5
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score,"\n")
}

# Anx X Response ANOVA
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) & 
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_01_Total_Score),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  var4 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\n")
}

#----------------------------------------------------------------------------------------------------

# Adjusting for age + gender

# PRETREATMENT
# COPE 1
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Group (ANX vs. NPD)
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
      )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}

# COPE 2
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Group (ANX vs. NPD)
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope2.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope1.gfeat/cope2.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}

# COPE 5
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Group (ANX vs. NPD)
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}

# PREPOST
# COPE 1
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# Anx X Response ANOVA
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) & 
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_01_Total_Score),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  var4 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var5 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\t",var5,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope1.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}

# Add in baseline PARS for completeness
loop <- clinical_vars[!is.na(clinical_vars$cope1pre_subs) &
                        !is.na(clinical_vars$cope1post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender","PARS_01_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  var5 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\t",var5,"\n")
}

# COPE 2
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# Anx X Response ANOVA
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) & 
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_01_Total_Score),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  var4 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var5 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\t",var5,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope2.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}

# Add in baseline PARS for completeness
loop <- clinical_vars[!is.na(clinical_vars$cope2pre_subs) &
                        !is.na(clinical_vars$cope2post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender","PARS_01_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  var5 <- loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\t",var5,"\n")
}

# COPE 5
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# Anx X Response ANOVA
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) & 
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_01_Total_Score),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1"]$forAnova_RespImp1
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv
  var4 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var5 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\t",var5,"\n")
}

# _____Therapy (CBT vs. CCT) X Response_____
loop <- clinical_vars[!is.na(clinical_vars$cope5pre_subs) &
                        !is.na(clinical_vars$cope5post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope5.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}
