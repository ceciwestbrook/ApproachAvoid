library(dplyr)
setwd('/ix/cladouceur/westbrook-data/Scripts')
allsubs <- read.table('imaging_subjects.txt')
setwd('/ix/cladouceur/westbrook-data/Scripts/level1')
prerun1 <- t(read.table('subjects_prerun1.txt'))
prerun2 <- t(read.table('subjects_prerun2.txt'))
postrun1 <- t(read.table('subjects_postrun1.txt'))
postrun2 <- t(read.table('subjects_postrun2.txt'))
setwd('/ix/cladouceur/westbrook-data/Scripts/level2')
regressors <- read.table('usable_regressors.txt',header=TRUE)

cope1 = data.frame("ID_Num"=numeric())
cope2 = data.frame("ID_Num"=numeric())
cope3 = data.frame("ID_Num"=numeric())
cope4 = data.frame("ID_Num"=numeric())
cope5 = data.frame("ID_Num"=numeric())
cope6 = data.frame("ID_Num"=numeric())
cope7 = data.frame("ID_Num"=numeric())
cope8 = data.frame("ID_Num"=numeric())

for(session in c("pre","post")){
  for(run in c("run1","run2")){
    assign("curfile",get(paste0(session,run)))
    for(subject in curfile){
      # cope1 - Approach (Approach2SResponse) - Control (XResponse2sResponse)
      if( regressors[regressors$ID_Num==subject &
                      regressors$session==paste0(session,"treatment") &
                      regressors$run==run &
                      regressors$Regressor=="Approach2sResponse", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="XResponse2sResponse", "usability"]=="usable"){
        cope1 <- rbind(cope1,subject)
        #write.table(cope1,paste0("cope1",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      }
      assign(paste0("cope1",session,run),cope1)
      
  # cope2 - Avoid - Control
      if( regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="Avoid2sResponse", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="XResponse2sResponse", "usability"]=="usable"){
        cope2 <- rbind(cope2,subject)
      }
      assign(paste0("cope2",session,run),cope2)
      #write.table(cope2,paste0("cope2",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      
  # cope3 - AmbigApproach - Control
        if( regressors[regressors$ID_Num==subject &
                       regressors$session==paste0(session,"treatment") &
                       regressors$run==run &
                       regressors$Regressor=="AmbigApproach2sResponse", "usability"]=="usable" &
            regressors[regressors$ID_Num==subject &
                       regressors$session==paste0(session,"treatment") &
                       regressors$run==run &
                       regressors$Regressor=="XResponse2sResponse", "usability"]=="usable"){
          cope3 <- rbind(cope3,subject)
        } 
      assign(paste0("cope3",session,run),cope3)
      #write.table(cope3,paste0("cope3",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      
  # cope4 - AmbigAvoid - Control
          if( regressors[regressors$ID_Num==subject &
                         regressors$session==paste0(session,"treatment") &
                         regressors$run==run &
                         regressors$Regressor=="AmbigAvoid2sResponse", "usability"]=="usable" &
              regressors[regressors$ID_Num==subject &
                         regressors$session==paste0(session,"treatment") &
                         regressors$run==run &
                         regressors$Regressor=="XResponse2sResponse", "usability"]=="usable"){
            cope4 <- rbind(cope4,subject)
          }
      assign(paste0("cope4",session,run),cope4)
      #write.table(cope4,paste0("cope4",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      
  # cope5 - AmbigAll - Control
            if( regressors[regressors$ID_Num==subject &
                           regressors$session==paste0(session,"treatment") &
                           regressors$run==run &
                           regressors$Regressor=="AmbigApproach2sResponse", "usability"]=="usable" &
                regressors[regressors$ID_Num==subject &
                           regressors$session==paste0(session,"treatment") &
                           regressors$run==run &
                           regressors$Regressor=="AmbigAvoid2sResponse", "usability"]=="usable" &
                regressors[regressors$ID_Num==subject &
                           regressors$session==paste0(session,"treatment") &
                           regressors$run==run &
                           regressors$Regressor=="XResponse2sResponse", "usability"]=="usable"){
              cope5 <- rbind(cope5,subject)
            }   
      assign(paste0("cope5",session,run),cope5)
      #write.table(cope5,paste0("cope5",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      
  # cope6 - AmbigApproach - (Avoid + Approach)
              if( regressors[regressors$ID_Num==subject &
                             regressors$session==paste0(session,"treatment") &
                             regressors$run==run &
                             regressors$Regressor=="AmbigApproach2sResponse", "usability"]=="usable" &
                  regressors[regressors$ID_Num==subject &
                             regressors$session==paste0(session,"treatment") &
                             regressors$run==run &
                             regressors$Regressor=="Avoid2sResponse", "usability"]=="usable" &
                  regressors[regressors$ID_Num==subject &
                             regressors$session==paste0(session,"treatment") &
                             regressors$run==run &
                             regressors$Regressor=="Approach2sResponse", "usability"]=="usable"){
                cope6 <- rbind(cope6,subject)
              }    
      assign(paste0("cope6",session,run),cope6)
      #write.table(cope6,paste0("cope6",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      
  # cope7 - AmbigAvoid - (Avoid + Approach)
                if( regressors[regressors$ID_Num==subject &
                               regressors$session==paste0(session,"treatment") &
                               regressors$run==run &
                               regressors$Regressor=="AmbigAvoid2sResponse", "usability"]=="usable" &
                    regressors[regressors$ID_Num==subject &
                               regressors$session==paste0(session,"treatment") &
                               regressors$run==run &
                               regressors$Regressor=="Avoid2sResponse", "usability"]=="usable" &
                    regressors[regressors$ID_Num==subject &
                               regressors$session==paste0(session,"treatment") &
                               regressors$run==run &
                               regressors$Regressor=="Approach2sResponse", "usability"]=="usable"){
                  cope7 <- rbind(cope7,subject)
                }      
      assign(paste0("cope7",session,run),cope7)
      #write.table(cope7,paste0("cope7",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      
  # cope8 - AmbigAll - (Avoid + Approach)
                  if( regressors[regressors$ID_Num==subject &
                                 regressors$session==paste0(session,"treatment") &
                                 regressors$run==run &
                                 regressors$Regressor=="AmbigApproach2sResponse", "usability"]=="usable" &
                      regressors[regressors$ID_Num==subject &
                                 regressors$session==paste0(session,"treatment") &
                                 regressors$run==run &
                                 regressors$Regressor=="AmbigAvoid2sResponse", "usability"]=="usable" &
                      regressors[regressors$ID_Num==subject &
                                 regressors$session==paste0(session,"treatment") &
                                 regressors$run==run &
                                 regressors$Regressor=="Avoid2sResponse", "usability"]=="usable" &
                      regressors[regressors$ID_Num==subject &
                                 regressors$session==paste0(session,"treatment") &
                                 regressors$run==run &
                                 regressors$Regressor=="Approach2sResponse", "usability"]=="usable"){
                    cope8 <- rbind(cope8,subject)
                  }
      assign(paste0("cope8",session,run),cope8)
      #write.table(cope8,paste0("cope8",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
    }
    cope1 = data.frame("ID_Num"=numeric())
    cope2 = data.frame("ID_Num"=numeric())
    cope3 = data.frame("ID_Num"=numeric())
    cope4 = data.frame("ID_Num"=numeric())
    cope5 = data.frame("ID_Num"=numeric())
    cope6 = data.frame("ID_Num"=numeric())
    cope7 = data.frame("ID_Num"=numeric())
    cope8 = data.frame("ID_Num"=numeric())
  }
}

run1run2 = data.frame("ID_Num"=numeric())
run1run1 = data.frame("ID_Num"=numeric())
run2run2 = data.frame("ID_Num"=numeric())

for(cope in c("cope1","cope2","cope3","cope4","cope5","cope6","cope7","cope8")){
  for(session in c("pre","post")){
    for(subject in allsubs[,]){
      if(subject %in% get(paste0(cope,session,"run1"))[,1] & subject %in% get(paste0(cope,session,"run2"))[,1]){
        run1run2 <- rbind(run1run2,subject)
        assign(paste0(cope,session,"run1run2"),run1run2)
      } else if(subject %in% get(paste0(cope,session,"run1"))[,1] & ! subject %in% get(paste0(cope,session,"run2"))[,1]) {
        run1run1 <- rbind(run1run1,subject)
        assign(paste0(cope,session,"run1run1"),run1run1)
        } else if(! subject %in% get(paste0(cope,session,"run1"))[,1] & subject %in% get(paste0(cope,session,"run2"))[,1]) {
        run2run2 <- rbind(run2run2,subject)
        assign(paste0(cope,session,"run2run2"),run2run2)
        }
      #write.table(get(paste0(cope,session,"run1run2")),paste0(cope,session,"run1run2.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      #write.table(get(paste0(cope,session,"run1run1")),paste0(cope,session,"run1run1.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      #write.table(get(paste0(cope,session,"run2run2")),paste0(cope,session,"run2run2.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
    }

    run1run2 = data.frame("ID_Num"=numeric())
    run1run1 = data.frame("ID_Num"=numeric())
    run2run2 = data.frame("ID_Num"=numeric())
    }
  }
