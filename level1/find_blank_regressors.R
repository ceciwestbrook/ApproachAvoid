library(gdata)
library(tidyverse)
setwd('/ix/cladouceur/westbrook-data/Scripts')
allsubs <- read.csv('imaging_subjects.txt')
setwd('/ix/cladouceur/westbrook-data/results')
write.table("ID_Num Regressor session run usability",'usable_regressors.txt',col.names=FALSE,row.names = FALSE,quote=FALSE)

# for debugging
#allsubs = data.frame(2130)

for(subject in allsubs[,]){
  subDir <- paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',subject,sep="")
  for(session in c('pretreatment','posttreatment')){
    for(run in c(1,2)){
      for(filename in c('AmbigApproach2sResponse','AmbigApproach2sRestOfTrial',
                      'AmbigAvoid2sResponse','AmbigAvoid2sRestOfTrial','Approach2sResponse',
                      'Approach2sRestOfTrial','Avoid2sResponse','Avoid2sRestOfTrial',
                      'NoInterest','XResponse2sResponse','XResponse2sRestOfTrial')){
        possibleError <- tryCatch(assign(filename,read.csv(
          paste(subDir,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',filename,'_run-0',run,'.txt',
                sep=""),sep=" ",header=FALSE)),error = function(x) x)
        if(get(filename)[1,1] != 0){
          line = paste(subject,' ',filename,' ',session,' run',run,' usable',sep="")
        } else {
          line = paste(subject,' ',filename,' ',session,' run',run,' BLANK',sep="")
        }
        write.table(line,'usable_regressors.txt',col.names=FALSE,row.names = FALSE,quote=FALSE,append=TRUE)
        assign(filename,matrix(1,1))
        }
      }
  }
  }
