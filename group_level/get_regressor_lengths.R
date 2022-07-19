# library(gdata)
# library(tidyverse)
# library(stringr)
# setwd('/ix/cladouceur/westbrook-data/Scripts')
# allsubs <- read.csv('imaging_subjects.txt')
# setwd('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/')
# 
# filenames = Sys.glob('*/*/func/*')
# filedata = lapply(filenames, function(x){read.table(file=x,header=T,sep="\t")})
# filedata[[384]][filedata[[384]]$event=="ApproachGainResponse","duration"]
# concatdata <- Reduce(function(x,y) {rbind(x,y)}, filedata)
# 
# unique(concatdata$event)
# 
# RespAll <- data.frame("Resp" = concatdata[concatdata$event %in% 
#                                              c("XRespResponse",
#                                                "AvoidLossResponse",
#                                                "AvoidNoLossResponse",
#                                                "ApproachNoGainResponse",
#                                                "ApproachGainResponse",
#                                                "AmbigApproachGainResponse",
#                                                "AmbigAvoidNoLossResponse",
#                                                "AmbigApproachLossResponse"),
#                                                "duration"],
#                   "RespPost"  = concatdata[concatdata$event %in% c("XRespPostResponse",
#                                                                    "AvoidLossPostResponse",
#                                                                    "AvoidNoLossPostResponse",
#                                                                    "ApproachNoGainPostResponse",
#                                                                    "ApproachGainPostResponse",
#                                                                    "AmbigApproachGainPostResponse",
#                                                                    "AmbigAvoidNoLossPostResponse",
#                                                                    "AmbigApproachLossPostResponse"),
#                                            "duration"])
# RespAll$sum <- rowSums(RespAll[,c("Resp","RespPost")])
# range(RespAll$sum)
# 
# FeedbackAll <- data.frame("Feedback" = concatdata[concatdata$event %in% 
#                                             c("XRespFeedback",
#                                               "AvoidLossFeedback",
#                                               "AvoidNoLossFeedback",
#                                               "ApproachNoGainFeedback",
#                                               "ApproachGainFeedback",
#                                               "AmbigApproachGainFeedback",
#                                               "AmbigAvoidNoLossFeedback",
#                                               "AmbigApproachLossFeedback"),
#                                           "duration"])
# range(FeedbackAll$Feedback)

library(gdata)
library(tidyverse)
setwd('/ix/cladouceur/westbrook-data/Scripts')
allsubs <- read.csv('imaging_subjects.txt')
setwd('level1/behav_data/')

# for debugging
#allsubs = data.frame(2157)

filedata <- list()
for(subject in allsubs[,]){
  
  # make directories for later
  subDir <- paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',subject,sep="")
  if (!file.exists(subDir)) {
    dir.create(subDir)
  }
  
  for(sesnum in c('02','22')) {
    for(run in c('a','b')) {
      possibleError <- tryCatch(read_csv2(paste(subject,'_',sesnum,run,'.dat',sep=""),skip=1,col_name=FALSE),
                                error = function(x) x)
      
      session <- ifelse(sesnum=='02',"pretreatment",
                        ifelse(sesnum=='22',"posttreatment",""))
      
      runnum <- ifelse(run=='a',"01",
                       ifelse(run=='b',"02",""))
      
      # check to make sure the file exists, otherwise break the loop
      if(!inherits(possibleError, "error")) {
        
        # make directories for later
        sesDir <- paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',subject,'/ses-',session,sep="")
        if (!file.exists(sesDir)) {
          dir.create(sesDir)
          dir.create(paste(sesDir,'/func',sep=""))
        }
        
        # read in the dat files and convert them into useable tibbles
        d <- read_csv2(paste(subject,'_',sesnum,run,'.dat',sep=""),skip=1,col_name=FALSE)
        d2 <- as_tibble(d[1:nrow(d)-1,])
        d2 <- mutate_if(d2,
                        is.character,
                        str_replace_all, "  "," ")
        curr_sub <- separate(d2,col=X1,into = c("Trial_type","Trial_begin","Num_avoid_resp","Num_approach_resp",
                                                "Last_response_T","Outcome_T","GainLoss_Magnitude","Cumulative_points"),sep=" ")
        curr_sub <- as_tibble(sapply(curr_sub,as.numeric))
        curr_sub$next_trial_start <- lead(curr_sub$Trial_begin)
        curr_sub$last_trial_points <- lag(curr_sub$Cumulative_points)
        curr_sub$GainLoss <- ifelse(curr_sub$Cumulative_points > curr_sub$last_trial_points,"Gain",
                                    ifelse(curr_sub$Cumulative_points < curr_sub$last_trial_points,"Loss",
                                           ifelse(curr_sub$Cumulative_points == curr_sub$last_trial_points,"NoChange",NA)))
        curr_sub[1,"GainLoss"] <- ifelse(curr_sub[1,"Cumulative_points"] > 0, "Gain",
                                         ifelse(curr_sub[1,"Cumulative_points"] < 0, "Loss",
                                                ifelse(curr_sub[1,"Cumulative_points"] == 0, "NoChange",NA)))
        curr_sub[nrow(curr_sub),"next_trial_start"] <- 384.10
        curr_sub$Response <- ifelse(curr_sub$Last_response_T > 0, (curr_sub$Last_response_T - curr_sub$Trial_begin),(curr_sub$Outcome_T - curr_sub$Trial_begin))
        curr_sub$PostResponse <- ifelse(curr_sub$Last_response_T > 0,(curr_sub$Outcome_T - curr_sub$Last_response_T),0)
        curr_sub$Feedback <- curr_sub$Outcome_T - curr_sub$Last_response_T
        curr_sub$Rest <- curr_sub$next_trial_start - curr_sub$Outcome_T
        #ResponseTrials <- curr_sub[curr_sub$Last_response_T>0,]
        #NoResponseTrials <- curr_sub[curr_sub$Last_response_T==0,]
        filedata[[which(allsubs$X2002==subject)]] <- curr_sub
      }
    }
  }
}
concatdata <- Reduce(function(x,y) {rbind(x,y)}, filedata)
concatdata$TotalResp <- rowSums(concatdata[,c("Response","PostResponse")])
range(concatdata[concatdata$Last_response_T>0,"TotalResp"])
range(concatdata[concatdata$Last_response_T>0,"Response"])
range(concatdata[concatdata$Last_response_T>0,"Feedback"])
range(concatdata[concatdata$Last_response_T>0,"Rest"])
concatdata[concatdata$Last_response_T>0 & concatdata$Rest>20,]
